{-# LANGUAGE Strict #-} -- or just StrictData
{-# LANGUAGE TemplateHaskell #-}

module Engine
    ( Cmd(..)
    , evalCmd
    , evalToYield
    , runQuery
    , Db(..)
    , EngineState(..)
    , defaultEngineState
    ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!?))

-- FIXME: use phantom types instead? Id a = Id Int
newtype CursorId = CursorId Int deriving (Eq, Ord, Show)
newtype TableId = TableId Int deriving (Eq, Ord, Show)

type Addr = Int
type Val = String
type Row = [Val]
newtype Db = Db (Map TableId [Row])
  deriving Show

data Cursor = TableCursor TableId [Row]
  deriving (Eq, Ord, Show)

type EngineErr = String

data EngineState = EngineState
  { _ip :: Int -- program counter
  , _jumpTo :: Maybe Int
  , _db :: Db
  , _registers :: Map Int Val
  , _openCursors :: Map CursorId Cursor
  , _yield :: [Row]
  }
  deriving Show

makeLenses ''EngineState

defaultEngineState :: EngineState
defaultEngineState = EngineState
  { _ip = 0
  , _jumpTo = Nothing
  , _db = Db Map.empty
  , _registers = Map.empty
  , _openCursors = Map.empty
  , _yield = []
  }

-- check yield
-- check ip
-- fetch new command


data Cmd
  = Init { goto :: Addr }
    -- ^ If P2 is not zero, jump to instruction P2.
  | String8 { reg :: Int, val :: String }
  | OpenRead { cursor :: CursorId, table :: TableId }
  | Rewind { cursor :: CursorId, goto :: Addr }
  | Goto { goto :: Addr }
  | Halt
  deriving (Eq, Show)

type Engine a = StateT EngineState (Except EngineErr) a

evalCmd :: Cmd -> Engine ()
evalCmd = \case
  Init{..} -> when (goto > 0) $ jumpTo .= Just goto
  Goto{..} -> jumpTo .= Just goto
  String8{..} -> registers %= Map.insert reg val
  _ -> lift $ throwE "Not implemented yet"


type Query = Vector Cmd

evalToYield :: Query -> Engine [Row]
evalToYield query = loop
  where
    loop = do
      EngineState{_ip} <- get

      case query !? _ip of
        Nothing -> lift $ throwE $ "Invalid location: " ++ show _ip
        Just Halt -> return []
        Just cmd -> do
          evalCmd cmd
          EngineState{_jumpTo, _yield} <- get

          -- update instruction pointer
          let ip' = fromMaybe (_ip + 1) _jumpTo
          jumpTo .= Nothing
          ip .= ip'

          case _yield of
            [] -> loop
            _ -> return _yield

runQuery :: Db -> Query -> Either EngineErr [Row]
runQuery db' q
  = runExcept $ evalStateT loop $ defaultEngineState { _db = db' }
  where
    -- FIXME: use pipes / conduits for efficiency
    loop = evalToYield q >>= \case
      [] -> pure []
      xs -> (xs ++) <$> loop
