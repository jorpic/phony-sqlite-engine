{-# LANGUAGE Strict #-} -- or just StrictData
{-# LANGUAGE TemplateHaskell #-}

module Engine
    ( Cmd(..)
    , evalCmd
    , evalToYield
    , runQuery
    , Db(..)
    , Val(..)
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
data Val = StrVal String | IntVal Integer | RealVal Double
  deriving (Eq, Show)
type Row = [Maybe Val]
newtype Db = Db (Map TableId [Row])
  deriving Show

data Cursor = TableCursor TableId [Row]
  deriving (Eq, Show)

type EngineErr = String

data EngineState = EngineState
  { _ip :: Int -- instruction (cmd) pointer
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

data Cmd
  = Init { goto :: Addr }
    -- ^ If P2 is not zero, jump to instruction P2.
  | Goto { goto :: Addr }
  | Halt
  | Real  { reg :: Int, rVal :: Double }
  | Integer  { reg :: Int, iVal :: Integer }
  | String8 { reg :: Int, sVal :: String }
  | ResultRow { reg :: Int, len :: Int }
    -- ^ The registers P1 through P1+P2-1 contain a single row of results.
  | OpenRead { cursor :: CursorId, table :: TableId }
  | Rewind { cursor :: CursorId, goto :: Addr }
  deriving (Eq, Show)

type Engine a = StateT EngineState (Except EngineErr) a

evalCmd :: Cmd -> Engine ()
evalCmd = \case
  Init{..} -> when (goto > 0) $ jumpTo .= Just goto
  Goto{..} -> jumpTo .= Just goto
  Halt -> lift $ throwE "Halt!"
  Real{..}    -> registers %= Map.insert reg (RealVal rVal)
  Integer{..} -> registers %= Map.insert reg (IntVal iVal)
  String8{..} -> registers %= Map.insert reg (StrVal sVal)
  ResultRow{..} -> do
    regs <- use registers
    let row = map (`Map.lookup` regs) [reg .. reg+len-1]
    yield .= [row]
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
          ip .= fromMaybe (_ip + 1) _jumpTo
          jumpTo .= Nothing
          -- loop if no yield
          case _yield of
            [] -> loop
            _ -> return _yield

runWithDb :: Db -> Engine a -> Either EngineErr a
runWithDb db' f = runExcept $ evalStateT f $ defaultEngineState { _db = db' }


-- NB. this should not be used to get huge results
runQuery :: Db -> Query -> Either EngineErr [Row]
runQuery db' q = runWithDb db' loop
  where
    loop = evalToYield q >>= \case
      [] -> pure []
      xs -> (xs ++) <$> loop -- FIXME: memory collects here
