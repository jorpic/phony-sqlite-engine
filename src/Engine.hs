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
import Control.Monad (when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V

type CursorId = Int
type TableId = String

type Addr = Int
data Val = StrVal String | IntVal Integer | RealVal Double
  deriving (Eq, Show)
type Row = Vector (Maybe Val)
type Table = Vector Row

newtype Db = Db
  { _dbData :: Map TableId Table
  }
  deriving Show

data Cursor = Cursor TableId Int
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

makeLenses ''Db
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
  | Goto {goto :: Addr}
  | Halt
  | Real  {reg :: Int, rVal :: Double}
  | Integer  {reg :: Int, iVal :: Integer}
  | String8 {reg :: Int, sVal :: String}
  | ResultRow {reg :: Int, len :: Int}
    -- ^ The registers P1 through P1+P2-1 contain a single row of results.
  | OpenRead {cursor :: CursorId, table :: TableId}
  | Rewind {cursor :: CursorId, goto :: Addr}
    -- ^ Rewind cursor P1. Jump to P2 if the table is empty.
  | Column {cursor :: CursorId, column :: Int, reg :: Int, def :: Maybe Val}
    -- ^ Read P2-th column from cursor P1, store result in P3, use P4 as
    -- default.
  | Next {cursor :: CursorId, goto :: Addr} -- _uniquenessHint :: Bool }
    -- Advance P1, jump to P2 if not empty, P3 is a hint

  deriving (Eq, Show)

type Engine a = StateT EngineState (Except EngineErr) a

evalCmd :: Cmd -> Engine ()
evalCmd = \case
  Init{..} -> when (goto > 0) $ jumpTo ?= goto
  Goto{..} -> jumpTo ?= goto
  Halt -> lift $ throwE "Halt!"
  Real{..}    -> registers %= (at reg ?~ RealVal rVal)
  Integer{..} -> registers %= (at reg ?~ IntVal iVal)
  String8{..} -> registers %= (at reg ?~ StrVal sVal)

  ResultRow{..} -> do
    regs <- use registers
    let row = V.fromList [regs ^. at r | r <- [reg .. reg+len-1]]
    yield .= [row]

  OpenRead{..} -> do
    void $ throwIfNothing (use $ db . dbData . at table)
      $ "unknown table" ++ show table
    throwIfJust (use $ openCursors . at cursor)
      $ "cursor " ++ show cursor ++ " is already used"
    openCursors %= (at cursor ?~ Cursor table 0)

  Rewind{..} -> do
    Cursor table _ <- getCursor cursor
    -- rewind cursor
    openCursors %= (at cursor ?~ Cursor table 0)
    -- check if table is empty
    row <- rowAt cursor
    when (row == Nothing) $ jumpTo ?= goto

  Column{..} -> do
    row <- throwIfNothing (rowAt cursor)
      $ "cursor overflow"
    let val = fromMaybe def $ row !? column
    registers %= (at reg .~ val)

  Next{..} -> do
    Cursor table offset <- getCursor cursor
    openCursors %= (at cursor ?~ Cursor table (offset+1))
    row <- rowAt cursor
    when (row /= Nothing) $ jumpTo ?= goto

  _ -> lift $ throwE "Not implemented yet"


throwIfJust :: Engine (Maybe a) -> EngineErr -> Engine ()
throwIfJust f err = f >>= maybe (pure ()) (\_ -> lift $ throwE err)

throwIfNothing :: Engine (Maybe a) -> EngineErr -> Engine a
throwIfNothing f err = f >>= maybe (lift $ throwE err) pure

getCursor :: CursorId -> Engine Cursor
getCursor c
  = throwIfNothing (use $ openCursors . at c)
    $ "invalid cursor id" ++ show c

rowAt :: CursorId -> Engine (Maybe Row)
rowAt c = do
  Cursor table offset <- getCursor c
  tableData <- throwIfNothing (use $ db . dbData . at table)
    $ "impossible! cursor for nonexistent table"
  pure $ tableData !? offset


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
          evalCmd cmd -- FIXME: catch and add context to error
          EngineState{_jumpTo, _yield} <- get
          -- update instruction pointer
          ip .= fromMaybe (_ip + 1) _jumpTo
          jumpTo .= Nothing
          -- loop if no yield
          case _yield of
            [] -> loop
            _ -> yield .= [] >> return _yield

runWithDb :: Db -> Engine a -> Either EngineErr a
runWithDb db' f = runExcept $ evalStateT f $ defaultEngineState { _db = db' }


-- NB. this should not be used to get huge results
runQuery :: Db -> Query -> Either EngineErr [Row]
runQuery db' q = runWithDb db' loop
  where
    loop = evalToYield q >>= \case
      [] -> pure []
      xs -> (xs ++) <$> loop -- FIXME: memory collects here
