{-# LANGUAGE TemplateHaskell #-}
module Engine.State where

import Control.Lens
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector ((!?))
import Engine.Types

type EngineErr = String

data EngineState = EngineState
  { _ip :: Int -- instruction (cmd) pointer
  , _jumpTo :: Maybe Int
  , _db :: Db
  , _registers :: Map Int Val
  , _openCursors :: Map CursorId Cursor
  , _yield :: [Row]
  , _cmpResult :: Maybe Ordering -- used by OP_Jump
  , _halted :: Bool
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
  , _cmpResult = Nothing
  , _halted = False
  }

type Engine a = StateT EngineState (Except EngineErr) a


-- Utility functions

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
