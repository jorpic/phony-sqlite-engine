module Engine
    ( Cmd(..)
    , evalCmd
    , evalToYield
    , runQuery
    , Db(..)
    , Val(..)
    , CmpOp(..)
    ) where

import Control.Lens
import Control.Monad (when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!?))
import Data.Vector qualified as V

import Engine.Types
import Engine.State
import Engine.Cmd

type Query = Vector Cmd

evalCmd :: Cmd -> Engine ()
evalCmd = \case
  Init{..} -> when (goto > 0) $ jumpTo ?= goto
  Goto{..} -> jumpTo ?= goto
  Halt -> lift $ throwE "Halt!"
  Real{..}    -> registers %= (at reg ?~ RealVal rVal)
  Integer{..} -> registers %= (at reg ?~ IntVal iVal)
  String8{..} -> registers %= (at reg ?~ StrVal sVal)

  Cmp{..} -> do
    a <- use $ registers . at reg
    b <- use $ registers . at reg2
    let res = compare a b
    let jump = case cmpOp of
          Eq -> res == EQ
          Ne -> res /= EQ
          Lt -> res == LT
          Gt -> res == GT
          Le -> res == LT || res == EQ
          Ge -> res == GT || res == EQ
    case (cmpFlag, a, b) of
      (          _,     Just  _, Just  _) -> when jump $ jumpTo ?= goto
      (Just NullEq,     Nothing, Nothing) -> jumpTo ?= goto
      (Just JumpIfNull,       _,       _) -> jumpTo ?= goto
      _                                   -> pure ()
    cmpResult ?= res

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
