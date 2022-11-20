{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
module Engine.Cmd where

import Control.Lens
import Control.Monad (when, void)
import Data.Maybe (fromMaybe)
import Data.Vector ((!?))
import Data.Vector qualified as V

import Engine.Types
import Engine.State


class Cmd c where
  evalCmd :: c -> Engine ()

data AnyCmd where
  Cmd :: Cmd c => c -> AnyCmd

instance Cmd AnyCmd where
  evalCmd (Cmd c) = evalCmd c

data CmpOp = Eq | Ne | Lt | Gt | Le | Ge
  deriving (Eq, Show)

data Init = Init {goto :: Addr}
-- If P2 is not zero, jump to instruction P2.
instance Cmd Init where
  evalCmd Init{..} = when (goto > 0) $ jumpTo ?= goto

data Goto = Goto {goto :: Addr}
instance Cmd Goto where
  evalCmd Goto{..} = jumpTo ?= goto

data Halt = Halt
instance Cmd Halt where
  evalCmd Halt = halted .= True

data SetVal
  = Real    {reg :: Int, rVal :: Double}
  | Integer {reg :: Int, iVal :: Integer}
  | String8 {reg :: Int, sVal :: String}

instance Cmd SetVal where
  evalCmd = \case
    Real{..}    -> registers %= (at reg ?~ RealVal rVal)
    Integer{..} -> registers %= (at reg ?~ IntVal iVal)
    String8{..} -> registers %= (at reg ?~ StrVal sVal)

data CmpFlag
  = NullEq
  -- ^ This flag is used only for equality operators (Eq and Ne).
  -- If both operands are NULL then the result of comparison is true.
  -- If either operand is NULL then the result is false.
  -- If neither operand is NULL the result is the same as if the flag were.
  | JumpIfNull
  -- If either operand is NULL then the take the jump.
  -- If this flag is not set then fall through if either operand is NULL.
  deriving (Eq, Show)

-- Compare the values in register P1 and P3. Jump to P2 if true.
data Cmp = Cmp
  { cmpOp :: CmpOp
  , reg :: Int
  , goto :: Addr
  , reg2 :: Int
  , cmpFlag :: Maybe CmpFlag
  }

instance Cmd Cmp where
  evalCmd Cmp{..} = do
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

-- The registers P1 through P1+P2-1 contain a single row of results.
data ResultRow = ResultRow
  { reg :: Int
  , len :: Int
  }

instance Cmd ResultRow where
  evalCmd ResultRow{..} = do
    regs <- use registers
    let row = V.fromList [regs ^. at r | r <- [reg .. reg+len-1]]
    yield .= [row]


data OpenRead = OpenRead
  { cursor :: CursorId
  , table :: TableId
  }

instance Cmd OpenRead where
  evalCmd OpenRead{..} = do
    void $ throwIfNothing (use $ db . dbData . at table)
      $ "unknown table" ++ show table
    throwIfJust (use $ openCursors . at cursor)
      $ "cursor " ++ show cursor ++ " is already used"
    openCursors %= (at cursor ?~ Cursor table 0)


-- Rewind cursor P1. Jump to P2 if the table is empty.
data Rewind = Rewind
  { cursor :: CursorId
  , goto :: Addr
  }

instance Cmd Rewind where
  evalCmd Rewind{..} = do
    Cursor table _ <- getCursor cursor
    -- rewind cursor
    openCursors %= (at cursor ?~ Cursor table 0)
    -- check if table is empty
    row <- rowAt cursor
    when (row == Nothing) $ jumpTo ?= goto

-- Read P2-th column from cursor P1, store result in P3, use P4 as
-- default.
data Column = Column
  { cursor :: CursorId
  , column :: Int
  , reg :: Int
  , def :: Maybe Val
  }

instance Cmd Column where
  evalCmd Column{..} = do
    row <- throwIfNothing (rowAt cursor)
      $ "cursor overflow"
    let val = fromMaybe def $ row !? column
    registers %= (at reg .~ val)

-- Advance P1, jump to P2 if not empty, P3 is a hint
data Next = Next
  { cursor :: CursorId
  , goto :: Addr
  } -- _uniquenessHint :: Bool }


instance Cmd Next where
  evalCmd Next{..} = do
    Cursor table offset <- getCursor cursor
    openCursors %= (at cursor ?~ Cursor table (offset+1))
    row <- rowAt cursor
    when (row /= Nothing) $ jumpTo ?= goto
