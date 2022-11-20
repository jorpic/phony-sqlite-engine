module Engine.Cmd where

import Engine.Types

data CmpOp = Eq | Ne | Lt | Gt | Le | Ge
  deriving (Eq, Show)

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

data Cmd
  = Init { goto :: Addr }
    -- ^ If P2 is not zero, jump to instruction P2.
  | Goto {goto :: Addr}
  | Halt
  | Real  {reg :: Int, rVal :: Double}
  | Integer  {reg :: Int, iVal :: Integer}
  | String8 {reg :: Int, sVal :: String}
  | Cmp
    -- ^ Compare the values in register P1 and P3. Jump to P2 if true.
    { cmpOp :: CmpOp
    , reg :: Int
    , goto :: Addr
    , reg2 :: Int
    , cmpFlag :: Maybe CmpFlag
    }

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
