module Engine.Types where

import Data.Map (Map)
import Data.Vector (Vector)

type CursorId = Int
type TableId = String

type Addr = Int
data Val = IntVal Integer | RealVal Double | StrVal String
  deriving (Eq, Ord, Show)
type Row = Vector (Maybe Val)
type Table = Vector Row

newtype Db = Db
  { _dbData :: Map TableId Table
  }
  deriving Show

data Cursor = Cursor TableId Int
  deriving (Eq, Show)
