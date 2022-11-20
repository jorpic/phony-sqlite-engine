module Engine
    ( Cmd(..)
    , evalToYield
    , runQuery
    , Db(..)
    , Val(..)
    , CmpOp(..)
    ) where

import Control.Lens
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!?))

import Engine.Types
import Engine.State
import Engine.Cmd

type Query = Vector AnyCmd

evalToYield :: Query -> Engine [Row]
evalToYield query = loop
  where
    loop = do
      EngineState{_ip} <- get

      case query !? _ip of
        Nothing -> lift $ throwE $ "Invalid location: " ++ show _ip
        Just cmd -> do
          evalCmd cmd -- FIXME: catch and add context to error
          EngineState{_jumpTo, _yield, _halted} <- get

          if _halted
            then return []
            else do
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
