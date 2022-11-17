module EngineSpec (spec) where

import Test.Hspec

import Data.Map qualified as Map
import Data.Vector qualified as V
import Engine (runQuery, Db(..), Val(..), Cmd(..))


spec :: SpecWith ()
spec = describe "engine" $ do
  let db = Db Map.empty
  let evalsTo q r = runQuery db (V.fromList q) `shouldBe` r
  it "evals trivial query"
    $ [Init 0, Halt] `evalsTo` Right []
  it "evals select constants "
    $ [ Init 0
      , Real 1 3.7, Integer 2 8, String8 3 "hello"
      , ResultRow 1 3
      ,  Halt
      ] `evalsTo` Right [map Just [RealVal 3.7, IntVal 8, StrVal "hello"]]
