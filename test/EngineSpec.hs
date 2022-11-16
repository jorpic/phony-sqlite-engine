module EngineSpec (spec) where

import Test.Hspec

import Data.Map qualified as Map
import Data.Vector qualified as V
import Engine (runQuery, Db(..), Cmd(..))


spec :: SpecWith ()
spec = describe "engine" $ do
  let db = Db Map.empty
  let evalsTo q r = runQuery db (V.fromList q) `shouldBe` r
  it "evals trivial query"
    $ [Init 0, Halt] `evalsTo` Right []
