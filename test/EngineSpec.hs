module EngineSpec (spec) where

import Test.Hspec

import Data.Map qualified as Map
import Data.Vector qualified as V
import Engine (runQuery, Db(..), Val(..), Cmd(..))


testDb :: Db
testDb = Db $ Map.fromList
  [("users", V.fromList $ map (V.fromList . map Just)
    [ [IntVal 1, StrVal "john doe", IntVal 42]
    , [IntVal 2, StrVal "jane doe", IntVal 37]
    ])
  ]

spec :: SpecWith ()
spec = describe "engine" $ do
  let evalsTo q r
        = runQuery testDb (V.fromList q) `shouldBe` Right (map V.fromList r)
  it "evals trivial query"
    $ [Init 0, Halt] `evalsTo` []
  it "evals select constants "
    $ [ Init 0
      , Real 1 3.7, Integer 2 8, String8 3 "hello"
      , ResultRow 1 3
      ,  Halt
      ] `evalsTo` [map Just [RealVal 3.7, IntVal 8, StrVal "hello"]]

  it "can select from table"
    $ [ Init {goto = 0}
      , OpenRead {cursor = 0, table = "users"}
      , Rewind {cursor = 0, goto = 7}
      ,  Column {cursor = 0, column = 0, reg = 1, def = Nothing}
      ,  Column {cursor = 0, column = 1, reg = 2, def = Nothing}
      ,  ResultRow {reg = 1, len = 2}
      , Next {cursor = 0, goto = 3}
      , Halt
      ]
      `evalsTo` (map (map Just))
        [ [IntVal 1, StrVal "john doe"]
        , [IntVal 2, StrVal "jane doe"]
        ]
