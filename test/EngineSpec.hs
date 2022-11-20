module EngineSpec (spec) where

import Test.Hspec

import Data.Map qualified as Map
import Data.Vector qualified as V

import Engine.Cmd
import Engine (runQuery, Db(..), Val(..))


testDb :: Db
testDb = Db $ Map.fromList
  [("users", V.fromList $ map (V.fromList . map Just)
    [ [IntVal 1, StrVal "john doe", IntVal 42]
    , [IntVal 2, StrVal "jane doe", IntVal 37]
    ])
  ]

(~>) :: Cmd c => Int -> c -> AnyCmd
(~>) = const Cmd

spec :: SpecWith ()
spec = describe "engine" $ do
  let evalsTo q r
        = runQuery testDb (V.fromList q) `shouldBe` Right (map V.fromList r)
  it "evals trivial query"
    $ [ 0 ~> Init 0
      , 1 ~> Halt
      ] `evalsTo` []
  it "evals select constants "
    $ [ 0 ~> Init 0
      , 1 ~> Real 1 3.7
      , 2 ~> Integer 2 8
      , 3 ~> String8 3 "hello"
      , 4 ~> ResultRow 1 3
      , 5 ~> Halt
      ] `evalsTo` [map Just [RealVal 3.7, IntVal 8, StrVal "hello"]]

  it "can select from table"
    $ [ 0 ~> Init {goto = 0}
      , 1 ~> OpenRead {cursor = 0, table = "users"}
      , 2 ~> Rewind {cursor = 0, goto = 7}
      , 3 ~>  Column {cursor = 0, column = 0, reg = 1, def = Nothing}
      , 4 ~>  Column {cursor = 0, column = 1, reg = 2, def = Nothing}
      , 5 ~>  ResultRow {reg = 1, len = 2}
      , 6 ~> Next {cursor = 0, goto = 3}
      , 7 ~> Halt
      ]
      `evalsTo` (map (map Just))
        [ [IntVal 1, StrVal "john doe"]
        , [IntVal 2, StrVal "jane doe"]
        ]
  it "can select from table with a filter"
    $ [ 0 ~> Init {goto = 9}
      , 1 ~> OpenRead {cursor = 0, table = "users"}
      , 2 ~> Rewind {cursor = 0, goto = 8}
      , 3 ~>  Column {cursor = 0, column = 2, reg = 1, def = Nothing}
      , 4 ~>  Cmp {cmpOp = Lt, reg = 1, reg2 = 3, goto = 7,  cmpFlag = Nothing}
      , 5 ~>  Column {cursor = 0, column = 1, reg = 2, def = Nothing}
      , 6 ~>  ResultRow {reg = 2, len = 1}
      , 7 ~> Next {cursor = 0, goto = 3}
      , 8 ~> Halt
      , 9 ~> Integer {reg = 3, iVal = 40}
      , 0 ~> Goto {goto = 1}
      ]
      `evalsTo` (map (map Just))
        [ [StrVal "john doe"]
        ]
