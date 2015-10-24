module Css.Internal.PropertyTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Property exposing (..)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.PropertyTests"
  [ stringValueFactoryTest, commaListValueFactoryTest ]

stringValueFactoryTest : Spec
stringValueFactoryTest =
  describe "stringValueFactory"
    [ stringValue "a" `shouldEqual` Value (Plain "a")
    ]

commaListValueFactoryTest : Spec
commaListValueFactoryTest =
  describe "pairValueFactory"
    [ it "should wrap an empty list"
        [ commaListValue stringValue [] `shouldEqual` emptyValue ]
    , it "should wrap plain values"
        [ commaListValue stringValue ["a", "b"] `shouldEqual` Value (Plain "a,b")]
    ]

-- TODO Test wrappers involving prefixed values in order to test merge instead of test below
{-
mergeTest : Spec
mergeTest =
  let plain1 = Plain "a"
      plain2 = Plain "b"
      withPrefix1 = Prefixed [("a","b"),("c","d")]
      withPrefix2 = Prefixed [("c","e")]
  in describe "merge"
       [ merge plain1 plain2 `shouldEqual` Plain "ab"
       , merge plain1 withPrefix1 `shouldEqual` Prefixed [("a","ab"),("c","ad")]
       , merge withPrefix1 plain1 `shouldEqual` Prefixed [("a","ba"),("c","da")]
       , merge withPrefix1 withPrefix2 `shouldEqual` Prefixed [("c","de")]
       ]
-}
