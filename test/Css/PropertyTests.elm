module Css.PropertyTests where

import Spec exposing (..)

import Css.Property exposing (..)

suite : List Spec
suite = [ prefixedTest, mergeTest ]

prefixedTest : Spec
prefixedTest =
  describe "PrefixedOrNot"
    [ ("a" |> Plain |> unPlain) `shouldEqual` "a"
    , ([ ("a","b"),("c","d")] |> Prefixed |> unPrefixed)
          `shouldEqual` [("a","b"),("c","d")]
    ]

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
