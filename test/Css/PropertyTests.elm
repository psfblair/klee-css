module Css.PropertyTests where

import Spec exposing (..)

import Css.Property exposing (..)

suite : List Spec
suite = [ prefixedTest, mergeTest ]

prefixedTest : Spec  
prefixedTest =
  describe "Prefixed"
             [ fromString "a" `shouldEqual` Plain "a"
             , ("a" |> fromString |> unPlain) `shouldEqual` "a"
             , ([ ("a","b"),("c","d")] |> WithPrefix |> unPrefixed)
             `shouldEqual` [("a","b"),("c","d")]
             ]

mergeTest : Spec
mergeTest = 
  let plain1 = Plain "a"
      plain2 = Plain "b"
      withPrefix1 = WithPrefix [("a","b"),("c","d")]
      withPrefix2 = WithPrefix [("c","e")]
  in describe "merge"
       [ merge plain1 plain2 `shouldEqual` Plain "ab"
       , merge plain1 withPrefix1 `shouldEqual` WithPrefix [("a","ab"),("c","ad")]
       , merge withPrefix1 plain1 `shouldEqual` WithPrefix [("a","ba"),("c","da")]
       , merge withPrefix1 withPrefix2 `shouldEqual` WithPrefix [("c","de")]
       ]
