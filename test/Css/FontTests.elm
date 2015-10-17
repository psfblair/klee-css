module Css.FontTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Property exposing (stringValueFactory)

import Css.Font exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.FontTests"
  [ describe "font"
    [ it "can accept a named font"
      [ renderProperties [font caption] `shouldEqual` "font:caption"
      ]
    ]
  ]
