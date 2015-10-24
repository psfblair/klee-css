module Css.BackgroundTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Background exposing (..)
import Css.Size exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.BackgroundTests"
  [ describe ""
    [ it "should "
      [ renderProperties []
          `shouldEqual` ""
      , renderProperties []
          `shouldEqual` ""
      ]
    ]
  ]
