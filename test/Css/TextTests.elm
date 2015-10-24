module Css.TextTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Text exposing (..)
import Css.Size exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.TextTests"
  [ describe ""
    [ it "should "
      [ renderProperties []
          `shouldEqual` ""
      , renderProperties []
          `shouldEqual` ""
      ]
    ]
  ]
