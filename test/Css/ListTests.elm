module Css.ListTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Position exposing (..)

import Css.Border exposing (..)
import Css.Size exposing (..)
import Css.Color exposing (..)
import Css.List exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.ListTests"
  [ describe "The  functions"
    [ it "should "
      [ renderProperties []
          `shouldEqual` ""
      ]
    ]
  ]
