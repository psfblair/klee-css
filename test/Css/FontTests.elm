module Css.FontTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Color exposing (..)
import Css.Font exposing (..)
import Css.Common exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.FontTests"
  [ describe "fontColor"
    [ it "can accept a named color"
      [ renderProperties [fontColor green] `shouldEqual` "color:#73D216"
      , renderProperties [fontColor (rgba 255 255 255 0.5)] 
          `shouldEqual` "color:rgba(255,255,255,0.5)" 
      ]
    , it "can accept common properties initial, inherit and other"
      [ renderProperties [fontColor initial] `shouldEqual` "color:initial"
      , renderProperties [fontColor inherit] `shouldEqual` "color:inherit" 
      , renderProperties [fontColor (other "wild-honey")] 
          `shouldEqual` "color:wild-honey" 
      ]
    ]
  , describe "font"
    [ it "can accept a named font"
      [ renderProperties [font caption] `shouldEqual` "font:caption"
      , renderProperties [font icon] `shouldEqual` "font:icon" 
      ]
    , it "can accept common properties initial, inherit and other"
      [ renderProperties [font initial] `shouldEqual` "font:initial"
      , renderProperties [font inherit] `shouldEqual` "font:inherit" 
      ]
    ]
  ]
