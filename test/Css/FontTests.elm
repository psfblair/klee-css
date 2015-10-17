module Css.FontTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Font exposing (..)
import Css.Common exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.FontTests"
  [ describe "font"
    [ it "can accept a named font"
      [ renderProperties [font caption] `shouldEqual` "font:caption"
      , renderProperties [font icon] `shouldEqual` "font:icon" 
      ]
    -- , it "can accept common properties initial, inherit and other"
    --   [ renderProperties [font initial] `shouldEqual` "font:initial"
    --   , renderProperties [font inherit] `shouldEqual` "font:inherit" 
    --   , renderProperties [font <| other 1] `shouldEqual` "font:1" 
    --   ]
    ]
  ]
