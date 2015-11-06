module Css.FontTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Color exposing (..)
import Css.Common exposing (..)
import Css.Font exposing (..)
import Css.Size exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.FontTests"
  [ describe "fontColor"
    [ it "can accept a named color"
      [ renderProperties [fontColor green] `shouldEqual` "color:#73D216"
      ]
    , it "can accept common properties initial, inherit and other"
      [ renderProperties [fontColor initial] `shouldEqual` "color:initial"
      , renderProperties [fontColor inherit] `shouldEqual` "color:inherit" 
      , renderProperties [fontColor (other "wild-honey")] 
          `shouldEqual` "color:wild-honey" 
      -- Should not compile:
      -- , renderProperties [fontColor all]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor auto]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor baseline]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor center]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor normal]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor none]
      --     `shouldEqual` "color:should not compile"      
      -- , renderProperties [fontColor visible]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor hidden]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor unset]
      --     `shouldEqual` "color:should not compile"          
      ]
    ]
  , describe "font"
    [ it "will accept a named font"
      [ renderProperties [font caption] `shouldEqual` "font:caption"
      , renderProperties [font icon] `shouldEqual` "font:icon" 
      , renderProperties [font menu] `shouldEqual` "font:menu" 
      , renderProperties [font messageBox] `shouldEqual` "font:message-box" 
      , renderProperties [font smallCaption] `shouldEqual` "font:small-caption" 
      , renderProperties [font statusBar] `shouldEqual` "font:status-bar" 
      ]
    , it "will accept a composed font"
      [ renderProperties [font (aFont (px 20) ["Lubalin Graph Medium"] [fantasy])] 
          `shouldEqual` "font:20px \"Lubalin Graph Medium\",fantasy"
      ]      
    , it "will accept common properties initial, inherit and other"
      [ renderProperties [font initial] `shouldEqual` "font:initial"
      , renderProperties [font inherit] `shouldEqual` "font:inherit" 
      , renderProperties [font (other "frankfurt")] 
          `shouldEqual` "font:frankfurt" 
      -- Should not compile:
      -- , renderProperties [font all]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font auto]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font baseline]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font center]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font normal]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font none]
      --     `shouldEqual` "font:should not compile"      
      -- , renderProperties [font visible]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font hidden]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font unset]
      --     `shouldEqual` "font:should not compile"          
      ]
    ]
  ]
