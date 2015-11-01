module Css.BackgroundTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Property exposing (stringValue)

import Css.Background exposing (..)
import Css.Color exposing (..)
import Css.Common exposing (..)
import Css.Size exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.BackgroundTests"
  [ describe "backgroundPosition"
    [ it "should render placed positions properly"
      [ renderProperties [backgroundPosition (placed sideLeft sideTop)]
          `shouldEqual` "background-position:left top"
      , renderProperties [backgroundPosition (placed sideCenter sideMiddle)]
          `shouldEqual` "background-position:center center"
      ]
    , it "should render positioned positions properly"
      [ renderProperties [backgroundPosition (positioned (px 10) (px 20))]
          `shouldEqual` "background-position:10px 20px"
      , renderProperties [backgroundPosition (positioned (pct 20) (pct 30))]
          `shouldEqual` "background-position:20% 30%"
      , renderProperties [backgroundPosition (positioned (px 20) (pct 30))]
          `shouldEqual` "background-position:20px 30%"
      ]
    , it "should render generic positions properly"
      [ renderProperties [backgroundPosition initial]
          `shouldEqual` "background-position:initial"
      , renderProperties [backgroundPosition inherit]
          `shouldEqual` "background-position:inherit"
      , renderProperties [backgroundPosition (stringValue "foo" |> other)]
          `shouldEqual` "background-position:foo"
      -- Should not compile:
      -- , renderProperties [backgroundPosition all]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [backgroundPosition auto]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [backgroundPosition baseline]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [backgroundPosition center]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [backgroundPosition normal]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [backgroundPosition visible]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [backgroundPosition hidden]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [backgroundPosition unset]
      --     `shouldEqual` "background-position:should not compile"
      ]
    ]
  , describe "backgroundSize"
    [ it "should render named sizes properly"
      [ renderProperties [backgroundSize cover]
          `shouldEqual` "background-size:cover"
      , renderProperties [backgroundSize contain]
          `shouldEqual` "background-size:contain"
      ]
    , it "should render dimensioned sizes properly"
      [ renderProperties [backgroundSize ((px 20) `by` (px 30))]
          `shouldEqual` "background-size:20px 30px"
      , renderProperties [backgroundSize (20 |> px |> bgWidth)]
          `shouldEqual` "background-size:20px auto"
      ]
    , it "should render generic sizes properly" 
      [ renderProperties [backgroundSize initial]
          `shouldEqual` "background-size:initial"
      , renderProperties [backgroundSize inherit]
          `shouldEqual` "background-size:inherit"
      , renderProperties [backgroundSize auto]
          `shouldEqual` "background-size:auto"
      , renderProperties [backgroundSize (stringValue "foo" |> other)]
          `shouldEqual` "background-size:foo"
      -- Should not compile:
      -- , renderProperties [backgroundSize all]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [backgroundSize baseline]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [backgroundSize center]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [backgroundSize normal]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [backgroundSize visible]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [backgroundSize hidden]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [backgroundSize unset]
      --     `shouldEqual` "background-size:should not compile"
      ]    
    ]
  , describe "backgroundColor"
    [ it "should render colors properly"
      [ renderProperties [backgroundColor green]
          `shouldEqual` "background-color:#73D216"
      , renderProperties [backgroundColor (rgba 255 255 255 0.5)]
          `shouldEqual` "background-color:rgba(255,255,255,0.5)"
      ]
    , it "should render transparent properly"
      [ renderProperties [backgroundColor transparent]
          `shouldEqual` "background-color:transparent"
      ]
    , it "should render generic colors properly" 
      [ renderProperties [backgroundColor initial]
          `shouldEqual` "background-color:initial"
      , renderProperties [backgroundColor inherit]
          `shouldEqual` "background-color:inherit"
      , renderProperties [backgroundColor (stringValue "foo" |> other)]
          `shouldEqual` "background-color:foo"
      -- Should not compile:
      -- , renderProperties [backgroundColor all]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [backgroundColor auto]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [backgroundColor baseline]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [backgroundColor center]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [backgroundColor normal]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [backgroundColor visible]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [backgroundColor hidden]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [backgroundColor unset]
      --     `shouldEqual` "background-color:should not compile"
      ]          
    ]
  ]
