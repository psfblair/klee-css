module Css.BoxTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Box exposing (..)
import Css.Color exposing (black)
import Css.Common exposing (..)
import Css.Geometry exposing (..)
import Css exposing (renderProperties)

suite : Spec
suite = describe "Css.BoxTests"
  [ describe "The box sizing function"
    [ it "should render the box sizing properties properly"
      [ renderProperties [boxSizing paddingBox] 
          `shouldEqual` "box-sizing:padding-box"
      , renderProperties [boxSizing borderBox] 
          `shouldEqual` "box-sizing:border-box"
      , renderProperties [boxSizing contentBox] 
          `shouldEqual` "box-sizing:content-box"
      ]
    , it "should render the generic box sizing properties properly"
      [ renderProperties [boxSizing initial] `shouldEqual` "box-sizing:initial"
      , renderProperties [boxSizing inherit] `shouldEqual` "box-sizing:inherit"
      , renderProperties [boxSizing unset] `shouldEqual` "box-sizing:unset"
      , renderProperties [boxSizing (other "foo")] 
          `shouldEqual` "box-sizing:foo"
      , renderProperties [boxSizing (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "box-sizing:-webkit-foo;box-sizing:-moz-foo"
      -- , renderProperties [boxSizing all]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing auto]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing baseline]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing center]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing normal]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing none]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing visible]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing hidden]
      --     `shouldEqual` "box-sizing:should not compile"
      ]
    ]
  , describe "The box shadow function"
    [ it "should render the box shadow properties properly"
      [ renderProperties 
          [ boxShadow <| shadow (px 20) (pct 30) ] 
            `shouldEqual` "box-shadow:20px 30%"
      , renderProperties 
          [ boxShadow <| boxColor black <| shadow (px 20) (pct 30) ]
            `shouldEqual` "box-shadow:20px 30% #000000"
      , renderProperties
          [ boxShadow <| inset <| boxColor black <| shadow (px 20) (pct 30) ]
            `shouldEqual` "box-shadow:20px 30% #000000 inset"
      , renderProperties
          [ boxShadow <| boxBlur (pct 40) (px 50) <| shadow (px 20) (pct 30) ]
            `shouldEqual` "box-shadow:20px 30% 40% 50px"
      , renderProperties
          [ shadow (px 20) (pct 30)
            |> inset
            |> boxBlur (pct 40) (px 50)
            |> boxShadow ]
              `shouldEqual` "box-shadow:20px 30% 40% 50px inset"
      , renderProperties
          [ shadow (px 20) (pct 30)
            |> boxColor black
            |> inset
            |> boxBlur (pct 40) (px 50)
            |> boxShadow ]
              `shouldEqual` "box-shadow:20px 30% 40% 50px #000000 inset"
      -- , renderProperties 
      --     [ boxShadow <| shadow inherit (pct 30) ]
      --       `shouldEqual` "should not compile"
      -- , renderProperties 
      --     [ boxShadow <| shadow (pct 30) inherit ]
      --       `shouldEqual` "should not compile"
      -- , renderProperties 
      --     [ boxShadow <| boxColor inherit <| shadow (px 20) (pct 30) ]
      --       `shouldEqual` "should not compile"
      -- , renderProperties
      --     [ boxShadow <| inherit <| boxColor black <| shadow (px 20) (pct 30) ]
      --       `shouldEqual` "should not compile"
      -- , renderProperties
      --     [ boxShadow <| boxBlur inherit (px 50) <| shadow (px 20) (pct 30) ]
      --       `shouldEqual` "should not compile"
      -- , renderProperties
      --     [ boxShadow <| boxBlur inherit <| shadow (px 20) (pct 30) ]
      --       `shouldEqual` "should not compile"
      ]
    , it "should render the generic box shadow properties properly"
      [ renderProperties [boxShadow initial] `shouldEqual` "box-shadow:initial"
      , renderProperties [boxShadow inherit] `shouldEqual` "box-shadow:inherit"
      , renderProperties [boxShadow unset] `shouldEqual` "box-shadow:unset"
      , renderProperties [boxShadow (other "foo")] 
          `shouldEqual` "box-shadow:foo"
      , renderProperties [boxShadow (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "box-shadow:-webkit-foo;box-shadow:-moz-foo"
      -- , renderProperties [boxShadow all]
      --     `shouldEqual` "box-shadow:should not compile"
      -- , renderProperties [boxShadow auto]
      --     `shouldEqual` "box-shadow:should not compile"
      -- , renderProperties [boxShadow baseline]
      --     `shouldEqual` "box-shadow:should not compile"
      -- , renderProperties [boxShadow center]
      --     `shouldEqual` "box-shadow:should not compile"
      -- , renderProperties [boxShadow normal]
      --     `shouldEqual` "box-shadow:should not compile"
      -- , renderProperties [boxShadow none]
      --     `shouldEqual` "box-shadow:should not compile"
      -- , renderProperties [boxShadow visible]
      --     `shouldEqual` "box-shadow:should not compile"
      -- , renderProperties [boxShadow hidden]
      --     `shouldEqual` "box-shadow:should not compile"
      ]
    ]
  ]
