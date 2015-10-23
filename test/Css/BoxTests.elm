module Css.BoxTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Box exposing (..)
import Css.Size exposing (px, pct)
import Css.Color exposing (black)
import Css exposing (renderProperties)

suite : Spec
suite = describe "Css.BoxTests"
  [ describe "The box sizing function"
    [ it "should render the box sizing properties properly"
      [ renderProperties [boxSizing paddingBox] `shouldEqual`
          ("-webkit-box-sizing:padding-box;" ++
            "-moz-box-sizing:padding-box;" ++
            "-ms-box-sizing:padding-box;" ++
            "-o-box-sizing:padding-box;" ++
            "box-sizing:padding-box")
      , renderProperties [boxSizing borderBox] `shouldEqual`
          ("-webkit-box-sizing:border-box;" ++
            "-moz-box-sizing:border-box;" ++
            "-ms-box-sizing:border-box;" ++
            "-o-box-sizing:border-box;" ++
            "box-sizing:border-box")
      , renderProperties [boxSizing contentBox] `shouldEqual`
          ("-webkit-box-sizing:content-box;" ++
            "-moz-box-sizing:content-box;" ++
            "-ms-box-sizing:content-box;" ++
            "-o-box-sizing:content-box;" ++
            "box-sizing:content-box")
      ]
    ]
  , describe "The box shadow function"
    [ it "should render the box shadow properties properly"
        [ renderProperties [boxShadow <| shadow (px 20) (pct 30)] `shouldEqual`
            ("-webkit-box-shadow:20px 30%;" ++
              "-moz-box-shadow:20px 30%;" ++
              "-ms-box-shadow:20px 30%;" ++
              "-o-box-shadow:20px 30%;" ++
              "box-shadow:20px 30%")
        , renderProperties [boxShadow <| withColor black <| shadow (px 20) (pct 30)]
            `shouldEqual`
              ("-webkit-box-shadow:20px 30% #000000;" ++
                "-moz-box-shadow:20px 30% #000000;" ++
                "-ms-box-shadow:20px 30% #000000;" ++
                "-o-box-shadow:20px 30% #000000;" ++
                "box-shadow:20px 30% #000000")
        , renderProperties
            [boxShadow <| inset <| withColor black <| shadow (px 20) (pct 30)]
            `shouldEqual`
              ("-webkit-box-shadow:20px 30% #000000 inset;" ++
                "-moz-box-shadow:20px 30% #000000 inset;" ++
                "-ms-box-shadow:20px 30% #000000 inset;" ++
                "-o-box-shadow:20px 30% #000000 inset;" ++
                "box-shadow:20px 30% #000000 inset")
        , renderProperties
            [boxShadow <| withBlur (pct 40) (px 50) <| shadow (px 20) (pct 30)]
            `shouldEqual`
              ("-webkit-box-shadow:20px 30% 40% 50px;" ++
                "-moz-box-shadow:20px 30% 40% 50px;" ++
                "-ms-box-shadow:20px 30% 40% 50px;" ++
                "-o-box-shadow:20px 30% 40% 50px;" ++
                "box-shadow:20px 30% 40% 50px")
        , renderProperties
            [shadow (px 20) (pct 30)
              |> inset
              |> withBlur (pct 40) (px 50)
              |> boxShadow]
            `shouldEqual`
              ("-webkit-box-shadow:20px 30% 40% 50px inset;" ++
                "-moz-box-shadow:20px 30% 40% 50px inset;" ++
                "-ms-box-shadow:20px 30% 40% 50px inset;" ++
                "-o-box-shadow:20px 30% 40% 50px inset;" ++
                "box-shadow:20px 30% 40% 50px inset")
        , renderProperties
            [shadow (px 20) (pct 30)
              |> withColor black
              |> inset
              |> withBlur (pct 40) (px 50)
              |> boxShadow]
            `shouldEqual`
              ("-webkit-box-shadow:20px 30% 40% 50px #000000 inset;" ++
                "-moz-box-shadow:20px 30% 40% 50px #000000 inset;" ++
                "-ms-box-shadow:20px 30% 40% 50px #000000 inset;" ++
                "-o-box-shadow:20px 30% 40% 50px #000000 inset;" ++
                "box-shadow:20px 30% 40% 50px #000000 inset")
        ]
    ]
  ]
