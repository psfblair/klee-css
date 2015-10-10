module CssTests where

import Spec exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "CssTests"
  [ describe "render"
    [ let stylesheet =
            a [ custom "-ms-lens-flare-style" "really-shiny"
              , custom "-ms-lens-flare-style" "really-shiny"
              ]
      in render stylesheet `shouldEqual`
          ("\na" ++
            "\n{\n  -ms-lens-flare-style : really-shiny;" ++
               "\n  -ms-lens-flare-style : really-shiny;" ++
            "\n}\n\n\n/* Generated with elm-css */")
    ]
  ]
