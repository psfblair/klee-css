module CssTests where

import Spec exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)

suite : Spec
suite = describe "CssTests"
  [ prettyPrintTest ]

prettyPrintTest : Spec
prettyPrintTest =
  describe "render" [ True `shouldEqual` True ]
  {- TODO Make the code follow this pattern
    [ let stylesheet =
        a [ custom "-ms-lens-flare-style" "really-shiny"
          , custom "-ms-lens-flare-style" "really-shiny"
          ]
      in render stylesheet `shouldEqual`
        "a\n{\n  -ms-lens-flare-style : really-shiny;\n  -ms-lens-flare-style : really-shiny;\n}"
    ]
-}

{-
  custom p k   returns Css -> Css
  sel ? [ Css -> Css ]  : returns Css -> Css
-}
