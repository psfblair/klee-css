module Css.Internal.InternalColorTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Color as ElmColor
import Css.Internal.Color exposing (..)

suite : Spec
suite = describe "Css.Internal.InternalColorTests"
  [ describe "lighten"
    [ it "lightens a color by a factor"
      [ lighten 0.2 (ElmColor.rgba 25 210 130 1.0) 
          `shouldEqual` ElmColor.rgba 71 219 155 1.0 ]
    , it "does not lighten a color past white"
      [ lighten 0.8 (ElmColor.rgba 25 210 130 1.0) 
          `shouldEqual` ElmColor.rgba 209 246 230 1.0 ]
    ]
  , describe "darken"
    [ it "darkens a color by a factor"
      [ darken 0.2 (ElmColor.rgba 25 210 130 1.0) 
          `shouldEqual` ElmColor.rgba 20 168 104 1.0 ]
    , it "does not darken a color past black"
      [ darken 0.8 (ElmColor.rgba 25 210 130 1.0) 
          `shouldEqual` ElmColor.rgba 5 42 26 1.0 ]
    ]    
  , let whiteRgba = ElmColor.rgba 255 255 255 1.0
        blackRgba = ElmColor.rgba 0 0 0 1.0
        transparentRgba = ElmColor.rgba 255 255 255 0
        transparentBlack = ElmColor.rgba 0 0 0 0
        halfTransparent = ElmColor.rgba 128 128 128 0.5
    in describe "lerp"
        [ it "gives the same color back if both colors are the same"
          [ lerp 0.2 (ElmColor.rgba 201 237 181 0.478) (ElmColor.rgba 201 237 181 0.478)
              `shouldEqual` ElmColor.rgba 201 237 181 0.48 ]
        , it "gives the same color back if the lerp factor is zero"
          [ lerp 0 transparentRgba whiteRgba `shouldEqual` transparentRgba
          , lerp 0.001 transparentRgba whiteRgba `shouldEqual` transparentRgba
          , lerp (-0.001) transparentRgba whiteRgba `shouldEqual` transparentRgba
          ]
        , it "gives the same color back at -1 if the color is less than the bound"
          [ lerp (-1.0) transparentRgba whiteRgba `shouldEqual` transparentRgba
          ]
        , it "gives the bound color at 100%"
          [ lerp 1.0 transparentRgba whiteRgba `shouldEqual` whiteRgba
          , lerp 1.001 transparentRgba whiteRgba `shouldEqual` whiteRgba
          , lerp 2.0 transparentRgba whiteRgba `shouldEqual` whiteRgba
          , lerp 0.999 transparentRgba whiteRgba 
              `shouldEqual` ElmColor.rgba 255 255 255 1.0
          ]
        , it "gives the right halfway color"
          [ lerp 0.5 blackRgba whiteRgba 
              `shouldEqual` ElmColor.rgba 128 128 128 1.0
          , lerp 0.5 transparentBlack whiteRgba `shouldEqual` halfTransparent
          , lerp 0.501 blackRgba whiteRgba 
              `shouldEqual` ElmColor.rgba 128 128 128 1.0
          , lerp 0.499 blackRgba whiteRgba 
              `shouldEqual` ElmColor.rgba 127 127 127 1.0
          ]
        , it "should lerp down (i.e., lighter color first)"
          [ lerp 0 whiteRgba blackRgba `shouldEqual` whiteRgba
          , lerp 1.0 whiteRgba blackRgba `shouldEqual` blackRgba
          , lerp 0.5 whiteRgba blackRgba 
              `shouldEqual` ElmColor.rgba 128 128 128 1.0
          ]
        , it "should lerp backwards (i.e., using a negative factor)"
          [ lerp (-1.0) blackRgba whiteRgba `shouldEqual` blackRgba
          , lerp (-0.5) blackRgba whiteRgba `shouldEqual` blackRgba
          , lerp (-0.5) (ElmColor.rgba 127 127 127 1.0) whiteRgba 
              `shouldEqual` ElmColor.rgba 63 63 63 1.0
          ]
        , it "should lerp a red color"
          [ lerp 0.5 (ElmColor.rgba 255 0 0 1.0) whiteRgba 
              `shouldEqual` ElmColor.rgba 255 128 128 1.0
          , lerp (-0.5) (ElmColor.rgba 255 0 0 1.0) whiteRgba 
              `shouldEqual` ElmColor.rgba 255 0 0 1.0
          , lerp 0.5 (ElmColor.rgba 255 0 0 1.0) blackRgba 
              `shouldEqual` ElmColor.rgba 128 0 0 1.0
          , lerp (-0.5) (ElmColor.rgba 255 0 0 1.0) blackRgba 
              `shouldEqual` ElmColor.rgba 255 0 0 1.0
          ]
        , it "should lerp a green color"
          [ lerp 0.5 (ElmColor.rgba 0 255 0 1.0) whiteRgba 
              `shouldEqual` ElmColor.rgba 128 255 128 1.0
          , lerp (-0.5) (ElmColor.rgba 0 255 0 1.0) whiteRgba 
              `shouldEqual` ElmColor.rgba 0 255 0 1.0
          , lerp 0.5 (ElmColor.rgba 0 255 0 1.0) blackRgba 
              `shouldEqual` ElmColor.rgba 0 128 0 1.0
          ]
        , it "should lerp a blue color"
          [ lerp 0.5 (ElmColor.rgba 0 0 255 1.0) whiteRgba 
              `shouldEqual` ElmColor.rgba 128 128 255 1.0
          , lerp (-0.5) (ElmColor.rgba 0 0 255 1.0) whiteRgba 
              `shouldEqual` ElmColor.rgba 0 0 255 1.0
          , lerp 0.5 (ElmColor.rgba 0 0 255 1.0) blackRgba 
              `shouldEqual` ElmColor.rgba 0 0 128 1.0
          ]
        , it "should lerp a random color"
          [ lerp 0.5 (ElmColor.rgba 201 237 181 0.478) whiteRgba 
              `shouldEqual` ElmColor.rgba 228 246 218 0.74
          , lerp 0.5 (ElmColor.rgba 201 237 181 0.478) blackRgba 
              `shouldEqual` ElmColor.rgba 101 119 91 0.74
          ]
        ]
  ]
