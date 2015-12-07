module Css.ColorTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Common exposing (otherValue)
import Css.Internal.Property exposing (Value, stringValue)
import Css.Internal.Utils as Utils

import Color as ElmColor
import Css.ColorsAndStrokes exposing (..)


-------------------------------------------------------------------------------
-- We test the creation of properties from colors in the tests for 
-- those properties in other modules.

suite : Spec
suite = describe "Css.ColorTests"
  [ describe "hex"
    [ it "can parse color strings"
      [ (hex "#FF6666") testColorExtractor
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:1")
      , (hex "44F") testColorExtractor
          `shouldEqual` (stringValue "r:68, g:68, b:255, a:1")
      , (hex "440220") testColorExtractor
          `shouldEqual` (stringValue "r:68, g:2, b:32, a:1")
      -- CSS4 color strings   
      , (hex "#FF666680") testColorExtractor
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:0.5")
      , (hex "#FFF8") testColorExtractor
          `shouldEqual` (stringValue "r:255, g:255, b:255, a:0.53")
      ]
    , it "handles invalid colors"  
      [ (hex "blarg") testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR STRING: blarg")
      , (hex "FA") testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR STRING: FA")
      , (hex "1") testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR STRING: 1")
      ]
    ]
  , describe "rgba"
    [ it "creates the proper color descriptor"
      [ (rgba 255 102 102 0.5) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:0.5")
      ]
    , it "handles invalid colors"  
      [ (rgba 255 -1 102 0.5) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,-1,102,0.5")
      , (rgba 256 102 102 0.5) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 256,102,102,0.5")
      , (rgba 255 102 102 -0.1) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,102,102,-0.1")
      , (rgba 256 102 102 1.2) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 256,102,102,1.2")
      ]
    ]
  , describe "rgb"
    [ it "creates the proper color"
      [ (rgb 255 102 102) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:1")
      ]
    , it "handles invalid colors"  
      [ (rgb -1 102 102) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: -1,102,102")
      , (rgb 256 102 102) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 256,102,102")
      , (rgb 255 -1 102) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,-1,102")
      , (rgb 255 256 102) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,256,102")
      , (rgb 255 102 -1) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,102,-1")
      , (rgb 255 102 256) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,102,256")
      ]
    ]
  , describe "hsla"
    [ it "creates the proper color"
      [ (hsla 99 0.61 0.82 0.478) testColorExtractor
          `shouldEqual` (stringValue "h:99, s:61%, l:82%, a:0.478")
      ]
    , it "handles invalid colors"  
      [ (hsla 361 0.61 0.82 0.478) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 361,0.61,0.82,0.478")
      , (hsla -1 0.61 0.82 0.478) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: -1,0.61,0.82,0.478")
      , (hsla 99 2.0 0.82 0.478) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,2,0.82,0.478")
      , (hsla 99 -0.5 0.82 0.478) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,-0.5,0.82,0.478")
      , (hsla 99 0.61 -0.1 0.478) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,-0.1,0.478")
      , (hsla 99 0.61 1.1 0.478) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,1.1,0.478")
      , (hsla 99 0.61 0.82 1.2) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,0.82,1.2")
      , (hsla 99 0.61 0.82 -0.1) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,0.82,-0.1")
      ]
    ]
  , describe "hsl"
    [ it "creates the proper color"
      [ (hsl 99 0.61 0.82) testColorExtractor
          `shouldEqual` (stringValue "h:99, s:61%, l:82%, a:1")
      ]
    , it "handles invalid colors"  
      [ (hsl 361 0.61 0.82) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 361,0.61,0.82")
      , (hsl -1 0.61 0.82) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: -1,0.61,0.82")
      , (hsl 99 2.0 0.82) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,2,0.82")
      , (hsl 99 -0.5 0.82) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,-0.5,0.82")
      , (hsl 99 0.61 -0.1) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,-0.1")
      , (hsl 99 0.61 1.1) testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,1.1")
      ]
    ]
  , describe "the CSS named colors" 
    [ it "are rendered as strings"
      [ cssWhite testColorExtractor `shouldEqual` (stringValue "White")
      ]
    ]
  , describe "the Elm named colors" 
    [ it "are rendered as rgba colors"
      [ red testColorExtractor `shouldEqual` (stringValue "r:204, g:0, b:0, a:1")
      ]
    ]
  , describe "lerp"
    [ it "gives the right halfway color"
      [ (lerp 0.5 black white) testColorExtractor
          `shouldEqual` (stringValue "r:128, g:128, b:128, a:1")
      , (lerp 0.5 (rgba 0 0 0 0) white) testColorExtractor
          `shouldEqual` (stringValue "r:128, g:128, b:128, a:0.5")
      -- TODO Elm color gives NaN for H of black; this is waiting for the fix
      -- , (lerp 0.5 (hsla 0 0 0 1.0) white)  testColorExtractor
      --     `shouldEqual` (stringValue "h:0, s:0%, l:50%, a:1")
      ]
    , it "should lerp a named color"
      [ (lerp 0.5 cssRed white) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:128, b:128, a:1")
      , (lerp (-0.5) cssRed white) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:0, b:0, a:1")
      , (lerp 0.5 cssRed black) testColorExtractor
          `shouldEqual` (stringValue "r:128, g:0, b:0, a:1")
      , (lerp (-0.5) cssRed black) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:0, b:0, a:1")
      ]
    , it "should lerp one color component"
      [ (lerp 0.5 (rgba 255 0 0 1.0) white) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:128, b:128, a:1")
      ]
    , it "gives the same color back when the lerp factor is zero"
      [ (lerp 0 (rgba 255 0 0 1.0) white) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:0, b:0, a:1")
      , (lerp 0 (rgba 255 0 0 1.0) black) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:0, b:0, a:1")
      -- TODO Elm color gives NaN for H and S of white; this is waiting for a fix
      -- , (lerp 0 (hsla 0 0 1.0 0) white) testColorExtractor
      --     `shouldEqual` (stringValue "h:0 s:0% l:100% a:0")
      ]
    , it "gives the boundary color at 100%"
      [ (lerp 1.0 green white) testColorExtractor
          `shouldEqual` (stringValue "r:255, g:255, b:255, a:1")
      ]
    , it "uses rgb/hsl format depending on the first argument, not the bound"
      [ -- TODO Elm color gives NaN for H of black; this is waiting for a fix
      -- (lerp 0.5 (hsla 0 0 0 1.0) (rgba 255 255 255 1.0)) testColorExtractor
      --     `shouldEqual` (stringValue "h:0, s:0%, l:50%, a:1")
      -- TODO Elm color gives NaN for H of black; this is waiting for a fix
      -- , (lerp 0.5 (hsla 0 0 0 1.0) white)  testColorExtractor
      --     `shouldEqual` (stringValue "h:0, s:0%, l:50%, a:1")              
      -- , 
        (lerp 0.5 (rgba 255 255 255 1.0) (hsla 0 0 0 1.0))  testColorExtractor
          `shouldEqual` (stringValue "r:128, g:128, b:128, a:1")
      , (lerp 0.5 white (hsla 0 0 0 1.0))  testColorExtractor
          `shouldEqual` (stringValue "r:128, g:128, b:128, a:1")
      ]          
    , it "should lerp down (i.e., lighter color first)"
      [ (lerp 0.5 white black) testColorExtractor 
          `shouldEqual` (stringValue "r:128, g:128, b:128, a:1")
      ]
    , it "should lerp backwards (i.e., using a negative factor)"
      [ (lerp (-1.0) black white) testColorExtractor  
          `shouldEqual` (stringValue "r:0, g:0, b:0, a:1")
      , (lerp (-0.5) black white) testColorExtractor
          `shouldEqual` (stringValue "r:0, g:0, b:0, a:1")
      , (lerp (-0.5) (rgba 127 127 127 1.0) white)   testColorExtractor
          `shouldEqual` (stringValue "r:63, g:63, b:63, a:1")
      ]
    -- , it "should not accept preset non-colors"
    --   [ (lerp (1.0) transparent white) testColorExtractor  
    --       `shouldEqual` (stringValue "should not compile")
    --   , (lerp (1.0) black currentColor) testColorExtractor
    --       `shouldEqual` (stringValue "should not compile")
    --   ]
    -- , it "should not accept generic property values"
    --   [ (lerp (1.0) initial white) testColorExtractor  
    --       `shouldEqual` (stringValue "should not compile")
    --   , (lerp (1.0) black initial) testColorExtractor
    --       `shouldEqual` (stringValue "should not compile")
    --   ]
    ]
  , describe "lighten"
    [ it "lightens an RGB color by a factor proportional to its distance from white"
      [ (lighten 0.2 (rgba 25 210 130 1.0)) testColorExtractor
          `shouldEqual` (stringValue "r:71, g:219, b:155, a:1") ]
    , it "does not lighten an RGB color past white"
      [ (lighten 0.8 (rgba 25 210 130 1.0)) testColorExtractor
          `shouldEqual` (stringValue "r:209, g:246, b:230, a:1") ]
    , it "lightens an HSL color by a factor proportional to its distance from white"
      [ (lighten 0.2 (hsla 154 0.79 0.46 1.0)) testColorExtractor
          `shouldEqual` (stringValue "h:154, s:67%, l:57%, a:1") ]
    , it "does not lighten an HSL color past white"
      [ (lighten 0.8 (hsla 154 0.79 0.46 1.0)) testColorExtractor
          `shouldEqual` (stringValue "h:154, s:67%, l:89%, a:1") ]
    -- , it "should not accept preset non-colors"
    --   [ (lighten 0.8 transparent) testColorExtractor  
    --       `shouldEqual` (stringValue "should not compile")
    --   , (lighten 0.8 currentColor) testColorExtractor
    --       `shouldEqual` (stringValue "should not compile")
    --   ]
    -- , it "should not accept generic property values"
    --   [ (lighten 0.8 initial) testColorExtractor  
    --       `shouldEqual` (stringValue "should not compile")
    --   ]
    ]
  , describe "darken"
    [ it "darkens an RGB color by a factor proportional to its distance from black"
      [ (darken 0.2 (rgba 25 210 130 1.0)) testColorExtractor
          `shouldEqual` (stringValue "r:20, g:168, b:104, a:1") ]
    , it "does not darken an RGB color past black"
      [ (darken 0.8 (rgba 25 210 130 1.0)) testColorExtractor
          `shouldEqual` (stringValue "r:5, g:42, b:26, a:1") ]
    , it "darkens an HSL color by a factor proportional to its distance from black"
      [ (darken 0.2 (hsla 154 0.79 0.46 1.0)) testColorExtractor
          `shouldEqual` (stringValue "h:154, s:79%, l:37%, a:1") ]
    , it "does not darken an HSL color past black"
      [ (darken 0.8 (hsla 154 0.79 0.46 1.0)) testColorExtractor
          `shouldEqual` (stringValue "h:154, s:79%, l:9%, a:1") ]
    -- , it "should not accept preset non-colors"
    --   [ (darken 0.8 transparent) testColorExtractor  
    --       `shouldEqual` (stringValue "should not compile")
    --   , (darken 0.8 currentColor) testColorExtractor
    --       `shouldEqual` (stringValue "should not compile")
    --   ]
    -- , it "should not accept generic property values"
    --   [ (darken 0.8 initial) testColorExtractor  
    --       `shouldEqual` (stringValue "should not compile")
    --   ]          
    ]    
  ]
-- TODO test that you can compose lighten and darken

testColorExtractor : { rgbaColor : ElmColor.Color -> Value
                     , hslaColor : ElmColor.Color -> Value
                     , namedRgba: String -> ElmColor.Color -> Value
                     , currentColor : Value
                     , invalid_ : String -> Value
                     , other_ : Value -> Value
                     }
testColorExtractor =
  { rgbaColor elmColor = 
      let color = ElmColor.toRgb elmColor
          colorString = "r:" ++ (toString color.red) ++ 
                          ", g:" ++ (toString color.green) ++ 
                          ", b:" ++ (toString color.blue) ++ 
                          ", a:" ++ (toString color.alpha)
      in stringValue colorString
  , hslaColor elmColor =
      let toDegrees rad = rad * 180 / pi |> Utils.toFixed 0
          color = ElmColor.toHsl elmColor
          colorString = "h:" ++ (color.hue |> toDegrees |> toString) ++ 
                          ", s:" ++ (color.saturation |> Utils.percentStr) ++ 
                          ", l:" ++ (color.lightness  |> Utils.percentStr) ++ 
                          ", a:" ++ (toString color.alpha)
      in stringValue colorString
  , namedRgba name elmColor = stringValue name
  , currentColor = stringValue "currentColor"
  , invalid_ str = stringValue str
  , other_ val = otherValue val
  } 
