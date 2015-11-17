module Css.ColorTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Common exposing (otherValue)
import Css.Internal.Property exposing (Value, stringValue)
import Css.Internal.Utils exposing (toFixed)

import Color as ElmColor
import Css.ColorsAndStrokes exposing (..)


-------------------------------------------------------------------------------
-- We test the creation of properties from colors in the tests for 
-- those properties in other modules.

suite : Spec
suite = describe "Css.ColorTests"
  [ describe "hex"
    [ it "can parse color strings"
      [ hex "#FF6666" testColorExtractor 
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:1")
      , hex "44F" testColorExtractor 
          `shouldEqual` (stringValue "r:68, g:68, b:255, a:1")
      , hex "440220" testColorExtractor 
          `shouldEqual` (stringValue "r:68, g:2, b:32, a:1")
      -- CSS4 color strings   
      , hex "#FF666680" testColorExtractor 
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:0.5")
      , hex "#FFF8" testColorExtractor 
          `shouldEqual` (stringValue "r:255, g:255, b:255, a:0.53")
      ]
    , it "handles invalid colors"  
      [ hex "blarg" testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR STRING: blarg")
      , hex "FA" testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR STRING: FA")
      , hex "1" testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR STRING: 1")
      ]
    ]
  , describe "rgba"
    [ it "creates the proper color descriptor"
      [ rgba 255 102 102 0.5 testColorExtractor 
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:0.5")
      ]
    , it "handles invalid colors"  
      [ rgba 255 -1 102 0.5 testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR: 255,-1,102,0.5")
      , rgba 256 102 102 0.5 testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR: 256,102,102,0.5")
      , rgba 255 102 102 -0.1 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,102,102,-0.1")
      , rgba 256 102 102 1.2 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 256,102,102,1.2")
      ]
    ]
  , describe "rgb"
    [ it "creates the proper color"
      [ rgb 255 102 102 testColorExtractor 
          `shouldEqual` (stringValue "r:255, g:102, b:102, a:1")
      ]
    , it "handles invalid colors"  
      [ rgb -1 102 102 testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR: -1,102,102")
      , rgb 256 102 102 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 256,102,102")
      , rgb 255 -1 102 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,-1,102")
      , rgb 255 256 102 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,256,102")
      , rgb 255 102 -1 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,102,-1")
      , rgb 255 102 256 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 255,102,256")
      ]
    ]
  , describe "hsla"
    [ it "creates the proper color"
      [ hsla 99 0.61 0.82 0.478 testColorExtractor
          `shouldEqual` (stringValue "h:99, s:0.61, l:0.82, a:0.478")
      ]
    , it "handles invalid colors"  
      [ hsla 361 0.61 0.82 0.478 testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR: 361,0.61,0.82,0.478")
      , hsla -1 0.61 0.82 0.478 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: -1,0.61,0.82,0.478")
      , hsla 99 2.0 0.82 0.478 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,2,0.82,0.478")
      , hsla 99 -0.5 0.82 0.478 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,-0.5,0.82,0.478")
      , hsla 99 0.61 -0.1 0.478 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,-0.1,0.478")
      , hsla 99 0.61 1.1 0.478 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,1.1,0.478")
      , hsla 99 0.61 0.82 1.2 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,0.82,1.2")
      , hsla 99 0.61 0.82 -0.1 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,0.82,-0.1")
      ]
    ]
  , describe "hsl"
    [ it "creates the proper color"
      [ hsl 99 0.61 0.82 testColorExtractor
          `shouldEqual` (stringValue "h:99, s:0.61, l:0.82, a:1")
      ]
    , it "handles invalid colors"  
      [ hsl 361 0.61 0.82 testColorExtractor 
          `shouldEqual` (stringValue "INVALID COLOR: 361,0.61,0.82")
      , hsl -1 0.61 0.82 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: -1,0.61,0.82")
      , hsl 99 2.0 0.82 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,2,0.82")
      , hsl 99 -0.5 0.82 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,-0.5,0.82")
      , hsl 99 0.61 -0.1 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,-0.1")
      , hsl 99 0.61 1.1 testColorExtractor
          `shouldEqual` (stringValue "INVALID COLOR: 99,0.61,1.1")
      ]
    ]
  ]

testColorExtractor : { rgbaColor : ElmColor.Color -> Value
                 , hslaColor : ElmColor.Color -> Value
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
      let color = ElmColor.toHsl elmColor
          colorString = "h:" ++ (color.hue |> \deg -> deg * 180 / pi |> toFixed 0 |> toString) ++ 
                          ", s:" ++ (toString color.saturation) ++ 
                          ", l:" ++ (toString color.lightness) ++ 
                          ", a:" ++ (toString color.alpha)
      in stringValue colorString
  , currentColor = stringValue "currentColor"
  , invalid_ str = stringValue str
  , other_ val = otherValue val
  } 
