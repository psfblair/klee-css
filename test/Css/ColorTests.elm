module Css.ColorTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Property exposing (Value, plain, stringValueFactory)

import Css.Color exposing (..)
import Color as ElmColor

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.ColorTests"
  [ describe "hex"
    [ it "can parse color strings"
      [ hex "#FF6666" colorFactory 
          `shouldEqual` CssRgba (ElmColor.rgba 255 102 102 1.0)
      , hex "44F" colorFactory 
          `shouldEqual` CssRgba (ElmColor.rgba 68 68 255 1.0)
      , hex "440220" colorFactory 
          `shouldEqual` CssRgba (ElmColor.rgba 68 2 32 1.0)
      -- CSS4 color strings   
      , hex "#FF666680" colorFactory 
          `shouldEqual` CssRgba (ElmColor.rgba 255 102 102 0.5)
      , hex "#FFF8" colorFactory 
          `shouldEqual` CssRgba (ElmColor.rgba 255 255 255 0.53)
      ]
    , it "handles invalid colors"  
      [ hex "blarg" colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR STRING: blarg"
      , hex "FA" colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR STRING: FA"
      , hex "1" colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR STRING: 1"
      ]
    ]
  , describe "rgba"
    [ it "creates the proper color descriptor"
      [ rgba 255 102 102 0.5 colorFactory 
          `shouldEqual` CssRgba (ElmColor.rgba 255 102 102 0.5)
      ]
    , it "handles invalid colors"  
      [ rgba 255 -1 102 0.5 colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR: 255,-1,102,0.5"
      , rgba 256 102 102 0.5 colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR: 256,102,102,0.5"
      , rgba 255 102 102 -0.1 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 255,102,102,-0.1"
      , rgba 256 102 102 1.2 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 256,102,102,1.2"
      ]
    ]
  , describe "rgb"
    [ it "creates the proper color"
      [ rgb 255 102 102 colorFactory 
          `shouldEqual` CssRgba (ElmColor.rgba 255 102 102 1.0)
      ]
    , it "handles invalid colors"  
      [ rgb -1 102 102 colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR: -1,102,102"
      , rgb 256 102 102 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 256,102,102"
      , rgb 255 -1 102 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 255,-1,102"
      , rgb 255 256 102 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 255,256,102"
      , rgb 255 102 -1 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 255,102,-1"
      , rgb 255 102 256 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 255,102,256"
      ]
    ]
  , describe "hsla"
    [ it "creates the proper color"
      [ hsla 99 0.61 0.82 0.478 colorFactory
          `shouldEqual` CssHsla (ElmColor.hsla (degrees 99.0) 0.61 0.82 0.478)
      ]
    , it "handles invalid colors"  
      [ hsla 361 0.61 0.82 0.478 colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR: 361,0.61,0.82,0.478"
      , hsla -1 0.61 0.82 0.478 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: -1,0.61,0.82,0.478"
      , hsla 99 2.0 0.82 0.478 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,2,0.82,0.478"
      , hsla 99 -0.5 0.82 0.478 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,-0.5,0.82,0.478"
      , hsla 99 0.61 -0.1 0.478 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,0.61,-0.1,0.478"
      , hsla 99 0.61 1.1 0.478 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,0.61,1.1,0.478"
      , hsla 99 0.61 0.82 1.2 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,0.61,0.82,1.2"
      , hsla 99 0.61 0.82 -0.1 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,0.61,0.82,-0.1"
      ]
    ]
  , describe "hsl"
    [ it "creates the proper color"
      [ hsl 99 0.61 0.82 colorFactory
          `shouldEqual` CssHsla (ElmColor.hsl (degrees 99.0) 0.61 0.82)
      ]
    , it "handles invalid colors"  
      [ hsl 361 0.61 0.82 colorFactory 
          `shouldEqual` InvalidColor "INVALID COLOR: 361,0.61,0.82"
      , hsl -1 0.61 0.82 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: -1,0.61,0.82"
      , hsl 99 2.0 0.82 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,2,0.82"
      , hsl 99 -0.5 0.82 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,-0.5,0.82"
      , hsl 99 0.61 -0.1 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,0.61,-0.1"
      , hsl 99 0.61 1.1 colorFactory
          `shouldEqual` InvalidColor "INVALID COLOR: 99,0.61,1.1"
      ]
    ]
  , describe "colorValueFactory"
    [ it "yields hex strings for colors specified as RGB without alpha"
      [ (rgb 255 102 102 colorFactory |> colorValueFactory.value) 
          `shouldEqual` stringValueFactory.value "#FF6666"
      ]
    , it "yields hex strings for colors specified as RGBA with 1.0 alpha"
      [ (rgba 255 102 102 1.0 colorFactory |> colorValueFactory.value) 
          `shouldEqual` stringValueFactory.value "#FF6666"
      ]
    , it "yields rgba for colors specified as rgba with less than 1.0 alpha"  
      [ (rgba 255 102 102 0.5 colorFactory |> colorValueFactory.value) 
          `shouldEqual` stringValueFactory.value "rgba(255,102,102,0.5)"
      ]
    , it "yields hsl for colors specified as HSL"  
      [ (hsl 99 0.61 0.82 colorFactory |> colorValueFactory.value) 
          `shouldEqual` stringValueFactory.value "hsl(99,61%,82%)"
      ]
    , it "yields hsl for colors specified as HSL with 1.0 alpha"  
      [ (hsla 99 0.61 0.82 1.0 colorFactory |> colorValueFactory.value) 
          `shouldEqual` stringValueFactory.value "hsl(99,61%,82%)"
      ]
    , it "yields hsla for colors specified as HSL with fractional alpha"  
      [ (hsla 99 0.61 0.82 0.478 colorFactory|> colorValueFactory.value) 
          `shouldEqual` stringValueFactory.value "hsla(99,61%,82%,0.48)"
      ]
    ]
  ]
