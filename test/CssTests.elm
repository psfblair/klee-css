module CssTests where

import Spec exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Border exposing (borderStyle, borderColor, solid)
import Css.Color exposing (green)
import Css.Display exposing (float, floatLeft, clear, both)
-------------------------------------------------------------------------------

suite : Spec
suite = describe "CssTests"
  [ describe "render"
    [ let stylesheet =
        [ p [ borderStyle solid, borderColor green ]
            [ a [ custom "-ms-lens-flare-style" "really-shiny" ] [] ]
        , (p `byClass` "error") 
            [ float floatLeft ] 
            [ (a `byId` "fred") [ clear both ] [] ]    
        ]
      in render stylesheet `shouldEqual` expectedPropertyStylesheet
    ]
  -- , describe "renderExtended"
  --   [ let stylesheet = 
  --       { imports = 
  --           [ importUrl "http://some.stylesheet.com" 
  --           , importUrl "http://some.stylesheet.org" 
  --           ]
  --       , fontFaces = 
  --           [ fontFace [ fontFamily: "face"
  --                      , fontFaceSrc [FontFaceSrcLocal "face.woff"] 
  --                      ]
  --           , fontFace [ fontFaceSrc 
  --                         [ FontFaceSrcLocal "face.woff"]
  --                         , FontFaceSrcUrl "http://font.url" (Just TrueType)
  --                         ]
  --           ]
  --       }
  --     in renderExtended stylesheet `shouldEqual` expectedExtendedStylesheet  
  -- ]
  ]



expectedPropertyStylesheet : String
expectedPropertyStylesheet = """
p.error 
{
  float : left;
}

p.error a#fred 
{
  clear : both;
}

p 
{
  border-color : #73D216;
  border-style : solid;
}

p a 
{
  -ms-lens-flare-style : really-shiny;
}


/* Generated with elm-css */"""

expectedExtendedStylesheet : String 
expectedExtendedStylesheet = """
@import url("http://some.stylesheet.com")

@import url("http://some.stylesheet.org")

@font-face 
{
  font-family: "Bitstream Vera Serif Bold";
  src: url("https://mdn.mozillademos.org/files/2468/VeraSeBd.ttf");
}


 /* Generated with elm-css */"""
