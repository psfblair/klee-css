module CssTests where

import Spec exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Box exposing (borderStyle, borderColor)
import Css.ColorsAndStrokes exposing (green, solid)
import Css.Display exposing (float, floatLeft, clear, both)
import Css.FontFace exposing 
  ( FontFaceFormat(..)
  , fontFaceSrc, localFontFaceSrc, urlFontFaceSrc
  )
import Css.Font exposing (fontFamily, family)
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
  , describe "extended render"
    [ let imports = 
            [ importUrl "http://some.stylesheet.com" 
            , importUrl "http://some.stylesheet.org" 
            ]
          fontFaces = 
            [ fontFace 
              [ fontFamily (family "Bitstream Vera Serif Bold")
              , fontFaceSrc 
                [ localFontFaceSrc "VeraBold.woff"
                , urlFontFaceSrc "http://font.url" (Just TrueType)
                ]
              ]
            ]
          stylesheet =
              [ p [ borderStyle solid, borderColor green ]
                  [ a [ custom "-ms-lens-flare-style" "really-shiny" ] [] ]
              , (p `byClass` "error") 
                  [ float floatLeft ] 
                  [ (a `byId` "fred") [ clear both ] [] ]    
              ]
      in render (append imports stylesheet |> append fontFaces) 
          `shouldEqual` expectedFullStylesheet  
    ]
  ]


expectedPropertyStylesheet : String
expectedPropertyStylesheet = """
p 
{
  border-style : solid;
  border-color : #73D216;
}

p a 
{
  -ms-lens-flare-style : really-shiny;
}

p.error 
{
  float : left;
}

p.error a#fred 
{
  clear : both;
}


/* Generated with css-elm */"""

expectedFullStylesheet : String 
expectedFullStylesheet = """
@import url("http://some.stylesheet.com");
@import url("http://some.stylesheet.org");
@font-face
{
  font-family : "Bitstream Vera Serif Bold";
  src         : local("VeraBold.woff"),
      url("http://font.url") format("truetype");
}

p 
{
  border-style : solid;
  border-color : #73D216;
}

p a 
{
  -ms-lens-flare-style : really-shiny;
}

p.error 
{
  float : left;
}

p.error a#fred 
{
  clear : both;
}


/* Generated with css-elm */"""
