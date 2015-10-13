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
            , (p `byClass` "error") [ float floatLeft ] 
                                  [ (a `byId` "fred") [ clear both ] [] ]    
            ]
      in render stylesheet `shouldEqual` expectedPropertyStylesheet
    ]
  -- , describe "renderExtended"
  -- Ultimately the following or something like it needs to work.
  --           , importUrl "http://some.stylesheet.com"

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
  border-color : rgba(0,128,0,1);
  border-style : solid;
}

p a 
{
  -ms-lens-flare-style : really-shiny;
}


/* Generated with elm-css */"""
