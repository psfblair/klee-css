module Css.ColorsAndStrokes
  (   
  -- * Stroke. Used for border-style, outline-style, text-decoration-style.
    
    solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

  , rgb, rgba, hsl, hsla, hex
  , currentColor, transparent, invert
  , red, orange, yellow, green, blue, purple, brown
  , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
  , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
  , white, lightGrey, lightGray, grey, gray, darkGrey, darkGray
  , lightCharcoal, charcoal, darkCharcoal, black
  ) where

import String 

import Color exposing (Color)
import Css.Internal.Utils exposing (toFixed, fromHex)

import Css.Internal.Color as Color
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke

-------------------------------------------------------------------------------

solid : Stroke.NubStrokeDescriptor rec
solid = \factory -> factory.stroke "solid"

dotted : Stroke.NubStrokeDescriptor rec
dotted = \factory -> factory.stroke "dotted"

dashed : Stroke.NubStrokeDescriptor rec
dashed = \factory -> factory.stroke "dashed"

double : Stroke.NubStrokeDescriptor rec
double = \factory -> factory.stroke "double"

wavy : Stroke.NubStrokeDescriptor rec
wavy = \factory -> factory.stroke "wavy"

groove : Stroke.NubBorderStrokeDescriptor rec
groove = \factory -> factory.stroke "groove"

ridge : Stroke.NubBorderStrokeDescriptor rec
ridge = \factory -> factory.stroke "ridge"

inset : Stroke.NubBorderStrokeDescriptor rec
inset = \factory -> factory.stroke "inset"

outset : Stroke.NubBorderStrokeDescriptor rec
outset = \factory -> factory.stroke "outset"

-------------------------------------------------------------------------------

rgb : Int -> Int -> Int -> Color.NubColorDescriptor rec
rgb r g b = 
  \factory -> 
    if Color.invalidRgb r g b
    then factory.invalid_ 
          ("INVALID COLOR: " ++ Color.join [toString r, toString g, toString b])
    else Color.rgb r g b |> factory.rgbaColor

rgba : Int -> Int -> Int -> Float -> Color.NubColorDescriptor rec
rgba r g b a = 
  \factory -> 
    if Color.invalidRgb r g b || Color.invalidFractionOf1 a
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            Color.join [toString r, toString g, toString b, toString a])
    else Color.rgba r g b a |> factory.rgbaColor

hsl : Int -> Float -> Float -> Color.NubColorDescriptor rec
hsl h s l = 
  \factory -> 
    if Color.invalidHsl h s l
    then factory.invalid_ 
          ("INVALID COLOR: " ++ Color.join [toString h, toString s, toString l])
    else Color.hsl (toFloat h |> degrees) s l |> factory.hslaColor

hsla : Int -> Float -> Float -> Float -> Color.NubColorDescriptor rec
hsla h s l a = 
  \factory -> 
    if Color.invalidHsl h s l || Color.invalidFractionOf1 a
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            Color.join [toString h, toString s, toString l, toString a])
    else Color.hsla (toFloat h |> degrees) s l a |> factory.hslaColor

hex : String -> Color.NubColorDescriptor rec
hex str = 
  \factory ->
    let unhex digit1 digit2 = fromHex <| String.fromList [digit1, digit2]
        toAlpha digit1 digit2 = 
          unhex digit1 digit2 
            |> Result.map toFloat 
            |> Result.map (\f -> f / 255.0)
            |> Result.map (\f -> toFixed 2 f)
        digits =
          case String.uncons str of
            Just ('#', cs) -> cs
            _ -> str
        result = 
          case String.toList digits of
            -- Hex alpha is in CSS 4
            [a, b, c, d, e, f, g, h] -> 
              Result.map4 Color.rgba (unhex a b) (unhex c d) (unhex e f) (toAlpha g h)
            [a, b, c, d, e, f      ] -> 
              Result.map3 Color.rgb  (unhex a b) (unhex c d) (unhex e f)
            [a, b, c, d            ] -> 
              Result.map4 Color.rgba (unhex a a) (unhex b b) (unhex c c) (toAlpha d d)  
            [a, b, c               ] -> 
              Result.map3 Color.rgb  (unhex a a) (unhex b b) (unhex c c)
            other                    -> Err ("INVALID COLOR STRING: " ++ str)
    in case result of
      Ok color -> factory.rgbaColor color
      Err str -> factory.invalid_ str

currentColor : Color.NubColorDescriptor rec
currentColor = \factory -> factory.currentColor

transparent : Color.NubColorDescriptor rec
transparent = \factory -> Property.stringValue "transparent" |> factory.other_

invert : Color.NubColorDescriptorWithInvert rec
invert = \factory -> factory.invert

red : Color.NubColorDescriptor rec
red = \factory -> Color.red |> factory.rgbaColor

orange : Color.NubColorDescriptor rec
orange = \factory -> Color.orange |> factory.rgbaColor

yellow : Color.NubColorDescriptor rec
yellow = \factory -> Color.yellow |> factory.rgbaColor

green : Color.NubColorDescriptor rec
green = \factory -> Color.green |> factory.rgbaColor

blue : Color.NubColorDescriptor rec
blue = \factory -> Color.blue |> factory.rgbaColor

purple : Color.NubColorDescriptor rec
purple = \factory -> Color.purple |> factory.rgbaColor

brown : Color.NubColorDescriptor rec
brown = \factory -> Color.brown |> factory.rgbaColor



lightRed : Color.NubColorDescriptor rec
lightRed = \factory -> Color.lightRed |> factory.rgbaColor

lightOrange : Color.NubColorDescriptor rec
lightOrange = \factory -> Color.lightOrange |> factory.rgbaColor

lightYellow : Color.NubColorDescriptor rec
lightYellow = \factory -> Color.lightYellow |> factory.rgbaColor

lightGreen : Color.NubColorDescriptor rec
lightGreen = \factory -> Color.lightGreen |> factory.rgbaColor

lightBlue : Color.NubColorDescriptor rec
lightBlue = \factory -> Color.lightBlue |> factory.rgbaColor

lightPurple : Color.NubColorDescriptor rec
lightPurple = \factory -> Color.lightPurple |> factory.rgbaColor

lightBrown : Color.NubColorDescriptor rec
lightBrown = \factory -> Color.lightBrown |> factory.rgbaColor



darkRed : Color.NubColorDescriptor rec
darkRed = \factory -> Color.darkRed |> factory.rgbaColor

darkOrange : Color.NubColorDescriptor rec
darkOrange = \factory -> Color.darkOrange |> factory.rgbaColor

darkYellow : Color.NubColorDescriptor rec
darkYellow = \factory -> Color.darkYellow |> factory.rgbaColor

darkGreen : Color.NubColorDescriptor rec
darkGreen = \factory -> Color.darkGreen |> factory.rgbaColor

darkBlue : Color.NubColorDescriptor rec
darkBlue = \factory -> Color.darkBlue |> factory.rgbaColor

darkPurple : Color.NubColorDescriptor rec
darkPurple = \factory -> Color.darkPurple |> factory.rgbaColor

darkBrown : Color.NubColorDescriptor rec
darkBrown = \factory -> Color.darkBrown |> factory.rgbaColor



white : Color.NubColorDescriptor rec
white = \factory -> Color.white |> factory.rgbaColor

lightGrey : Color.NubColorDescriptor rec
lightGrey = \factory -> Color.lightGrey |> factory.rgbaColor

lightGray : Color.NubColorDescriptor rec
lightGray = \factory -> Color.lightGray |> factory.rgbaColor

grey : Color.NubColorDescriptor rec
grey = \factory -> Color.grey |> factory.rgbaColor

gray : Color.NubColorDescriptor rec
gray = \factory -> Color.gray |> factory.rgbaColor

darkGrey : Color.NubColorDescriptor rec
darkGrey = \factory -> Color.darkGrey |> factory.rgbaColor

darkGray : Color.NubColorDescriptor rec
darkGray = \factory -> Color.darkGray |> factory.rgbaColor

lightCharcoal : Color.NubColorDescriptor rec 
lightCharcoal = \factory -> Color.lightCharcoal |> factory.rgbaColor

charcoal : Color.NubColorDescriptor rec
charcoal = \factory -> Color.charcoal |> factory.rgbaColor

darkCharcoal : Color.NubColorDescriptor rec
darkCharcoal = \factory -> Color.darkCharcoal |> factory.rgbaColor

black : Color.NubColorDescriptor rec
black = \factory -> Color.black |> factory.rgbaColor
