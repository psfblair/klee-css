module Css.Color
  ( rgb, rgba, hsl, hsla, hex
  , currentColor, transparent, invert
  , red, orange, yellow, green, blue, purple, brown
  , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
  , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
  , white, lightGrey, lightGray, grey, gray, darkGrey, darkGray
  , lightCharcoal, charcoal, darkCharcoal, black
  ) where

import String 

import Color exposing (Color)
import Css.Internal.Color exposing (..)
import Css.Internal.Utils exposing (toFixed, fromHex)

import Css.Internal.Property as Property
-------------------------------------------------------------------------------

rgb : Int -> Int -> Int -> ColorDescriptor rec
rgb r g b factory = 
  if invalidRgb r g b
  then factory.invalid_ 
        ("INVALID COLOR: " ++ join [toString r, toString g, toString b])
  else Color.rgb r g b |> factory.rgbaColor

rgba : Int -> Int -> Int -> Float -> ColorDescriptor rec
rgba r g b a factory = 
  if invalidRgb r g b || invalidFractionOf1 a
  then factory.invalid_ 
        ("INVALID COLOR: " ++ join [toString r, toString g, toString b, toString a])
  else Color.rgba r g b a |> factory.rgbaColor

hsl : Int -> Float -> Float -> ColorDescriptor rec
hsl h s l factory = 
  if invalidHsl h s l
  then factory.invalid_ 
        ("INVALID COLOR: " ++ join [toString h, toString s, toString l])
  else Color.hsl (toFloat h |> degrees) s l |> factory.hslaColor

hsla : Int -> Float -> Float -> Float -> ColorDescriptor rec
hsla h s l a factory = 
  if invalidHsl h s l || invalidFractionOf1 a
  then factory.invalid_ 
        ("INVALID COLOR: " ++ join [toString h, toString s, toString l, toString a])
  else Color.hsla (toFloat h |> degrees) s l a |> factory.hslaColor

hex : String -> ColorDescriptor rec
hex str factory =
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

currentColor : ColorDescriptor rec
currentColor factory = factory.currentColor

transparent : ColorDescriptor rec
transparent factory = Property.stringValue "transparent" |> factory.other_

invert : NubColorDescriptorWithInvert rec
invert factory = factory.invert

red : ColorDescriptor rec
red factory = Color.red |> factory.rgbaColor

orange : ColorDescriptor rec
orange factory = Color.orange |> factory.rgbaColor

yellow : ColorDescriptor rec
yellow factory = Color.yellow |> factory.rgbaColor

green : ColorDescriptor rec
green factory = Color.green |> factory.rgbaColor

blue : ColorDescriptor rec
blue factory = Color.blue |> factory.rgbaColor

purple : ColorDescriptor rec
purple factory = Color.purple |> factory.rgbaColor

brown : ColorDescriptor rec
brown factory = Color.brown |> factory.rgbaColor



lightRed : ColorDescriptor rec
lightRed factory = Color.lightRed |> factory.rgbaColor

lightOrange : ColorDescriptor rec
lightOrange factory = Color.lightOrange |> factory.rgbaColor

lightYellow : ColorDescriptor rec
lightYellow factory = Color.lightYellow |> factory.rgbaColor

lightGreen : ColorDescriptor rec
lightGreen factory = Color.lightGreen |> factory.rgbaColor

lightBlue : ColorDescriptor rec
lightBlue factory = Color.lightBlue |> factory.rgbaColor

lightPurple : ColorDescriptor rec
lightPurple factory = Color.lightPurple |> factory.rgbaColor

lightBrown : ColorDescriptor rec
lightBrown factory = Color.lightBrown |> factory.rgbaColor



darkRed : ColorDescriptor rec
darkRed factory = Color.darkRed |> factory.rgbaColor

darkOrange : ColorDescriptor rec
darkOrange factory = Color.darkOrange |> factory.rgbaColor

darkYellow : ColorDescriptor rec
darkYellow factory = Color.darkYellow |> factory.rgbaColor

darkGreen : ColorDescriptor rec
darkGreen factory = Color.darkGreen |> factory.rgbaColor

darkBlue : ColorDescriptor rec
darkBlue factory = Color.darkBlue |> factory.rgbaColor

darkPurple : ColorDescriptor rec
darkPurple factory = Color.darkPurple |> factory.rgbaColor

darkBrown : ColorDescriptor rec
darkBrown factory = Color.darkBrown |> factory.rgbaColor



white : ColorDescriptor rec
white factory = Color.white |> factory.rgbaColor

lightGrey : ColorDescriptor rec
lightGrey factory = Color.lightGrey |> factory.rgbaColor

lightGray : ColorDescriptor rec
lightGray factory = Color.lightGray |> factory.rgbaColor

grey : ColorDescriptor rec
grey factory = Color.grey |> factory.rgbaColor

gray : ColorDescriptor rec
gray factory = Color.gray |> factory.rgbaColor

darkGrey : ColorDescriptor rec
darkGrey factory = Color.darkGrey |> factory.rgbaColor

darkGray : ColorDescriptor rec
darkGray factory = Color.darkGray |> factory.rgbaColor

lightCharcoal : ColorDescriptor rec 
lightCharcoal factory = Color.lightCharcoal |> factory.rgbaColor

charcoal : ColorDescriptor rec
charcoal factory = Color.charcoal |> factory.rgbaColor

darkCharcoal : ColorDescriptor rec
darkCharcoal factory = Color.darkCharcoal |> factory.rgbaColor

black : ColorDescriptor rec
black factory = Color.black |> factory.rgbaColor
