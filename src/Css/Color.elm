module Css.Color
  ( rgb, rgba, hsl, hsla, hex
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
-------------------------------------------------------------------------------
  
rgb : Int -> Int -> Int -> ColorDescriptor a
rgb r g b factory = 
  if invalidRgb r g b
  then factory.invalid 
        ("INVALID COLOR: " ++ join [toString r, toString g, toString b])
  else Color.rgb r g b |> factory.rgbaColor

rgba : Int -> Int -> Int -> Float -> ColorDescriptor a
rgba r g b a factory = 
  if invalidRgb r g b || invalidFractionOf1 a
  then factory.invalid 
        ("INVALID COLOR: " ++ join [toString r, toString g, toString b, toString a])
  else Color.rgba r g b a |> factory.rgbaColor

hsl : Int -> Float -> Float -> ColorDescriptor a
hsl h s l factory = 
  if invalidHsl h s l
  then factory.invalid 
        ("INVALID COLOR: " ++ join [toString h, toString s, toString l])
  else Color.hsl (toFloat h |> degrees) s l |> factory.hslaColor

hsla : Int -> Float -> Float -> Float -> ColorDescriptor a
hsla h s l a factory = 
  if invalidHsl h s l || invalidFractionOf1 a
  then factory.invalid 
        ("INVALID COLOR: " ++ join [toString h, toString s, toString l, toString a])
  else Color.hsla (toFloat h |> degrees) s l a |> factory.hslaColor

hex : String -> ColorDescriptor a
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
    Err str -> factory.invalid str

-- TODO
-- background-color needs transparent as a special color descriptor that yields "transparent"

red : ColorDescriptor a
red factory = Color.red |> factory.rgbaColor

orange : ColorDescriptor a
orange factory = Color.orange |> factory.rgbaColor

yellow : ColorDescriptor a
yellow factory = Color.yellow |> factory.rgbaColor

green : ColorDescriptor a
green factory = Color.green |> factory.rgbaColor

blue : ColorDescriptor a
blue factory = Color.blue |> factory.rgbaColor

purple : ColorDescriptor a
purple factory = Color.purple |> factory.rgbaColor

brown : ColorDescriptor a
brown factory = Color.brown |> factory.rgbaColor



lightRed : ColorDescriptor a
lightRed factory = Color.lightRed |> factory.rgbaColor

lightOrange : ColorDescriptor a
lightOrange factory = Color.lightOrange |> factory.rgbaColor

lightYellow : ColorDescriptor a
lightYellow factory = Color.lightYellow |> factory.rgbaColor

lightGreen : ColorDescriptor a
lightGreen factory = Color.lightGreen |> factory.rgbaColor

lightBlue : ColorDescriptor a
lightBlue factory = Color.lightBlue |> factory.rgbaColor

lightPurple : ColorDescriptor a
lightPurple factory = Color.lightPurple |> factory.rgbaColor

lightBrown : ColorDescriptor a
lightBrown factory = Color.lightBrown |> factory.rgbaColor



darkRed : ColorDescriptor a
darkRed factory = Color.darkRed |> factory.rgbaColor

darkOrange : ColorDescriptor a
darkOrange factory = Color.darkOrange |> factory.rgbaColor

darkYellow : ColorDescriptor a
darkYellow factory = Color.darkYellow |> factory.rgbaColor

darkGreen : ColorDescriptor a
darkGreen factory = Color.darkGreen |> factory.rgbaColor

darkBlue : ColorDescriptor a
darkBlue factory = Color.darkBlue |> factory.rgbaColor

darkPurple : ColorDescriptor a
darkPurple factory = Color.darkPurple |> factory.rgbaColor

darkBrown : ColorDescriptor a
darkBrown factory = Color.darkBrown |> factory.rgbaColor



white : ColorDescriptor a
white factory = Color.white |> factory.rgbaColor

lightGrey : ColorDescriptor a
lightGrey factory = Color.lightGrey |> factory.rgbaColor

lightGray : ColorDescriptor a
lightGray factory = Color.lightGray |> factory.rgbaColor

grey : ColorDescriptor a
grey factory = Color.grey |> factory.rgbaColor

gray : ColorDescriptor a
gray factory = Color.gray |> factory.rgbaColor

darkGrey : ColorDescriptor a
darkGrey factory = Color.darkGrey |> factory.rgbaColor

darkGray : ColorDescriptor a
darkGray factory = Color.darkGray |> factory.rgbaColor

lightCharcoal : ColorDescriptor a 
lightCharcoal factory = Color.lightCharcoal |> factory.rgbaColor

charcoal : ColorDescriptor a
charcoal factory = Color.charcoal |> factory.rgbaColor

darkCharcoal : ColorDescriptor a
darkCharcoal factory = Color.darkCharcoal |> factory.rgbaColor

black : ColorDescriptor a
black factory = Color.black |> factory.rgbaColor
