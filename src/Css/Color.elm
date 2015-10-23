module Css.Color
  ( CssColor (..), ColorDescriptor
  , rgb, rgba, hsl, hsla, hex
  , red, orange, yellow, green, blue, purple, brown
  , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
  , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
  , white, lightGrey, lightGray, grey, gray, darkGrey, darkGray
  , lightCharcoal, charcoal, darkCharcoal, black
    -- used by other modules
  , ColorFactory, colorFactory, colorValueFactory, rgbaString, hslaString
  ) where

import String
import Color exposing (Color)

import Css.Internal.Property exposing (Value, ValueFactory, stringValueFactory)
import Css.Internal.Utils exposing (floatMod, toFixed, toHexString, fromHex)

import Css.Common exposing 
  ( Initial, Inherit, Other
  , initialValue, inheritValue, otherValue
  )

-------------------------------------------------------------------------------
type CssColor
  = CssRgba Color 
  | CssHsla Color 
  | InitialColor
  | InheritColor
  | InvalidColor String
  | OtherColor String

type alias ColorDescriptor a = ColorFactory a -> CssColor
  
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

-------------------------------------------------------------------------------

-- These functions are not part of the DSL. They integrate the colors with the
-- rest of the Css framework.

-- We make ColorFactory extensible because there are some colors that are not 
-- generally applicable. E.g., invert for outline, or transparent for background.
type alias ColorFactory a =
  { a | rgbaColor: Color -> CssColor
      , hslaColor: Color -> CssColor
      , initial: CssColor
      , inherit: CssColor
      , invalid: String -> CssColor
      , other: String -> CssColor
  }

colorFactory : ColorFactory {}
colorFactory =
  { rgbaColor color = CssRgba color
  , hslaColor color = CssHsla color
  , initial = InitialColor
  , inherit = InheritColor
  , invalid str = InvalidColor str
  , other str = OtherColor str
  }

colorValueFactory : ValueFactory CssColor
colorValueFactory =
  { value cssColor =
      case cssColor of 
        CssRgba color ->  rgbaString color |> stringValueFactory.value
        CssHsla color ->  hslaString color |> stringValueFactory.value
        InitialColor -> initialValue
        InheritColor -> inheritValue
        OtherColor str -> otherValue str
  }

rgbaString : Color -> String
rgbaString color =
  let unwrapped = Color.toRgb color
      fixedStr num = toFixed 2 num |> toString
  in 
    if unwrapped.alpha == 1.0
    then 
      let hexRed = toHexString 2 unwrapped.red 
          hexGreen = toHexString 2 unwrapped.green
          hexBlue = toHexString 2 unwrapped.blue
      in String.join "" ["#", hexRed, hexGreen, hexBlue] 
    else
      let red = toString unwrapped.red
          green = toString unwrapped.green
          blue = toString unwrapped.blue
          alpha = fixedStr unwrapped.alpha
          valueFactory = stringValueFactory
      in String.join "" ["rgba(", red, ",", green, ",", blue, ",", alpha, ")"]

hslaString : Color -> String
hslaString color =
  let unwrapped = Color.toHsl color
      percentStr num = num * 100 |> toFixed 0 |> toString |> \x -> x ++ "%"
      h = unwrapped.hue |> \deg -> deg * 180 / pi |> toFixed 0 |> toString
      s = unwrapped.saturation |> percentStr 
      l = unwrapped.lightness |> percentStr 
  in 
    if unwrapped.alpha == 1.0
    then String.join "" ["hsl(", h, ",", s, ",", l, ")"]
    else
      let a = unwrapped.alpha |> toFixed 2 |> toString 
      in String.join "" ["hsla(", h, ",", s, ",", l, ",", a, ")"]

invalidRgb : Int -> Int -> Int -> Bool
invalidRgb r g b = 
  let invalidComponent num = num > 255 || num < 0
  in invalidComponent r || invalidComponent g || invalidComponent b

invalidHsl : Int -> Float -> Float -> Bool
invalidHsl h s l =
  let invalidHue num = num > 360 || num < 0      
  in invalidHue h || invalidFractionOf1 s || invalidFractionOf1 l
  
invalidFractionOf1 : Float -> Bool
invalidFractionOf1 num = num > 1.0 || num < 0

join : List String -> String
join strings = String.join "," strings
