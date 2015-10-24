module Css.Internal.Color
  ( CssColor (..), ColorDescriptor, ColorFactory
  , colorFactory, colorValueFactory
  , rgbaString, hslaString, invalidRgb, invalidHsl
  , invalidFractionOf1, join
  ) where

import String
import Color exposing (Color)

import Css.Internal.Common exposing (initialValue, inheritValue, otherValue)
import Css.Internal.Property exposing (Value, ValueFactory, stringValueFactory)
import Css.Internal.Utils exposing (toFixed, toHexString)

-------------------------------------------------------------------------------

type alias ColorDescriptor a = ColorFactory a -> CssColor

type CssColor
  = CssRgba Color 
  | CssHsla Color 
  | InitialColor
  | InheritColor
  | InvalidColor String
  | OtherColor String

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
