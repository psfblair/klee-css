module Css.Internal.Color
  ( CssColor (..), ColorDescriptor, ColorFactory
  , BasicColorDescriptor, BasicColorFactory
  , nubColorFactory, colorFactory, colorValue
  , rgbaString, hslaString, invalidRgb, invalidHsl
  , invalidFractionOf1, join
  ) where

import String
import Color as ElmColor

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

type alias ColorDescriptor rec = ColorFactory rec -> CssColor

type alias BasicColorDescriptor = BasicColorFactory {} -> CssColor

type CssColor
  = CssRgba ElmColor.Color 
  | CssHsla ElmColor.Color 
  | InitialColor
  | InheritColor
  | UnsetColor
  | InvalidColor String
  | OtherColor Property.Value

-- We make ColorFactory extensible because there are some colors that are not 
-- generally applicable. E.g., invert for outline, or transparent for background.
type alias ColorFactory rec =
  { rec | rgbaColor: ElmColor.Color -> CssColor
        , hslaColor: ElmColor.Color -> CssColor
        , invalid_ : String -> CssColor
        , other_ : Property.Value -> CssColor
  }

nubColorFactory : ColorFactory {}
nubColorFactory =
  { rgbaColor color = CssRgba color
  , hslaColor color = CssHsla color
  , invalid_ str = InvalidColor str
  , other_ val = OtherColor val
  }

type alias BasicColorFactory rec = 
  ColorFactory 
    (Common.Initial CssColor
      (Common.Inherit CssColor
        (Common.Unset CssColor rec)))

colorFactory : BasicColorFactory {}
colorFactory =
  let withInitial = { nubColorFactory | initial_ = InitialColor }
      withInherit = { withInitial    | inherit_ = InheritColor }
  in { withInherit | unset_ = UnsetColor }

colorValue : CssColor -> Property.Value
colorValue cssColor =
  case cssColor of 
    CssRgba color ->  rgbaString color |> Property.stringValue
    CssHsla color ->  hslaString color |> Property.stringValue
    InitialColor -> Common.initialValue
    InheritColor -> Common.inheritValue
    UnsetColor -> Common.unsetValue
    OtherColor val -> Common.otherValue val

rgbaString : ElmColor.Color -> String
rgbaString color =
  let unwrapped = ElmColor.toRgb color
      fixedStr num = Utils.toFixed 2 num |> toString
  in 
    if unwrapped.alpha == 1.0
    then 
      let hexRed = Utils.toHexString 2 unwrapped.red 
          hexGreen = Utils.toHexString 2 unwrapped.green
          hexBlue = Utils.toHexString 2 unwrapped.blue
      in String.join "" ["#", hexRed, hexGreen, hexBlue] 
    else
      let red = toString unwrapped.red
          green = toString unwrapped.green
          blue = toString unwrapped.blue
          alpha = fixedStr unwrapped.alpha
      in String.join "" ["rgba(", red, ",", green, ",", blue, ",", alpha, ")"]

hslaString : ElmColor.Color -> String
hslaString color =
  let unwrapped = ElmColor.toHsl color
      percentStr num = num * 100 |> Utils.toFixed 0 |> toString |> \x -> x ++ "%"
      h = unwrapped.hue |> \deg -> deg * 180 / pi |> Utils.toFixed 0 |> toString
      s = unwrapped.saturation |> percentStr 
      l = unwrapped.lightness |> percentStr 
  in 
    if unwrapped.alpha == 1.0
    then String.join "" ["hsl(", h, ",", s, ",", l, ")"]
    else
      let a = unwrapped.alpha |> Utils.toFixed 2 |> toString 
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
