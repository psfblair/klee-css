module Css.Internal.Color
  ( NubColorDescriptor, nubColorFactory
  , ColorDescriptor, colorFactory
  , NubColorDescriptorWithInvert, nubColorFactoryWithInvert
  , ColorDescriptorWithInvert, colorFactoryWithInvert
  , rgbaString, hslaString, invalidRgb, invalidHsl
  ) where

import String
import Color as ElmColor

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

type alias NubColorDescriptor rec = NubColorFactory rec -> Property.Value

type alias ColorDescriptor = ColorFactory {} -> Property.Value

type alias NubColorDescriptorWithInvert rec =
  NubColorFactory (WithInvert rec) -> Property.Value

type alias ColorDescriptorWithInvert rec =
  ColorFactory (WithInvert rec) -> Property.Value

-------------------------------------------------------------------------------

type alias NubColorFactory rec =
  { rec | rgbaColor : ElmColor.Color -> Property.Value
        , hslaColor : ElmColor.Color -> Property.Value
        , named : String -> ElmColor.Color -> Property.Value
        , currentColor : Property.Value
        , invalid_ : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

-- TODO Create Property.invalidValue to spit out validation errors.
nubColorFactory : NubColorFactory {}
nubColorFactory =
  { rgbaColor color = rgbaString color |> Property.stringValue
  , hslaColor color = hslaString color |> Property.stringValue
  , named name color = Property.stringValue name
  , currentColor = Property.stringValue "currentColor"
  , invalid_ str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias ColorFactory rec = 
  NubColorFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value rec)))

colorFactory : ColorFactory {}
colorFactory = Common.addCommonValues nubColorFactory

type alias WithInvert rec = { rec | invert: Property.Value }

nubColorFactoryWithInvert : NubColorFactory (WithInvert {})
nubColorFactoryWithInvert =
  { nubColorFactory | invert = Property.stringValue "invert" }

colorFactoryWithInvert : ColorFactory (WithInvert {})
colorFactoryWithInvert =
  { colorFactory | invert = Property.stringValue "invert" }

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
  in invalidHue h || Utils.invalidFractionOf1 s || Utils.invalidFractionOf1 l
