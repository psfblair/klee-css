module Css.Internal.Color
  ( ManipulableColorDescriptor
  , NubColorDescriptor, nubColorFactory
  , ColorDescriptor, colorFactory
  , NubColorDescriptorWithInvert, nubColorFactoryWithInvert
  , ColorDescriptorWithInvert, colorFactoryWithInvert
  , rgbaString, hslaString, invalidRgb, invalidHsl
  , ColorHolder, descriptorLerp, lerp, lighten, darken
  ) where

import String
import Color as ElmColor

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------
type alias ManipulableColorDescriptor rec resultType = 
  ManipulableColorFactory rec resultType -> resultType
  
type alias NubColorDescriptor rec = 
  NubColorFactory rec -> Property.Value

type alias ColorDescriptor = ColorFactory {} -> Property.Value

type alias NubColorDescriptorWithInvert rec =
  NubColorFactory (WithInvert rec) -> Property.Value

type alias ColorDescriptorWithInvert rec =
  ColorFactory (WithInvert rec) -> Property.Value

-------------------------------------------------------------------------------
-- Colors that can be lightened, darkened, etc.
type alias ManipulableColorFactory rec resultType =
  { rec | rgbaColor : ElmColor.Color -> resultType
        , hslaColor : ElmColor.Color -> resultType
        , namedRgba : String -> ElmColor.Color -> resultType
        , invalid_ : String -> resultType
  }

-- TODO Create Property.invalidValue to spit out validation errors.
manipulableColorFactory : ManipulableColorFactory {} Property.Value
manipulableColorFactory =
  { rgbaColor color = rgbaString color |> Property.stringValue
  , hslaColor color = hslaString color |> Property.stringValue
  , namedRgba name color = Property.stringValue name
  , invalid_ str = Property.stringValue str
  }

type alias NubColorFactory rec =
  ManipulableColorFactory 
    { rec | currentColor : Property.Value
          , transparent : Property.Value
          , other_ : Property.Value -> Property.Value
    } 
    Property.Value
  
-- TODO Create Property.invalidValue to spit out validation errors.
nubColorFactory : NubColorFactory {}
nubColorFactory =
  let withCurrentColor = 
        { manipulableColorFactory 
            | currentColor = Property.stringValue "currentColor" }
      withTransparent = 
        { withCurrentColor 
            | transparent = Property.stringValue "transparent"}
      withOther = 
        { withTransparent | other_ = \val -> Common.otherValue val }
  in withOther

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
      h = unwrapped.hue |> \rad -> rad * 180 / pi |> Utils.toFixed 0 |> toString
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

-------------------------------------------------------------------------------
type ColorHolder
  = ValidRgbaColor ElmColor.Color
  | ValidHslaColor ElmColor.Color
  | InvalidColor String
  
colorManipulationFactory : ManipulableColorFactory {} ColorHolder 
colorManipulationFactory =  
  { rgbaColor color = color |> ValidRgbaColor
  , hslaColor color = color |> ValidHslaColor
  , namedRgba name color = color |> ValidRgbaColor
  , invalid_ str = InvalidColor str
  }
  
-- TODO Elm color gives NaN for H of black and white, and S of white; need a fix
descriptorLerp : Float -> 
                 ManipulableColorDescriptor {} ColorHolder -> 
                 ManipulableColorDescriptor {} ColorHolder -> 
                 ManipulableColorDescriptor rec resultType
descriptorLerp factor startColorDescriptor boundaryColorDescriptor =  
  let startColorHolder = startColorDescriptor colorManipulationFactory 
      boundaryHolder = boundaryColorDescriptor colorManipulationFactory
  in case (startColorHolder, boundaryHolder) of
    (InvalidColor str, _) -> \factory -> factory.invalid_ str
    (_, InvalidColor str) -> \factory -> factory.invalid_ str
    (ValidRgbaColor start, ValidRgbaColor bound) ->
      \factory -> factory.rgbaColor (lerp factor start bound)
    (ValidRgbaColor start, ValidHslaColor bound) ->
      \factory -> factory.rgbaColor (lerp factor start bound)
    (ValidHslaColor start, ValidRgbaColor bound) ->
      \factory -> factory.hslaColor (lerp factor start bound)
    (ValidHslaColor start, ValidHslaColor bound) ->
      \factory -> factory.hslaColor (lerp factor start bound)

lerp : Float -> ElmColor.Color -> ElmColor.Color -> ElmColor.Color
lerp factor startColor boundaryColor =
  let start = ElmColor.toRgb startColor
      bound = ElmColor.toRgb boundaryColor
      lerpFloats amount start bound =
        let difference = bound - start
            adjustment = difference * amount
        in start + adjustment |> Utils.toFixed 2
      lerpInts amount start bound =
        let lerped = lerpFloats amount (toFloat start) (toFloat bound)
        in lerped |> round |> clampColor
  in ElmColor.rgba (lerpInts factor start.red bound.red)
                   (lerpInts factor start.green bound.green)
                   (lerpInts factor start.blue bound.blue)
                   (lerpFloats factor start.alpha bound.alpha |> clampAlpha)
  
lighten : Float -> ElmColor.Color -> ElmColor.Color
lighten factor color = lerp factor color ElmColor.white

darken : Float -> ElmColor.Color -> ElmColor.Color
darken factor color = lerp factor color ElmColor.black

clampColor : Int -> Int
clampColor = clamp 0 255

clampAlpha : Float -> Float
clampAlpha = clamp 0.0 1.0
