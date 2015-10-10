module Css.Color
  ( Color (..)
  , rgb, rgba, hsl, hsla, parse
  , toRgba, toHsla, setR, setG, setB, setA
  , clampColor, clampAlpha
  , (*.), (+.), (-.)
  , lighten, darken, lerp
  , grayish, transparent
  , aliceblue, antiquewhite, aqua, aquamarine, azure, beige, bisque, black
  , blanchedalmond, blue, blueviolet, brown, burlywood, cadetblue, chartreuse
  , chocolate, coral, cornflowerblue, cornsilk, crimson, cyan, darkblue, darkcyan
  , darkgoldenrod, darkgray, darkgreen, darkgrey, darkkhaki, darkmagenta
  , darkolivegreen, darkorange, darkorchid, darkred, darksalmon, darkseagreen
  , darkslateblue, darkslategray, darkslategrey, darkturquoise, darkviolet
  , deeppink, deepskyblue, dimgray, dimgrey, dodgerblue, firebrick, floralwhite
  , forestgreen, fuchsia, gainsboro, ghostwhite, gold, goldenrod, gray, green
  , greenyellow, grey, honeydew, hotpink, indianred, indigo, ivory, khaki
  , lavender, lavenderblush, lawngreen, lemonchiffon, lightblue, lightcoral
  , lightcyan, lightgoldenrodyellow, lightgray, lightgreen, lightgrey, lightpink
  , lightsalmon, lightseagreen, lightskyblue, lightslategray, lightslategrey
  , lightsteelblue, lightyellow, lime, limegreen, linen, magenta, maroon
  , mediumaquamarine, mediumblue, mediumorchid, mediumpurple, mediumseagreen
  , mediumslateblue, mediumspringgreen, mediumturquoise, mediumvioletred
  , midnightblue, mintcream, mistyrose, moccasin, navajowhite, navy, oldlace
  , olive, olivedrab, orange, orangered, orchid, palegoldenrod, palegreen
  , paleturquoise, palevioletred, papayawhip, peachpuff, peru, pink, plum
  , powderblue, purple, red, rosybrown, royalblue, saddlebrown, salmon
  , sandybrown, seagreen, seashell, sienna, silver, skyblue, slateblue
  , slategray, slategrey, snow, springgreen, steelblue, tan, teal, thistle
  , tomato, turquoise, violet, wheat, white, whitesmoke, yellow, yellowgreen

  -- used by other modules
  , colorValueFactory
  ) where

import String

import Css.Internal.Property exposing (Value, ValueFactory, stringValueFactory)
import Css.Internal.Utils exposing (floatMod, toFixed, toHexString, fromHex)

import Css.Common exposing (None, Auto, Inherit, Other)

-------------------------------------------------------------------------------

type Color
  = Rgba Int Int Int Float
  | Hsla Int Float Float Float
  | Other Value


rgb : Int -> Int -> Int -> Color
rgb r g b = rgba r g b 1.0


rgba : Int -> Int -> Int -> Float -> Color
rgba = Rgba


hsl : Int -> Float -> Float -> Color
hsl r g b = hsla r g b 1.0


hsla : Int -> Float -> Float -> Float -> Color
hsla = Hsla


parse : String -> Result String Color
parse str =
  let hex digit1 digit2 = fromHex <| String.fromList [digit1, digit2]
      toAlpha hexResult = hexResult |> Result.map toFloat |> Result.map ((/) 255.0)
      err = "Invalid color string" |> Err
      digits =
        case String.uncons str of
          Just ('#', cs) -> cs
          _ -> str
  in case String.toList digits of
      -- Hex alpha is in CSS 4
      [a, b, c, d, e, f, g, h] -> Result.map4 rgba (hex a b) (hex c d) (hex e f) (hex g h |> toAlpha)
      [a, b, c, d, e, f      ] -> Result.map3 rgb  (hex a b) (hex c d) (hex e f)
      [a, b, c, d            ] -> Result.map4 rgba (hex a a) (hex b b) (hex c c) (hex d d |> toAlpha)
      [a, b, c               ] -> Result.map3 rgb  (hex a a) (hex b b) (hex c c)
      _                        -> err


-- * Color conversions.

toRgba : Color -> Color
toRgba color =
    case color of
        Rgba _ _ _ _ as color -> color
        Other color as color -> color
        Hsla h s l a ->
              let sextant = toFloat h / 60.0
                  chroma = 2.0 * l |> (+) -1.0 |> abs |> \x -> 1.0 - x |>  (*) s
                  x = sextant `floatMod` 2 |> (+) -1.0 |> abs |> \x -> 1.0 - x |> (*) chroma
                  lightnessAdjustment = l - (chroma / 2.0)

                  toRgbPart component = component + lightnessAdjustment |> (*) 255.0 |> truncate
                  toRgba (r, g, b) = Rgba (toRgbPart r) (toRgbPart g) (toRgbPart b)

                  rgb' =
                    if | h >= 0   && h <  60 -> (chroma, x     ,  0)
                       | h >= 60  && h < 120 -> (x     , chroma,  0)
                       | h >= 120 && h < 180 -> (0     , chroma,  x)
                       | h >= 180 && h < 240 -> (0     , x     ,  chroma)
                       | h >= 240 && h < 300 -> (x     , 0     ,  chroma)
                       | otherwise           -> (chroma, 0     ,  x)
               in toRgba rgb' a


toHsla : Color -> Color
toHsla color =
    case color of
        Hsla _ _ _ _ as color -> color
        Other color as color -> color
        Rgba red green blue alpha ->
            let r = toFloat red   / 255.0
                g = toFloat green / 255.0
                b = toFloat blue  / 255.0

                min = List.minimum [r, g, b] |> Maybe.withDefault 255
                max = List.maximum [r, g, b] |> Maybe.withDefault 255
                delta = max - min

                l = (min + max) / 2.0
                s = if delta == 0.0 then 0.0
                    else 2.0 * l |> (+) -1.0 |> abs |> \x -> 1.0 - x |> (\x -> delta / x)

                h' =
                  if | delta == 0.0 -> 0.0
                     | r == max     -> ((g - b) / delta) `floatMod` 6.0
                     | g == max     -> ((b - r) / delta) + 2.0
                     | otherwise    -> ((r - g) / delta) + 4.0

                h'' = 60 * h' |> truncate
                h = if h'' < 0 then h''+ 360 else h''
            in Hsla h (toFixed 3 s) (toFixed 3 l) alpha

-- * Setting individual color components.

setR : Int -> Color -> Color
setR redToSet col =
  case col of
     Rgba _ g b a -> Rgba (clampColor redToSet) g b a
     (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setR redToSet |> toHsla
     _ as other -> other


setG : Int -> Color -> Color
setG greenToSet col =
  case col of
    (Rgba r _ b a) -> Rgba r (clampColor greenToSet) b a
    (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setG greenToSet |> toHsla
    _ as other -> other


setB : Int -> Color -> Color
setB blueToSet col =
  case col of
    (Rgba r g _ a) -> Rgba r g (clampColor blueToSet) a
    (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setB blueToSet |> toHsla
    _ as other -> other


setA : Float -> Color -> Color
setA alphaToSet col =
  case col of
    (Rgba r g b _) -> Rgba r g b (clampAlpha alphaToSet)
    (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setA alphaToSet |> toHsla
    _ as other -> other

-- * Computing with colors.

clampColor : Int -> Int
clampColor = clamp 0 255

clampAlpha : Float -> Float
clampAlpha = clamp 0.0 1.0

(*.) : Color -> Float -> Color
(*.) col factor =
  let multiplyNormalize val = toFloat val |> (*) factor |> truncate |> clampColor
  in case col of
    (Rgba r g b a) ->
      rgba (multiplyNormalize r) (multiplyNormalize g) (multiplyNormalize b) a
    (Hsla _ _ _ _) as hslaColor ->
      hslaColor |> toRgba |> \c -> (*.) c factor |> toHsla
    _ as other ->
      other

(+.) : Color -> Int -> Color
(+.) col increment =
  case col of
    (Rgba r g b a) ->
      rgba (clampColor (r + increment)) (clampColor (g + increment)) (clampColor (b + increment)) a
    (Hsla _ _ _ _) as hslaColor ->
      hslaColor |> toRgba |> \c -> (+.) c increment |> toHsla
    _ as other ->
      other

(-.) : Color -> Int -> Color
(-.) col decrement =
  case col of
    (Rgba r g b a) ->
      rgba (clampColor (r - decrement)) (clampColor (g - decrement)) (clampColor (b - decrement)) a
    (Hsla _ _ _ _) as hslaColor ->
      hslaColor |> toRgba |> \c -> (-.) c decrement |> toHsla
    _ as other ->
      other

lighten : Float -> Color -> Color
lighten factor color =
    case color of
        (Hsla _ _ _ _) as color -> lighten factor (toRgba color) |> toHsla
        (Rgba _ _ _ _) as color -> lerp factor color (Rgba 255 255 255 1.0)

darken : Float -> Color -> Color
darken factor color =
    case color of
        (Hsla _ _ _ _) as color -> darken factor (toRgba color) |> toHsla
        (Rgba _ _ _ _) as color -> lerp factor color (Rgba 0 0 0 1.0)

lerp : Float -> Color -> Color -> Color
lerp factor startColor boundColor =
    case (startColor, boundColor) of
        ((Hsla _ _ _ _), bound) -> lerp factor (toRgba startColor) bound |> toHsla

        (start, (Hsla _ _ _ _)) -> lerp factor start (toRgba boundColor) |> toHsla

        (Rgba r g b a, Rgba r' g' b' a') ->
            let lerpFloats amount start bound =
                  let difference = bound - start
                      adjustment = difference * amount
                  in start + adjustment |> toFixed 2
                lerpInts amount start bound =
                  lerpFloats amount (toFloat start) (toFloat bound) |> round |> clampColor
            in Rgba (lerpInts factor r r')
                    (lerpInts factor g g')
                    (lerpInts factor b b')
                    (lerpFloats factor a a' |> clampAlpha)

-------------------------------------------------------------------------------

grayish : Int -> Color
grayish g = rgb g g g

-- * List of color values by name.

transparent          : Color
transparent          = rgba 0 0 0 0

aliceblue            : Color
aliceblue            = rgb 240 248 255

antiquewhite         : Color
antiquewhite         = rgb 250 235 215

aqua                 : Color
aqua                 = rgb   0 255 255

aquamarine           : Color
aquamarine           = rgb 127 255 212

azure                : Color
azure                = rgb 240 255 255

beige                : Color
beige                = rgb 245 245 220

bisque               : Color
bisque               = rgb 255 228 196

black                : Color
black                = rgb   0   0   0

blanchedalmond       : Color
blanchedalmond       = rgb 255 235 205

blue                 : Color
blue                 = rgb   0   0 255

blueviolet           : Color
blueviolet           = rgb 138  43 226

brown                : Color
brown                = rgb 165  42  42

burlywood            : Color
burlywood            = rgb 222 184 135

cadetblue            : Color
cadetblue            = rgb  95 158 160

chartreuse           : Color
chartreuse           = rgb 127 255   0

chocolate            : Color
chocolate            = rgb 210 105  30

coral                : Color
coral                = rgb 255 127  80

cornflowerblue       : Color
cornflowerblue       = rgb 100 149 237

cornsilk             : Color
cornsilk             = rgb 255 248 220

crimson              : Color
crimson              = rgb 220  20  60

cyan                 : Color
cyan                 = rgb   0 255 255

darkblue             : Color
darkblue             = rgb   0   0 139

darkcyan             : Color
darkcyan             = rgb   0 139 139

darkgoldenrod        : Color
darkgoldenrod        = rgb 184 134  11

darkgray             : Color
darkgray             = rgb 169 169 169

darkgreen            : Color
darkgreen            = rgb   0 100   0

darkgrey             : Color
darkgrey             = rgb 169 169 169

darkkhaki            : Color
darkkhaki            = rgb 189 183 107

darkmagenta          : Color
darkmagenta          = rgb 139   0 139

darkolivegreen       : Color
darkolivegreen       = rgb  85 107  47

darkorange           : Color
darkorange           = rgb 255 140   0

darkorchid           : Color
darkorchid           = rgb 153  50 204

darkred              : Color
darkred              = rgb 139   0   0

darksalmon           : Color
darksalmon           = rgb 233 150 122

darkseagreen         : Color
darkseagreen         = rgb 143 188 143

darkslateblue        : Color
darkslateblue        = rgb  72  61 139

darkslategray        : Color
darkslategray        = rgb  47  79  79

darkslategrey        : Color
darkslategrey        = rgb  47  79  79

darkturquoise        : Color
darkturquoise        = rgb   0 206 209

darkviolet           : Color
darkviolet           = rgb 148   0 211

deeppink             : Color
deeppink             = rgb 255  20 147

deepskyblue          : Color
deepskyblue          = rgb   0 191 255

dimgray              : Color
dimgray              = rgb 105 105 105

dimgrey              : Color
dimgrey              = rgb 105 105 105

dodgerblue           : Color
dodgerblue           = rgb  30 144 255

firebrick            : Color
firebrick            = rgb 178  34  34

floralwhite          : Color
floralwhite          = rgb 255 250 240

forestgreen          : Color
forestgreen          = rgb 34  139  34

fuchsia              : Color
fuchsia              = rgb 255   0 255

gainsboro            : Color
gainsboro            = rgb 220 220 220

ghostwhite           : Color
ghostwhite           = rgb 248 248 255

gold                 : Color
gold                 = rgb 255 215   0

goldenrod            : Color
goldenrod            = rgb 218 165  32

gray                 : Color
gray                 = rgb 128 128 128

green                : Color
green                = rgb   0 128   0

greenyellow          : Color
greenyellow          = rgb 173 255  47

grey                 : Color
grey                 = rgb 128 128 128

honeydew             : Color
honeydew             = rgb 240 255 240

hotpink              : Color
hotpink              = rgb 255 105 180

indianred            : Color
indianred            = rgb 205  92  92

indigo               : Color
indigo               = rgb 75    0 130

ivory                : Color
ivory                = rgb 255 255 240

khaki                : Color
khaki                = rgb 240 230 140

lavender             : Color
lavender             = rgb 230 230 250

lavenderblush        : Color
lavenderblush        = rgb 255 240 245

lawngreen            : Color
lawngreen            = rgb 124 252   0

lemonchiffon         : Color
lemonchiffon         = rgb 255 250 205

lightblue            : Color
lightblue            = rgb 173 216 230

lightcoral           : Color
lightcoral           = rgb 240 128 128

lightcyan            : Color
lightcyan            = rgb 224 255 255

lightgoldenrodyellow : Color
lightgoldenrodyellow = rgb 250 250 210

lightgray            : Color
lightgray            = rgb 211 211 211

lightgreen           : Color
lightgreen           = rgb 144 238 144

lightgrey            : Color
lightgrey            = rgb 211 211 211

lightpink            : Color
lightpink            = rgb 255 182 193

lightsalmon          : Color
lightsalmon          = rgb 255 160 122

lightseagreen        : Color
lightseagreen        = rgb  32 178 170

lightskyblue         : Color
lightskyblue         = rgb 135 206 250

lightslategray       : Color
lightslategray       = rgb 119 136 153

lightslategrey       : Color
lightslategrey       = rgb 119 136 153

lightsteelblue       : Color
lightsteelblue       = rgb 176 196 222

lightyellow          : Color
lightyellow          = rgb 255 255 224

lime                 : Color
lime                 = rgb   0 255   0

limegreen            : Color
limegreen            = rgb  50 205  50

linen                : Color
linen                = rgb 250 240 230

magenta              : Color
magenta              = rgb 255   0 255

maroon               : Color
maroon               = rgb 128   0   0

mediumaquamarine     : Color
mediumaquamarine     = rgb 102 205 170

mediumblue           : Color
mediumblue           = rgb   0   0 205

mediumorchid         : Color
mediumorchid         = rgb 186  85 211

mediumpurple         : Color
mediumpurple         = rgb 147 112 219

mediumseagreen       : Color
mediumseagreen       = rgb  60 179 113

mediumslateblue      : Color
mediumslateblue      = rgb 123 104 238

mediumspringgreen    : Color
mediumspringgreen    = rgb   0 250 154

mediumturquoise      : Color
mediumturquoise      = rgb  72 209 204

mediumvioletred      : Color
mediumvioletred      = rgb 199  21 133

midnightblue         : Color
midnightblue         = rgb  25  25 112

mintcream            : Color
mintcream            = rgb 245 255 250

mistyrose            : Color
mistyrose            = rgb 255 228 225

moccasin             : Color
moccasin             = rgb 255 228 181

navajowhite          : Color
navajowhite          = rgb 255 222 173

navy                 : Color
navy                 = rgb   0   0 128

oldlace              : Color
oldlace              = rgb 253 245 230

olive                : Color
olive                = rgb 128 128   0

olivedrab            : Color
olivedrab            = rgb 107 142  35

orange               : Color
orange               = rgb 255 165   0

orangered            : Color
orangered            = rgb 255 69    0

orchid               : Color
orchid               = rgb 218 112 214

palegoldenrod        : Color
palegoldenrod        = rgb 238 232 170

palegreen            : Color
palegreen            = rgb 152 251 152

paleturquoise        : Color
paleturquoise        = rgb 175 238 238

palevioletred        : Color
palevioletred        = rgb 219 112 147

papayawhip           : Color
papayawhip           = rgb 255 239 213

peachpuff            : Color
peachpuff            = rgb 255 218 185

peru                 : Color
peru                 = rgb 205 133  63

pink                 : Color
pink                 = rgb 255 192 203

plum                 : Color
plum                 = rgb 221 160 221

powderblue           : Color
powderblue           = rgb 176 224 230

purple               : Color
purple               = rgb 128   0 128

red                  : Color
red                  = rgb 255   0   0

rosybrown            : Color
rosybrown            = rgb 188 143 143

royalblue            : Color
royalblue            = rgb  65 105 225

saddlebrown          : Color
saddlebrown          = rgb 139  69  19

salmon               : Color
salmon               = rgb 250 128 114

sandybrown           : Color
sandybrown           = rgb 244 164  96

seagreen             : Color
seagreen             = rgb  46 139  87

seashell             : Color
seashell             = rgb 255 245 238

sienna               : Color
sienna               = rgb 160  82  45

silver               : Color
silver               = rgb 192 192 192

skyblue              : Color
skyblue              = rgb 135 206 235

slateblue            : Color
slateblue            = rgb 106  90 205

slategray            : Color
slategray            = rgb 112 128 144

slategrey            : Color
slategrey            = rgb 112 128 144

snow                 : Color
snow                 = rgb 255 250 250

springgreen          : Color
springgreen          = rgb   0 255 127

steelblue            : Color
steelblue            = rgb  70 130 180

tan                  : Color
tan                  = rgb 210 180 140

teal                 : Color
teal                 = rgb   0 128 128

thistle              : Color
thistle              = rgb 216 191 216

tomato               : Color
tomato               = rgb 255  99  71

turquoise            : Color
turquoise            = rgb  64 224 208

violet               : Color
violet               = rgb 238 130 238

wheat                : Color
wheat                = rgb 245 222 179

white                : Color
white                = rgb 255 255 255

whitesmoke           : Color
whitesmoke           = rgb 245 245 245

yellow               : Color
yellow               = rgb 255 255   0

yellowgreen          : Color
yellowgreen          = rgb 154 205  50


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- These functions are not part of the DSL. They integrate the colors with the
-- rest of the Css framework.

colorValueFactory : ValueFactory Color
colorValueFactory =
  let fixedStr str = toFixed 4 str |> toString
      wrapped clr =
        case clr of
          Rgba r g b 255 ->
            ["#", toHexString r, toHexString g, toHexString b]
            |> String.join ""
            |> stringValueFactory.value
          Rgba r g b a ->
            ["rgba(", toString r, ",", toString g, ",", toString b, ",", fixedStr a, ")"]
            |> String.join ""
            |> stringValueFactory.value
          Hsla h s l 255 ->
            ["hsl(",  toString h, ",", fixedStr s, ",", fixedStr l,                  ")"]
            |> String.join ""
            |> stringValueFactory.value
          Hsla h s l a   ->
            ["hsla(", toString h, ",", fixedStr s, ",", fixedStr l, ",", fixedStr a, ")"]
            |> String.join ""
            |> stringValueFactory.value
          Other o        -> o
  in { value = \x -> wrapped x }

noneColorFactory : None Color
noneColorFactory  = { none  = Other <| stringValueFactory.value "none" }

autoColorFactory : Auto Color
autoColorFactory  = { auto  = Other <| stringValueFactory.value "auto" }

inheritColorFactory : Inherit Color
inheritColorFactory  = { inherit  = Other <| stringValueFactory.value "inherit"  }

otherColorFactory : Other Color
otherColorFactory  = { other  = Other }
