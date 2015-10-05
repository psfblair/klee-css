module Css.Color where

import Css.Property exposing (Value, ValueWrapper, stringValueWrapper)
import Css.Common exposing (None, Auto, Inherit, Other)
import Css.Utils exposing (floatMod, toFixed, toHexString, fromHex)

import String

type Color
  = Rgba Int Int Int Float
  | Hsla Int Float Float Float
  | Other Value


rgba : Int -> Int -> Int -> Float -> Color
rgba = Rgba


rgb : Int -> Int -> Int -> Color
rgb r g b = rgba r g b 1.0


hsla : Int -> Float -> Float -> Float -> Color
hsla = Hsla


hsl : Int -> Float -> Float -> Color
hsl r g b = hsla r g b 1.0


grayish : Int -> Color
grayish g = rgb g g g


transparent : Color
transparent = rgba 0 0 0 0

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
     Rgba _ g b a -> Rgba redToSet g b a
     (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setR redToSet |> toHsla
     _ as other -> other


setG : Int -> Color -> Color
setG greenToSet col =
  case col of
    (Rgba r _ b a) -> Rgba r greenToSet b a
    (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setG greenToSet |> toHsla
    _ as other -> other


setB : Int -> Color -> Color
setB blueToSet col =
  case col of
    (Rgba r g _ a) -> Rgba r g blueToSet a
    (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setB blueToSet |> toHsla
    _ as other -> other


setA : Float -> Color -> Color
setA alphaToSet col =
  case col of
    (Rgba r g b _) -> Rgba r g b alphaToSet
    (Hsla _ _ _ _) as hslaColor -> hslaColor |> toRgba |> setA alphaToSet |> toHsla
    _ as other -> other

-- * Computing with colors.

clampColor : Int -> Int
clampColor = clamp 0 255

clampAlpha : Float -> Float
clampAlpha = clamp 0.0 1.0

(*.) : Color -> Int -> Color
(*.) col factor =
  case col of
    (Rgba r g b a) ->
      rgba (clampColor (r * factor)) (clampColor (g * factor)) (clampColor (b * factor)) a
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
colorValueWrapper : ValueWrapper Color
colorValueWrapper =
  let fixedStr str = toFixed 4 str |> toString
      wrapped clr =
        case clr of
          Rgba r g b 255 ->
            ["#", toHexString r, toHexString g, toHexString b]
            |> String.join ""
            |> stringValueWrapper.value
          Rgba r g b a ->
            ["rgba(", toString r, ",", toString g, ",", toString b, ",", fixedStr a, ")"]
            |> String.join ""
            |> stringValueWrapper.value
          Hsla h s l 255 ->
            ["hsl(",  toString h, ",", fixedStr s, ",", fixedStr l,                  ")"]
            |> String.join ""
            |> stringValueWrapper.value
          Hsla h s l a   ->
            ["hsla(", toString h, ",", fixedStr s, ",", fixedStr l, ",", fixedStr a, ")"]
            |> String.join ""
            |> stringValueWrapper.value
          Other o        -> o
  in { value = \x -> wrapped x }

noneColorWrapper : None Color
noneColorWrapper  = { none  = Other <| stringValueWrapper.value "none" }

autoColorWrapper : Auto Color
autoColorWrapper  = { auto  = Other <| stringValueWrapper.value "auto" }

inheritColorWrapper : Inherit Color
inheritColorWrapper  = { inherit  = Other <| stringValueWrapper.value "inherit"  }

otherColorWrapper : Other Color
otherColorWrapper  = { other  = Other }

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

-------------------------------------------------------------------------------

-- * List of color values by name.

aliceblue            = rgb 240 248 255
antiquewhite         = rgb 250 235 215
aqua                 = rgb   0 255 255
aquamarine           = rgb 127 255 212
azure                = rgb 240 255 255
beige                = rgb 245 245 220
bisque               = rgb 255 228 196
black                = rgb   0   0   0
blanchedalmond       = rgb 255 235 205
blue                 = rgb   0   0 255
blueviolet           = rgb 138  43 226
brown                = rgb 165  42  42
burlywood            = rgb 222 184 135
cadetblue            = rgb  95 158 160
chartreuse           = rgb 127 255   0
chocolate            = rgb 210 105  30
coral                = rgb 255 127  80
cornflowerblue       = rgb 100 149 237
cornsilk             = rgb 255 248 220
crimson              = rgb 220  20  60
cyan                 = rgb   0 255 255
darkblue             = rgb   0   0 139
darkcyan             = rgb   0 139 139
darkgoldenrod        = rgb 184 134  11
darkgray             = rgb 169 169 169
darkgreen            = rgb   0 100   0
darkgrey             = rgb 169 169 169
darkkhaki            = rgb 189 183 107
darkmagenta          = rgb 139   0 139
darkolivegreen       = rgb  85 107  47
darkorange           = rgb 255 140   0
darkorchid           = rgb 153  50 204
darkred              = rgb 139   0   0
darksalmon           = rgb 233 150 122
darkseagreen         = rgb 143 188 143
darkslateblue        = rgb  72  61 139
darkslategray        = rgb  47  79  79
darkslategrey        = rgb  47  79  79
darkturquoise        = rgb   0 206 209
darkviolet           = rgb 148   0 211
deeppink             = rgb 255  20 147
deepskyblue          = rgb   0 191 255
dimgray              = rgb 105 105 105
dimgrey              = rgb 105 105 105
dodgerblue           = rgb  30 144 255
firebrick            = rgb 178  34  34
floralwhite          = rgb 255 250 240
forestgreen          = rgb 34  139  34
fuchsia              = rgb 255   0 255
gainsboro            = rgb 220 220 220
ghostwhite           = rgb 248 248 255
gold                 = rgb 255 215   0
goldenrod            = rgb 218 165  32
gray                 = rgb 128 128 128
green                = rgb   0 128   0
greenyellow          = rgb 173 255  47
grey                 = rgb 128 128 128
honeydew             = rgb 240 255 240
hotpink              = rgb 255 105 180
indianred            = rgb 205  92  92
indigo               = rgb 75    0 130
ivory                = rgb 255 255 240
khaki                = rgb 240 230 140
lavender             = rgb 230 230 250
lavenderblush        = rgb 255 240 245
lawngreen            = rgb 124 252   0
lemonchiffon         = rgb 255 250 205
lightblue            = rgb 173 216 230
lightcoral           = rgb 240 128 128
lightcyan            = rgb 224 255 255
lightgoldenrodyellow = rgb 250 250 210
lightgray            = rgb 211 211 211
lightgreen           = rgb 144 238 144
lightgrey            = rgb 211 211 211
lightpink            = rgb 255 182 193
lightsalmon          = rgb 255 160 122
lightseagreen        = rgb  32 178 170
lightskyblue         = rgb 135 206 250
lightslategray       = rgb 119 136 153
lightslategrey       = rgb 119 136 153
lightsteelblue       = rgb 176 196 222
lightyellow          = rgb 255 255 224
lime                 = rgb   0 255   0
limegreen            = rgb  50 205  50
linen                = rgb 250 240 230
magenta              = rgb 255   0 255
maroon               = rgb 128   0   0
mediumaquamarine     = rgb 102 205 170
mediumblue           = rgb   0   0 205
mediumorchid         = rgb 186  85 211
mediumpurple         = rgb 147 112 219
mediumseagreen       = rgb  60 179 113
mediumslateblue      = rgb 123 104 238
mediumspringgreen    = rgb   0 250 154
mediumturquoise      = rgb  72 209 204
mediumvioletred      = rgb 199  21 133
midnightblue         = rgb  25  25 112
mintcream            = rgb 245 255 250
mistyrose            = rgb 255 228 225
moccasin             = rgb 255 228 181
navajowhite          = rgb 255 222 173
navy                 = rgb   0   0 128
oldlace              = rgb 253 245 230
olive                = rgb 128 128   0
olivedrab            = rgb 107 142  35
orange               = rgb 255 165   0
orangered            = rgb 255 69    0
orchid               = rgb 218 112 214
palegoldenrod        = rgb 238 232 170
palegreen            = rgb 152 251 152
paleturquoise        = rgb 175 238 238
palevioletred        = rgb 219 112 147
papayawhip           = rgb 255 239 213
peachpuff            = rgb 255 218 185
peru                 = rgb 205 133  63
pink                 = rgb 255 192 203
plum                 = rgb 221 160 221
powderblue           = rgb 176 224 230
purple               = rgb 128   0 128
red                  = rgb 255   0   0
rosybrown            = rgb 188 143 143
royalblue            = rgb  65 105 225
saddlebrown          = rgb 139  69  19
salmon               = rgb 250 128 114
sandybrown           = rgb 244 164  96
seagreen             = rgb  46 139  87
seashell             = rgb 255 245 238
sienna               = rgb 160  82  45
silver               = rgb 192 192 192
skyblue              = rgb 135 206 235
slateblue            = rgb 106  90 205
slategray            = rgb 112 128 144
slategrey            = rgb 112 128 144
snow                 = rgb 255 250 250
springgreen          = rgb   0 255 127
steelblue            = rgb  70 130 180
tan                  = rgb 210 180 140
teal                 = rgb   0 128 128
thistle              = rgb 216 191 216
tomato               = rgb 255  99  71
turquoise            = rgb  64 224 208
violet               = rgb 238 130 238
wheat                = rgb 245 222 179
white                = rgb 255 255 255
whitesmoke           = rgb 245 245 245
yellow               = rgb 255 255   0
yellowgreen          = rgb 154 205  50
