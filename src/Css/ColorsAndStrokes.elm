module Css.ColorsAndStrokes
  (   
  -- Colors. Colors without the "css" prefix represent Elm colors.

    rgb, rgba, hsl, hsla, hex
  , lighten, darken, lerp
  
  , currentColor, transparent, invert

  , cssAntiqueWhite, cssAqua, cssAquamarine, cssAzure, cssBeige, cssBisque
  , cssBlack, cssBlanchedAlmond, cssBlue, cssBlueViolet, cssBrown, cssBurlyWood
  , cssCadetBlue, cssChartreuse, cssChocolate, cssCoral, cssCornflowerBlue
  , cssCornsilk, cssCrimson, cssCyan, cssDarkBlue, cssDarkCyan, cssDarkGoldenRod
  , cssDarkGray, cssDarkGreen, cssDarkKhaki, cssDarkMagenta, cssDarkOliveGreen
  , cssDarkOrange, cssDarkOrchid, cssDarkRed, cssDarkSalmon, cssDarkSeaGreen
  , cssDarkSlateBlue, cssDarkSlateGray, cssDarkTurquoise, cssDarkViolet
  , cssDeepPink, cssDeepSkyBlue, cssDimGray, cssDodgerBlue, cssFireBrick
  , cssFloralWhite, cssForestGreen, cssFuchsia, cssGainsboro, cssGhostWhite
  , cssGold, cssGoldenRod, cssGray, cssGreen, cssGreenYellow, cssHoneyDew
  , cssHotPink, cssIndianRed , cssIndigo , cssIvory, cssKhaki, cssLavender
  , cssLavenderBlush, cssLawnGreen, cssLemonChiffon, cssLightBlue, cssLightCoral
  , cssLightCyan, cssLightGoldenRodYellow, cssLightGray, cssLightGreen
  , cssLightPink, cssLightSalmon, cssLightSeaGreen, cssLightSkyBlue
  , cssLightSlateGray, cssLightSteelBlue, cssLightYellow, cssLime, cssLimeGreen
  , cssLinen, cssMagenta, cssMaroon, cssMediumAquaMarine, cssMediumBlue
  , cssMediumOrchid, cssMediumPurple, cssMediumSeaGreen, cssMediumSlateBlue
  , cssMediumSpringGreen, cssMediumTurquoise, cssMediumVioletRed
  , cssMidnightBlue, cssMintCream, cssMistyRose, cssMoccasin, cssNavajoWhite
  , cssNavy, cssOldLace, cssOlive, cssOliveDrab, cssOrange, cssOrangeRed
  , cssOrchid, cssPaleGoldenRod, cssPaleGreen, cssPaleTurquoise
  , cssPaleVioletRed, cssPapayaWhip, cssPeachPuff, cssPeru, cssPink, cssPlum
  , cssPowderBlue, cssPurple, cssRebeccaPurple, cssRed, cssRosyBrown
  , cssRoyalBlue, cssSaddleBrown, cssSalmon, cssSandyBrown, cssSeaGreen
  , cssSeaShell, cssSienna, cssSilver, cssSkyBlue, cssSlateBlue, cssSlateGray
  , cssSnow, cssSpringGreen, cssSteelBlue, cssTan, cssTeal, cssThistle
  , cssTomato, cssTurquoise, cssViolet, cssWheat, cssWhite, cssWhiteSmoke
  , cssYellow

  , red, orange, yellow, green, blue, purple, brown
  , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
  , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
  , white, lightGrey, lightGray, grey, gray, darkGrey, darkGray
  , lightCharcoal, charcoal, darkCharcoal, black

  -- * Stroke. Used for border-style, outline-style, text-decoration-style.
    
  , solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

  -- * List style types. Used for lists, also for counters.
  
  , disc, armenian, circleListStyle, cjkIdeographic
  , decimal, decimalLeadingZero
  , georgian, hebrew
  , hiragana, hiraganaIroha
  , katakana, katakanaIroha
  , lowerAlpha, lowerGreek, lowerLatin, lowerRoman
  , square
  , upperAlpha, upperLatin, upperRoman

  ) where


import Color as ElmColor
import String as String

import Css.Internal.Color as Color
import Css.Internal.List as List
import Css.Internal.Stroke as Stroke
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

rgb : Int -> Int -> Int -> Color.ManipulableColorDescriptor rec resultType
rgb r g b = 
  \factory -> 
    if Color.invalidRgb r g b
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString r, toString g, toString b])
    else ElmColor.rgb r g b |> factory.rgbaColor

rgba : Int -> Int -> Int -> Float -> Color.ManipulableColorDescriptor rec resultType
rgba r g b a = 
  \factory -> 
    if Color.invalidRgb r g b || Utils.invalidFractionOf1 a
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString r, toString g, toString b, toString a])
    else ElmColor.rgba r g b a |> factory.rgbaColor

hsl : Int -> Float -> Float -> Color.ManipulableColorDescriptor rec resultType
hsl h s l = 
  \factory -> 
    if Color.invalidHsl h s l
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString h, toString s, toString l])
    else ElmColor.hsl (toFloat h |> degrees) s l |> factory.hslaColor

hsla : Int -> Float -> Float -> Float -> Color.ManipulableColorDescriptor rec resultType
hsla h s l a = 
  \factory -> 
    if Color.invalidHsl h s l || Utils.invalidFractionOf1 a
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString h, toString s, toString l, toString a])
    else ElmColor.hsla (toFloat h |> degrees) s l a |> factory.hslaColor

hex : String -> Color.ManipulableColorDescriptor rec resultType
hex str = 
  \factory ->
    let unhex digit1 digit2 = Utils.fromHex <| String.fromList [digit1, digit2]
        toAlpha digit1 digit2 = 
          unhex digit1 digit2 
            |> Result.map toFloat 
            |> Result.map (\f -> f / 255.0)
            |> Result.map (\f -> Utils.toFixed 2 f)
        digits =
          case String.uncons str of
            Just ('#', cs) -> cs
            _ -> str
        result = 
          case String.toList digits of
            -- Hex alpha is in CSS 4
            [a, b, c, d, e, f, g, h] -> 
              Result.map4 ElmColor.rgba (unhex a b) (unhex c d) (unhex e f) (toAlpha g h)
            [a, b, c, d, e, f      ] -> 
              Result.map3 ElmColor.rgb  (unhex a b) (unhex c d) (unhex e f)
            [a, b, c, d            ] -> 
              Result.map4 ElmColor.rgba (unhex a a) (unhex b b) (unhex c c) (toAlpha d d)  
            [a, b, c               ] -> 
              Result.map3 ElmColor.rgb  (unhex a a) (unhex b b) (unhex c c)
            other                    -> Err ("INVALID COLOR STRING: " ++ str)
    in case result of
      Ok color -> factory.rgbaColor color
      Err str -> factory.invalid_ str

-------------------------------------------------------------------------------
lighten : Float -> 
          Color.ManipulableColorDescriptor {} Color.ColorHolder -> 
          Color.ManipulableColorDescriptor rec resultType
lighten factor colorDescriptor = lerp factor colorDescriptor white

darken : Float -> 
         Color.ManipulableColorDescriptor {} Color.ColorHolder-> 
         Color.ManipulableColorDescriptor rec resultType
darken factor colorDescriptor = lerp factor colorDescriptor black

lerp : Float -> 
       Color.ManipulableColorDescriptor {} Color.ColorHolder -> 
       Color.ManipulableColorDescriptor {} Color.ColorHolder -> 
       Color.ManipulableColorDescriptor rec resultType
lerp factor startColorDescriptor boundaryColorDescriptor = 
  Color.descriptorLerp factor startColorDescriptor boundaryColorDescriptor

-------------------------------------------------------------------------------

currentColor : Color.NubColorDescriptor rec
currentColor = \factory -> factory.currentColor

transparent : Color.NubColorDescriptor rec
transparent = \factory -> factory.transparent

invert : Color.NubColorDescriptorWithInvert rec
invert = \factory -> factory.invert

-------------------------------------------------------------------------------
-- Css colors. The generated CSS uses the color names.

cssAntiqueWhite : Color.ManipulableColorDescriptor rec resultType
cssAntiqueWhite = 
  \factory -> factory.namedRgba "AntiqueWhite" (ElmColor.rgb 250 235 215)

cssAqua : Color.ManipulableColorDescriptor rec resultType
cssAqua = 
  \factory -> factory.namedRgba "Aqua" (ElmColor.rgb 0 255 255)

cssAquamarine : Color.ManipulableColorDescriptor rec resultType
cssAquamarine = 
  \factory -> factory.namedRgba "Aquamarine" (ElmColor.rgb 127 255 212)

cssAzure : Color.ManipulableColorDescriptor rec resultType
cssAzure = 
  \factory -> factory.namedRgba "Azure" (ElmColor.rgb 240 255 255)

cssBeige : Color.ManipulableColorDescriptor rec resultType
cssBeige = 
  \factory -> factory.namedRgba "Beige" (ElmColor.rgb 245 245 220)

cssBisque : Color.ManipulableColorDescriptor rec resultType
cssBisque = 
  \factory -> factory.namedRgba "Bisque" (ElmColor.rgb 255 228 196)

cssBlack : Color.ManipulableColorDescriptor rec resultType
cssBlack = 
  \factory -> factory.namedRgba "Black" (ElmColor.rgb 0 0 0)

cssBlanchedAlmond : Color.ManipulableColorDescriptor rec resultType
cssBlanchedAlmond = 
  \factory -> factory.namedRgba "BlanchedAlmond" (ElmColor.rgb 255 235 205)

cssBlue : Color.ManipulableColorDescriptor rec resultType
cssBlue = 
  \factory -> factory.namedRgba "Blue" (ElmColor.rgb 0 0 255)

cssBlueViolet : Color.ManipulableColorDescriptor rec resultType
cssBlueViolet = 
  \factory -> factory.namedRgba "BlueViolet" (ElmColor.rgb 138 43 226)

cssBrown : Color.ManipulableColorDescriptor rec resultType
cssBrown = 
  \factory -> factory.namedRgba "Brown" (ElmColor.rgb 165 42 42)

cssBurlyWood : Color.ManipulableColorDescriptor rec resultType
cssBurlyWood = 
  \factory -> factory.namedRgba "BurlyWood" (ElmColor.rgb 222 184 135)

cssCadetBlue : Color.ManipulableColorDescriptor rec resultType
cssCadetBlue = 
  \factory -> factory.namedRgba "CadetBlue" (ElmColor.rgb 95 158 160)

cssChartreuse : Color.ManipulableColorDescriptor rec resultType
cssChartreuse = 
  \factory -> factory.namedRgba "Chartreuse" (ElmColor.rgb 127 255 0)

cssChocolate : Color.ManipulableColorDescriptor rec resultType
cssChocolate = 
  \factory -> factory.namedRgba "Chocolate" (ElmColor.rgb 210 105 30)

cssCoral : Color.ManipulableColorDescriptor rec resultType
cssCoral = 
  \factory -> factory.namedRgba "Coral" (ElmColor.rgb 255 127 80)

cssCornflowerBlue : Color.ManipulableColorDescriptor rec resultType
cssCornflowerBlue = 
  \factory -> factory.namedRgba "CornflowerBlue" (ElmColor.rgb 100 149 237)

cssCornsilk : Color.ManipulableColorDescriptor rec resultType
cssCornsilk = 
  \factory -> factory.namedRgba "Cornsilk" (ElmColor.rgb 255 248 220)

cssCrimson : Color.ManipulableColorDescriptor rec resultType
cssCrimson = 
  \factory -> factory.namedRgba "Crimson" (ElmColor.rgb 220 20 60)

cssCyan : Color.ManipulableColorDescriptor rec resultType
cssCyan = 
  \factory -> factory.namedRgba "Cyan" (ElmColor.rgb 0 255 255)

cssDarkBlue : Color.ManipulableColorDescriptor rec resultType
cssDarkBlue = 
  \factory -> factory.namedRgba "DarkBlue" (ElmColor.rgb 0 0 139)

cssDarkCyan : Color.ManipulableColorDescriptor rec resultType
cssDarkCyan = 
  \factory -> factory.namedRgba "DarkCyan" (ElmColor.rgb 0 139 139)

cssDarkGoldenRod : Color.ManipulableColorDescriptor rec resultType
cssDarkGoldenRod = 
  \factory -> factory.namedRgba "DarkGoldenRod" (ElmColor.rgb 184 134 11)

cssDarkGray : Color.ManipulableColorDescriptor rec resultType
cssDarkGray = 
  \factory -> factory.namedRgba "DarkGray" (ElmColor.rgb 169 169 169)

cssDarkGreen : Color.ManipulableColorDescriptor rec resultType
cssDarkGreen = 
  \factory -> factory.namedRgba "DarkGreen" (ElmColor.rgb 0 100 0)

cssDarkKhaki : Color.ManipulableColorDescriptor rec resultType
cssDarkKhaki = 
  \factory -> factory.namedRgba "DarkKhaki" (ElmColor.rgb 189 183 107)

cssDarkMagenta : Color.ManipulableColorDescriptor rec resultType
cssDarkMagenta = 
  \factory -> factory.namedRgba "DarkMagenta" (ElmColor.rgb 139 0 139)

cssDarkOliveGreen : Color.ManipulableColorDescriptor rec resultType
cssDarkOliveGreen = 
  \factory -> factory.namedRgba "DarkOliveGreen" (ElmColor.rgb 85 107 47)

cssDarkOrange : Color.ManipulableColorDescriptor rec resultType
cssDarkOrange = 
  \factory -> factory.namedRgba "DarkOrange" (ElmColor.rgb 255 140 0)

cssDarkOrchid : Color.ManipulableColorDescriptor rec resultType
cssDarkOrchid = 
  \factory -> factory.namedRgba "DarkOrchid" (ElmColor.rgb 153 50 204)

cssDarkRed : Color.ManipulableColorDescriptor rec resultType
cssDarkRed = 
  \factory -> factory.namedRgba "DarkRed" (ElmColor.rgb 139 0 0)

cssDarkSalmon : Color.ManipulableColorDescriptor rec resultType
cssDarkSalmon = 
  \factory -> factory.namedRgba "DarkSalmon" (ElmColor.rgb 233 150 122)

cssDarkSeaGreen : Color.ManipulableColorDescriptor rec resultType
cssDarkSeaGreen = 
  \factory -> factory.namedRgba "DarkSeaGreen" (ElmColor.rgb 143 188 143)

cssDarkSlateBlue : Color.ManipulableColorDescriptor rec resultType
cssDarkSlateBlue = 
  \factory -> factory.namedRgba "DarkSlateBlue" (ElmColor.rgb 72 61 139)

cssDarkSlateGray : Color.ManipulableColorDescriptor rec resultType
cssDarkSlateGray = 
  \factory -> factory.namedRgba "DarkSlateGray" (ElmColor.rgb 47 79 79)

cssDarkTurquoise : Color.ManipulableColorDescriptor rec resultType
cssDarkTurquoise = 
  \factory -> factory.namedRgba "DarkTurquoise" (ElmColor.rgb 0 206 209)

cssDarkViolet : Color.ManipulableColorDescriptor rec resultType
cssDarkViolet = 
  \factory -> factory.namedRgba "DarkViolet" (ElmColor.rgb 148 0 211)

cssDeepPink : Color.ManipulableColorDescriptor rec resultType
cssDeepPink = 
  \factory -> factory.namedRgba "DeepPink" (ElmColor.rgb 255 20 147)

cssDeepSkyBlue : Color.ManipulableColorDescriptor rec resultType
cssDeepSkyBlue = 
  \factory -> factory.namedRgba "DeepSkyBlue" (ElmColor.rgb 0 191 255)

cssDimGray : Color.ManipulableColorDescriptor rec resultType
cssDimGray = 
  \factory -> factory.namedRgba "DimGray" (ElmColor.rgb 105 105 105)

cssDodgerBlue : Color.ManipulableColorDescriptor rec resultType
cssDodgerBlue = 
  \factory -> factory.namedRgba "DodgerBlue" (ElmColor.rgb 30 144 255)

cssFireBrick : Color.ManipulableColorDescriptor rec resultType
cssFireBrick = 
  \factory -> factory.namedRgba "FireBrick" (ElmColor.rgb 178 34 34)

cssFloralWhite : Color.ManipulableColorDescriptor rec resultType
cssFloralWhite = 
  \factory -> factory.namedRgba "FloralWhite" (ElmColor.rgb 255 250 240)

cssForestGreen : Color.ManipulableColorDescriptor rec resultType
cssForestGreen = 
  \factory -> factory.namedRgba "ForestGreen" (ElmColor.rgb 34 139 34)

cssFuchsia : Color.ManipulableColorDescriptor rec resultType
cssFuchsia = 
  \factory -> factory.namedRgba "Fuchsia" (ElmColor.rgb 255 0 255)

cssGainsboro : Color.ManipulableColorDescriptor rec resultType
cssGainsboro = 
  \factory -> factory.namedRgba "Gainsboro" (ElmColor.rgb 220 220 220)

cssGhostWhite : Color.ManipulableColorDescriptor rec resultType
cssGhostWhite = 
  \factory -> factory.namedRgba "GhostWhite" (ElmColor.rgb 248 248 255)

cssGold : Color.ManipulableColorDescriptor rec resultType
cssGold = 
  \factory -> factory.namedRgba "Gold" (ElmColor.rgb 255 215 0)

cssGoldenRod : Color.ManipulableColorDescriptor rec resultType
cssGoldenRod = 
  \factory -> factory.namedRgba "GoldenRod" (ElmColor.rgb 218 165 32)

cssGray : Color.ManipulableColorDescriptor rec resultType
cssGray = 
  \factory -> factory.namedRgba "Gray" (ElmColor.rgb 128 128 128)

cssGreen : Color.ManipulableColorDescriptor rec resultType
cssGreen = 
  \factory -> factory.namedRgba "Green" (ElmColor.rgb 0 128 0)

cssGreenYellow : Color.ManipulableColorDescriptor rec resultType
cssGreenYellow = 
  \factory -> factory.namedRgba "GreenYellow" (ElmColor.rgb 173 255 47)

cssHoneyDew : Color.ManipulableColorDescriptor rec resultType
cssHoneyDew = 
  \factory -> factory.namedRgba "HoneyDew" (ElmColor.rgb 240 255 240)

cssHotPink : Color.ManipulableColorDescriptor rec resultType
cssHotPink = 
  \factory -> factory.namedRgba "HotPink" (ElmColor.rgb 255 105 180)

cssIndianRed  : Color.ManipulableColorDescriptor rec resultType
cssIndianRed  = 
  \factory -> factory.namedRgba "IndianRed " (ElmColor.rgb 205 92 92)

cssIndigo  : Color.ManipulableColorDescriptor rec resultType
cssIndigo  = 
  \factory -> factory.namedRgba "Indigo " (ElmColor.rgb 75 0 130)

cssIvory : Color.ManipulableColorDescriptor rec resultType
cssIvory = 
  \factory -> factory.namedRgba "Ivory" (ElmColor.rgb 255 255 240)

cssKhaki : Color.ManipulableColorDescriptor rec resultType
cssKhaki = 
  \factory -> factory.namedRgba "Khaki" (ElmColor.rgb 240 230 140)

cssLavender : Color.ManipulableColorDescriptor rec resultType
cssLavender = 
  \factory -> factory.namedRgba "Lavender" (ElmColor.rgb 230 230 250)

cssLavenderBlush : Color.ManipulableColorDescriptor rec resultType
cssLavenderBlush = 
  \factory -> factory.namedRgba "LavenderBlush" (ElmColor.rgb 255 240 245)

cssLawnGreen : Color.ManipulableColorDescriptor rec resultType
cssLawnGreen = 
  \factory -> factory.namedRgba "LawnGreen" (ElmColor.rgb 124 252 0)

cssLemonChiffon : Color.ManipulableColorDescriptor rec resultType
cssLemonChiffon = 
  \factory -> factory.namedRgba "LemonChiffon" (ElmColor.rgb 255 250 205)

cssLightBlue : Color.ManipulableColorDescriptor rec resultType
cssLightBlue = 
  \factory -> factory.namedRgba "LightBlue" (ElmColor.rgb 173 216 230)

cssLightCoral : Color.ManipulableColorDescriptor rec resultType
cssLightCoral = 
  \factory -> factory.namedRgba "LightCoral" (ElmColor.rgb 240 128 128)

cssLightCyan : Color.ManipulableColorDescriptor rec resultType
cssLightCyan = 
  \factory -> factory.namedRgba "LightCyan" (ElmColor.rgb 224 255 255)

cssLightGoldenRodYellow : Color.ManipulableColorDescriptor rec resultType
cssLightGoldenRodYellow = 
  \factory -> factory.namedRgba "LightGoldenRodYellow" (ElmColor.rgb 250 250 210)

cssLightGray : Color.ManipulableColorDescriptor rec resultType
cssLightGray = 
  \factory -> factory.namedRgba "LightGray" (ElmColor.rgb 211 211 211)

cssLightGreen : Color.ManipulableColorDescriptor rec resultType
cssLightGreen = 
  \factory -> factory.namedRgba "LightGreen" (ElmColor.rgb 144 238 144)

cssLightPink : Color.ManipulableColorDescriptor rec resultType
cssLightPink = 
  \factory -> factory.namedRgba "LightPink" (ElmColor.rgb 255 182 193)

cssLightSalmon : Color.ManipulableColorDescriptor rec resultType
cssLightSalmon = 
  \factory -> factory.namedRgba "LightSalmon" (ElmColor.rgb 255 160 122)

cssLightSeaGreen : Color.ManipulableColorDescriptor rec resultType
cssLightSeaGreen = 
  \factory -> factory.namedRgba "LightSeaGreen" (ElmColor.rgb 32 178 170)

cssLightSkyBlue : Color.ManipulableColorDescriptor rec resultType
cssLightSkyBlue = 
  \factory -> factory.namedRgba "LightSkyBlue" (ElmColor.rgb 135 206 250)

cssLightSlateGray : Color.ManipulableColorDescriptor rec resultType
cssLightSlateGray = 
  \factory -> factory.namedRgba "LightSlateGray" (ElmColor.rgb 119 136 153)

cssLightSteelBlue : Color.ManipulableColorDescriptor rec resultType
cssLightSteelBlue = 
  \factory -> factory.namedRgba "LightSteelBlue" (ElmColor.rgb 176 196 222)

cssLightYellow : Color.ManipulableColorDescriptor rec resultType
cssLightYellow = 
  \factory -> factory.namedRgba "LightYellow" (ElmColor.rgb 255 255 224)

cssLime : Color.ManipulableColorDescriptor rec resultType
cssLime = 
  \factory -> factory.namedRgba "Lime" (ElmColor.rgb 0 255 0)

cssLimeGreen : Color.ManipulableColorDescriptor rec resultType
cssLimeGreen = 
  \factory -> factory.namedRgba "LimeGreen" (ElmColor.rgb 50 205 50)

cssLinen : Color.ManipulableColorDescriptor rec resultType
cssLinen = 
  \factory -> factory.namedRgba "Linen" (ElmColor.rgb 250 240 230)

cssMagenta : Color.ManipulableColorDescriptor rec resultType
cssMagenta = 
  \factory -> factory.namedRgba "Magenta" (ElmColor.rgb 255 0 255)

cssMaroon : Color.ManipulableColorDescriptor rec resultType
cssMaroon = 
  \factory -> factory.namedRgba "Maroon" (ElmColor.rgb 128 0 0)

cssMediumAquaMarine : Color.ManipulableColorDescriptor rec resultType
cssMediumAquaMarine = 
  \factory -> factory.namedRgba "MediumAquaMarine" (ElmColor.rgb 102 205 170)

cssMediumBlue : Color.ManipulableColorDescriptor rec resultType
cssMediumBlue = 
  \factory -> factory.namedRgba "MediumBlue" (ElmColor.rgb 0 0 205)

cssMediumOrchid : Color.ManipulableColorDescriptor rec resultType
cssMediumOrchid = 
  \factory -> factory.namedRgba "MediumOrchid" (ElmColor.rgb 186 85 211)

cssMediumPurple : Color.ManipulableColorDescriptor rec resultType
cssMediumPurple = 
  \factory -> factory.namedRgba "MediumPurple" (ElmColor.rgb 147 112 219)

cssMediumSeaGreen : Color.ManipulableColorDescriptor rec resultType
cssMediumSeaGreen = 
  \factory -> factory.namedRgba "MediumSeaGreen" (ElmColor.rgb 60 179 113)

cssMediumSlateBlue : Color.ManipulableColorDescriptor rec resultType
cssMediumSlateBlue = 
  \factory -> factory.namedRgba "MediumSlateBlue" (ElmColor.rgb 123 104 238)

cssMediumSpringGreen : Color.ManipulableColorDescriptor rec resultType
cssMediumSpringGreen = 
  \factory -> factory.namedRgba "MediumSpringGreen" (ElmColor.rgb 0 250 154)

cssMediumTurquoise : Color.ManipulableColorDescriptor rec resultType
cssMediumTurquoise = 
  \factory -> factory.namedRgba "MediumTurquoise" (ElmColor.rgb 72 209 204)

cssMediumVioletRed : Color.ManipulableColorDescriptor rec resultType
cssMediumVioletRed = 
  \factory -> factory.namedRgba "MediumVioletRed" (ElmColor.rgb 199 21 133)

cssMidnightBlue : Color.ManipulableColorDescriptor rec resultType
cssMidnightBlue = 
  \factory -> factory.namedRgba "MidnightBlue" (ElmColor.rgb 25 25 112)

cssMintCream : Color.ManipulableColorDescriptor rec resultType
cssMintCream = 
  \factory -> factory.namedRgba "MintCream" (ElmColor.rgb 245 255 250)

cssMistyRose : Color.ManipulableColorDescriptor rec resultType
cssMistyRose = 
  \factory -> factory.namedRgba "MistyRose" (ElmColor.rgb 255 228 225)

cssMoccasin : Color.ManipulableColorDescriptor rec resultType
cssMoccasin = 
  \factory -> factory.namedRgba "Moccasin" (ElmColor.rgb 255 228 181)

cssNavajoWhite : Color.ManipulableColorDescriptor rec resultType
cssNavajoWhite = 
  \factory -> factory.namedRgba "NavajoWhite" (ElmColor.rgb 255 222 173)

cssNavy : Color.ManipulableColorDescriptor rec resultType
cssNavy = 
  \factory -> factory.namedRgba "Navy" (ElmColor.rgb 0 0 128)

cssOldLace : Color.ManipulableColorDescriptor rec resultType
cssOldLace = 
  \factory -> factory.namedRgba "OldLace" (ElmColor.rgb 253 245 230)

cssOlive : Color.ManipulableColorDescriptor rec resultType
cssOlive = 
  \factory -> factory.namedRgba "Olive" (ElmColor.rgb 128 128 0)

cssOliveDrab : Color.ManipulableColorDescriptor rec resultType
cssOliveDrab = 
  \factory -> factory.namedRgba "OliveDrab" (ElmColor.rgb 107 142 35)

cssOrange : Color.ManipulableColorDescriptor rec resultType
cssOrange = 
  \factory -> factory.namedRgba "Orange" (ElmColor.rgb 255 165 0)

cssOrangeRed : Color.ManipulableColorDescriptor rec resultType
cssOrangeRed = 
  \factory -> factory.namedRgba "OrangeRed" (ElmColor.rgb 255 69 0)

cssOrchid : Color.ManipulableColorDescriptor rec resultType
cssOrchid = 
  \factory -> factory.namedRgba "Orchid" (ElmColor.rgb 218 112 214)

cssPaleGoldenRod : Color.ManipulableColorDescriptor rec resultType
cssPaleGoldenRod = 
  \factory -> factory.namedRgba "PaleGoldenRod" (ElmColor.rgb 238 232 170)

cssPaleGreen : Color.ManipulableColorDescriptor rec resultType
cssPaleGreen = 
  \factory -> factory.namedRgba "PaleGreen" (ElmColor.rgb 152 251 152)

cssPaleTurquoise : Color.ManipulableColorDescriptor rec resultType
cssPaleTurquoise = 
  \factory -> factory.namedRgba "PaleTurquoise" (ElmColor.rgb 175 238 238)

cssPaleVioletRed : Color.ManipulableColorDescriptor rec resultType
cssPaleVioletRed = 
  \factory -> factory.namedRgba "PaleVioletRed" (ElmColor.rgb 219 112 147)

cssPapayaWhip : Color.ManipulableColorDescriptor rec resultType
cssPapayaWhip = 
  \factory -> factory.namedRgba "PapayaWhip" (ElmColor.rgb 255 239 213)

cssPeachPuff : Color.ManipulableColorDescriptor rec resultType
cssPeachPuff = 
  \factory -> factory.namedRgba "PeachPuff" (ElmColor.rgb 255 218 185)

cssPeru : Color.ManipulableColorDescriptor rec resultType
cssPeru = 
  \factory -> factory.namedRgba "Peru" (ElmColor.rgb 205 133 63)

cssPink : Color.ManipulableColorDescriptor rec resultType
cssPink = 
  \factory -> factory.namedRgba "Pink" (ElmColor.rgb 255 192 203)

cssPlum : Color.ManipulableColorDescriptor rec resultType
cssPlum = 
  \factory -> factory.namedRgba "Plum" (ElmColor.rgb 221 160 221)

cssPowderBlue : Color.ManipulableColorDescriptor rec resultType
cssPowderBlue = 
  \factory -> factory.namedRgba "PowderBlue" (ElmColor.rgb 176 224 230)

cssPurple : Color.ManipulableColorDescriptor rec resultType
cssPurple = 
  \factory -> factory.namedRgba "Purple" (ElmColor.rgb 128 0 128)

cssRebeccaPurple : Color.ManipulableColorDescriptor rec resultType
cssRebeccaPurple = 
  \factory -> factory.namedRgba "RebeccaPurple" (ElmColor.rgb 102 51 153)

cssRed : Color.ManipulableColorDescriptor rec resultType
cssRed = 
  \factory -> factory.namedRgba "Red" (ElmColor.rgb 255 0 0)

cssRosyBrown : Color.ManipulableColorDescriptor rec resultType
cssRosyBrown = 
  \factory -> factory.namedRgba "RosyBrown" (ElmColor.rgb 188 143 143)

cssRoyalBlue : Color.ManipulableColorDescriptor rec resultType
cssRoyalBlue = 
  \factory -> factory.namedRgba "RoyalBlue" (ElmColor.rgb 65 105 225)

cssSaddleBrown : Color.ManipulableColorDescriptor rec resultType
cssSaddleBrown = 
  \factory -> factory.namedRgba "SaddleBrown" (ElmColor.rgb 139 69 19)

cssSalmon : Color.ManipulableColorDescriptor rec resultType
cssSalmon = 
  \factory -> factory.namedRgba "Salmon" (ElmColor.rgb 250 128 114)

cssSandyBrown : Color.ManipulableColorDescriptor rec resultType
cssSandyBrown = 
  \factory -> factory.namedRgba "SandyBrown" (ElmColor.rgb 244 164 96)

cssSeaGreen : Color.ManipulableColorDescriptor rec resultType
cssSeaGreen = 
  \factory -> factory.namedRgba "SeaGreen" (ElmColor.rgb 46 139 87)

cssSeaShell : Color.ManipulableColorDescriptor rec resultType
cssSeaShell = 
  \factory -> factory.namedRgba "SeaShell" (ElmColor.rgb 255 245 238)

cssSienna : Color.ManipulableColorDescriptor rec resultType
cssSienna = 
  \factory -> factory.namedRgba "Sienna" (ElmColor.rgb 160 82 45)

cssSilver : Color.ManipulableColorDescriptor rec resultType
cssSilver = 
  \factory -> factory.namedRgba "Silver" (ElmColor.rgb 192 192 192)

cssSkyBlue : Color.ManipulableColorDescriptor rec resultType
cssSkyBlue = 
  \factory -> factory.namedRgba "SkyBlue" (ElmColor.rgb 135 206 235)

cssSlateBlue : Color.ManipulableColorDescriptor rec resultType
cssSlateBlue = 
  \factory -> factory.namedRgba "SlateBlue" (ElmColor.rgb 106 90 205)

cssSlateGray : Color.ManipulableColorDescriptor rec resultType
cssSlateGray = 
  \factory -> factory.namedRgba "SlateGray" (ElmColor.rgb 112 128 144)

cssSnow : Color.ManipulableColorDescriptor rec resultType
cssSnow = 
  \factory -> factory.namedRgba "Snow" (ElmColor.rgb 255 250 250)

cssSpringGreen : Color.ManipulableColorDescriptor rec resultType
cssSpringGreen = 
  \factory -> factory.namedRgba "SpringGreen" (ElmColor.rgb 0 255 127)

cssSteelBlue : Color.ManipulableColorDescriptor rec resultType
cssSteelBlue = 
  \factory -> factory.namedRgba "SteelBlue" (ElmColor.rgb 70 130 180)

cssTan : Color.ManipulableColorDescriptor rec resultType
cssTan = 
  \factory -> factory.namedRgba "Tan" (ElmColor.rgb 210 180 140)

cssTeal : Color.ManipulableColorDescriptor rec resultType
cssTeal = 
  \factory -> factory.namedRgba "Teal" (ElmColor.rgb 0 128 128)

cssThistle : Color.ManipulableColorDescriptor rec resultType
cssThistle = 
  \factory -> factory.namedRgba "Thistle" (ElmColor.rgb 216 191 216)

cssTomato : Color.ManipulableColorDescriptor rec resultType
cssTomato = 
  \factory -> factory.namedRgba "Tomato" (ElmColor.rgb 255 99 71)

cssTurquoise : Color.ManipulableColorDescriptor rec resultType
cssTurquoise = 
  \factory -> factory.namedRgba "Turquoise" (ElmColor.rgb 64 224 208)

cssViolet : Color.ManipulableColorDescriptor rec resultType
cssViolet = 
  \factory -> factory.namedRgba "Violet" (ElmColor.rgb 238 130 238)

cssWheat : Color.ManipulableColorDescriptor rec resultType
cssWheat = 
  \factory -> factory.namedRgba "Wheat" (ElmColor.rgb 245 222 179)

cssWhite : Color.ManipulableColorDescriptor rec resultType
cssWhite = 
  \factory -> factory.namedRgba "White" (ElmColor.rgb 255 255 255)

cssWhiteSmoke : Color.ManipulableColorDescriptor rec resultType
cssWhiteSmoke = 
  \factory -> factory.namedRgba "WhiteSmoke" (ElmColor.rgb 245 245 245)

cssYellow : Color.ManipulableColorDescriptor rec resultType
cssYellow = 
  \factory -> factory.namedRgba "Yellow" (ElmColor.rgb 255 255 0)

cssYellowGreen : Color.ManipulableColorDescriptor rec resultType
cssYellowGreen = 
  \factory -> factory.namedRgba "YellowGreen" (ElmColor.rgb 154 205 50)

-------------------------------------------------------------------------------
-- Elm colors.

red : Color.ManipulableColorDescriptor rec resultType
red = \factory -> ElmColor.red |> factory.rgbaColor

orange : Color.ManipulableColorDescriptor rec resultType
orange = \factory -> ElmColor.orange |> factory.rgbaColor

yellow : Color.ManipulableColorDescriptor rec resultType
yellow = \factory -> ElmColor.yellow |> factory.rgbaColor

green : Color.ManipulableColorDescriptor rec resultType
green = \factory -> ElmColor.green |> factory.rgbaColor

blue : Color.ManipulableColorDescriptor rec resultType
blue = \factory -> ElmColor.blue |> factory.rgbaColor

purple : Color.ManipulableColorDescriptor rec resultType
purple = \factory -> ElmColor.purple |> factory.rgbaColor

brown : Color.ManipulableColorDescriptor rec resultType
brown = \factory -> ElmColor.brown |> factory.rgbaColor



lightRed : Color.ManipulableColorDescriptor rec resultType
lightRed = \factory -> ElmColor.lightRed |> factory.rgbaColor

lightOrange : Color.ManipulableColorDescriptor rec resultType
lightOrange = \factory -> ElmColor.lightOrange |> factory.rgbaColor

lightYellow : Color.ManipulableColorDescriptor rec resultType
lightYellow = \factory -> ElmColor.lightYellow |> factory.rgbaColor

lightGreen : Color.ManipulableColorDescriptor rec resultType
lightGreen = \factory -> ElmColor.lightGreen |> factory.rgbaColor

lightBlue : Color.ManipulableColorDescriptor rec resultType
lightBlue = \factory -> ElmColor.lightBlue |> factory.rgbaColor

lightPurple : Color.ManipulableColorDescriptor rec resultType
lightPurple = \factory -> ElmColor.lightPurple |> factory.rgbaColor

lightBrown : Color.ManipulableColorDescriptor rec resultType
lightBrown = \factory -> ElmColor.lightBrown |> factory.rgbaColor



darkRed : Color.ManipulableColorDescriptor rec resultType
darkRed = \factory -> ElmColor.darkRed |> factory.rgbaColor

darkOrange : Color.ManipulableColorDescriptor rec resultType
darkOrange = \factory -> ElmColor.darkOrange |> factory.rgbaColor

darkYellow : Color.ManipulableColorDescriptor rec resultType
darkYellow = \factory -> ElmColor.darkYellow |> factory.rgbaColor

darkGreen : Color.ManipulableColorDescriptor rec resultType
darkGreen = \factory -> ElmColor.darkGreen |> factory.rgbaColor

darkBlue : Color.ManipulableColorDescriptor rec resultType
darkBlue = \factory -> ElmColor.darkBlue |> factory.rgbaColor

darkPurple : Color.ManipulableColorDescriptor rec resultType
darkPurple = \factory -> ElmColor.darkPurple |> factory.rgbaColor

darkBrown : Color.ManipulableColorDescriptor rec resultType
darkBrown = \factory -> ElmColor.darkBrown |> factory.rgbaColor



white : Color.ManipulableColorDescriptor rec resultType
white = \factory -> ElmColor.white |> factory.rgbaColor

lightGrey : Color.ManipulableColorDescriptor rec resultType
lightGrey = \factory -> ElmColor.lightGrey |> factory.rgbaColor

lightGray : Color.ManipulableColorDescriptor rec resultType
lightGray = \factory -> ElmColor.lightGray |> factory.rgbaColor

grey : Color.ManipulableColorDescriptor rec resultType
grey = \factory -> ElmColor.grey |> factory.rgbaColor

gray : Color.ManipulableColorDescriptor rec resultType
gray = \factory -> ElmColor.gray |> factory.rgbaColor

darkGrey : Color.ManipulableColorDescriptor rec resultType
darkGrey = \factory -> ElmColor.darkGrey |> factory.rgbaColor

darkGray : Color.ManipulableColorDescriptor rec resultType
darkGray = \factory -> ElmColor.darkGray |> factory.rgbaColor

lightCharcoal : Color.ManipulableColorDescriptor rec resultType 
lightCharcoal = \factory -> ElmColor.lightCharcoal |> factory.rgbaColor

charcoal : Color.ManipulableColorDescriptor rec resultType
charcoal = \factory -> ElmColor.charcoal |> factory.rgbaColor

darkCharcoal : Color.ManipulableColorDescriptor rec resultType
darkCharcoal = \factory -> ElmColor.darkCharcoal |> factory.rgbaColor

black : Color.ManipulableColorDescriptor rec resultType
black = \factory -> ElmColor.black |> factory.rgbaColor

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
  
disc : List.ListStyleTypeDescriptor
disc = \factory -> factory.named "disc"

armenian : List.ListStyleTypeDescriptor
armenian = \factory -> factory.named "armenian"

circleListStyle : List.ListStyleTypeDescriptor
circleListStyle = \factory -> factory.named "circle"

cjkIdeographic : List.ListStyleTypeDescriptor
cjkIdeographic = \factory -> factory.named "cjk-ideographic"

decimal : List.ListStyleTypeDescriptor
decimal = \factory -> factory.named "decimal"

decimalLeadingZero : List.ListStyleTypeDescriptor
decimalLeadingZero = \factory -> factory.named "decimal-leading-zero"

georgian : List.ListStyleTypeDescriptor
georgian = \factory -> factory.named "georgian"

hebrew : List.ListStyleTypeDescriptor
hebrew = \factory -> factory.named "hebrew"

hiragana : List.ListStyleTypeDescriptor
hiragana = \factory -> factory.named "hiragana"

hiraganaIroha : List.ListStyleTypeDescriptor
hiraganaIroha = \factory -> factory.named "hiragana-iroha"

katakana : List.ListStyleTypeDescriptor
katakana = \factory -> factory.named "katakana"

katakanaIroha : List.ListStyleTypeDescriptor
katakanaIroha = \factory -> factory.named "katakana-iroha"

lowerAlpha : List.ListStyleTypeDescriptor
lowerAlpha = \factory -> factory.named "lower-alpha"

lowerGreek : List.ListStyleTypeDescriptor
lowerGreek = \factory -> factory.named "lower-greek"

lowerLatin : List.ListStyleTypeDescriptor
lowerLatin = \factory -> factory.named "lower-latin"

lowerRoman : List.ListStyleTypeDescriptor
lowerRoman = \factory -> factory.named "lower-roman"

square : List.ListStyleTypeDescriptor
square = \factory -> factory.named "square"

upperAlpha : List.ListStyleTypeDescriptor
upperAlpha = \factory -> factory.named "upper-alpha"

upperLatin : List.ListStyleTypeDescriptor
upperLatin = \factory -> factory.named "upper-latin"

upperRoman : List.ListStyleTypeDescriptor
upperRoman = \factory -> factory.named "upper-roman"
