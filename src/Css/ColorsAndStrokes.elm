module Css.ColorsAndStrokes
  (   
  -- Colors. Colors without the "css" prefix represent Elm colors.

    rgb, rgba, hsl, hsla, hex
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
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

rgb : Int -> Int -> Int -> Color.NubColorDescriptor rec
rgb r g b = 
  \factory -> 
    if Color.invalidRgb r g b
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString r, toString g, toString b])
    else ElmColor.rgb r g b |> factory.rgbaColor

rgba : Int -> Int -> Int -> Float -> Color.NubColorDescriptor rec
rgba r g b a = 
  \factory -> 
    if Color.invalidRgb r g b || Utils.invalidFractionOf1 a
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString r, toString g, toString b, toString a])
    else ElmColor.rgba r g b a |> factory.rgbaColor

hsl : Int -> Float -> Float -> Color.NubColorDescriptor rec
hsl h s l = 
  \factory -> 
    if Color.invalidHsl h s l
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString h, toString s, toString l])
    else ElmColor.hsl (toFloat h |> degrees) s l |> factory.hslaColor

hsla : Int -> Float -> Float -> Float -> Color.NubColorDescriptor rec
hsla h s l a = 
  \factory -> 
    if Color.invalidHsl h s l || Utils.invalidFractionOf1 a
    then factory.invalid_ 
          ("INVALID COLOR: " ++ 
            String.join "," [toString h, toString s, toString l, toString a])
    else ElmColor.hsla (toFloat h |> degrees) s l a |> factory.hslaColor

hex : String -> Color.NubColorDescriptor rec
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

currentColor : Color.NubColorDescriptor rec
currentColor = \factory -> factory.currentColor

transparent : Color.NubColorDescriptor rec
transparent = \factory -> Property.stringValue "transparent" |> factory.other_

invert : Color.NubColorDescriptorWithInvert rec
invert = \factory -> factory.invert

-------------------------------------------------------------------------------
-- Css colors. The names are used in the generated CSS.

cssAntiqueWhite : Color.NubColorDescriptor rec
cssAntiqueWhite = 
  \factory -> factory.named "AntiqueWhite" (ElmColor.rgb 250 235 215)

cssAqua : Color.NubColorDescriptor rec
cssAqua = 
  \factory -> factory.named "Aqua" (ElmColor.rgb 0 255 255)

cssAquamarine : Color.NubColorDescriptor rec
cssAquamarine = 
  \factory -> factory.named "Aquamarine" (ElmColor.rgb 127 255 212)

cssAzure : Color.NubColorDescriptor rec
cssAzure = 
  \factory -> factory.named "Azure" (ElmColor.rgb 240 255 255)

cssBeige : Color.NubColorDescriptor rec
cssBeige = 
  \factory -> factory.named "Beige" (ElmColor.rgb 245 245 220)

cssBisque : Color.NubColorDescriptor rec
cssBisque = 
  \factory -> factory.named "Bisque" (ElmColor.rgb 255 228 196)

cssBlack : Color.NubColorDescriptor rec
cssBlack = 
  \factory -> factory.named "Black" (ElmColor.rgb 0 0 0)

cssBlanchedAlmond : Color.NubColorDescriptor rec
cssBlanchedAlmond = 
  \factory -> factory.named "BlanchedAlmond" (ElmColor.rgb 255 235 205)

cssBlue : Color.NubColorDescriptor rec
cssBlue = 
  \factory -> factory.named "Blue" (ElmColor.rgb 0 0 255)

cssBlueViolet : Color.NubColorDescriptor rec
cssBlueViolet = 
  \factory -> factory.named "BlueViolet" (ElmColor.rgb 138 43 226)

cssBrown : Color.NubColorDescriptor rec
cssBrown = 
  \factory -> factory.named "Brown" (ElmColor.rgb 165 42 42)

cssBurlyWood : Color.NubColorDescriptor rec
cssBurlyWood = 
  \factory -> factory.named "BurlyWood" (ElmColor.rgb 222 184 135)

cssCadetBlue : Color.NubColorDescriptor rec
cssCadetBlue = 
  \factory -> factory.named "CadetBlue" (ElmColor.rgb 95 158 160)

cssChartreuse : Color.NubColorDescriptor rec
cssChartreuse = 
  \factory -> factory.named "Chartreuse" (ElmColor.rgb 127 255 0)

cssChocolate : Color.NubColorDescriptor rec
cssChocolate = 
  \factory -> factory.named "Chocolate" (ElmColor.rgb 210 105 30)

cssCoral : Color.NubColorDescriptor rec
cssCoral = 
  \factory -> factory.named "Coral" (ElmColor.rgb 255 127 80)

cssCornflowerBlue : Color.NubColorDescriptor rec
cssCornflowerBlue = 
  \factory -> factory.named "CornflowerBlue" (ElmColor.rgb 100 149 237)

cssCornsilk : Color.NubColorDescriptor rec
cssCornsilk = 
  \factory -> factory.named "Cornsilk" (ElmColor.rgb 255 248 220)

cssCrimson : Color.NubColorDescriptor rec
cssCrimson = 
  \factory -> factory.named "Crimson" (ElmColor.rgb 220 20 60)

cssCyan : Color.NubColorDescriptor rec
cssCyan = 
  \factory -> factory.named "Cyan" (ElmColor.rgb 0 255 255)

cssDarkBlue : Color.NubColorDescriptor rec
cssDarkBlue = 
  \factory -> factory.named "DarkBlue" (ElmColor.rgb 0 0 139)

cssDarkCyan : Color.NubColorDescriptor rec
cssDarkCyan = 
  \factory -> factory.named "DarkCyan" (ElmColor.rgb 0 139 139)

cssDarkGoldenRod : Color.NubColorDescriptor rec
cssDarkGoldenRod = 
  \factory -> factory.named "DarkGoldenRod" (ElmColor.rgb 184 134 11)

cssDarkGray : Color.NubColorDescriptor rec
cssDarkGray = 
  \factory -> factory.named "DarkGray" (ElmColor.rgb 169 169 169)

cssDarkGreen : Color.NubColorDescriptor rec
cssDarkGreen = 
  \factory -> factory.named "DarkGreen" (ElmColor.rgb 0 100 0)

cssDarkKhaki : Color.NubColorDescriptor rec
cssDarkKhaki = 
  \factory -> factory.named "DarkKhaki" (ElmColor.rgb 189 183 107)

cssDarkMagenta : Color.NubColorDescriptor rec
cssDarkMagenta = 
  \factory -> factory.named "DarkMagenta" (ElmColor.rgb 139 0 139)

cssDarkOliveGreen : Color.NubColorDescriptor rec
cssDarkOliveGreen = 
  \factory -> factory.named "DarkOliveGreen" (ElmColor.rgb 85 107 47)

cssDarkOrange : Color.NubColorDescriptor rec
cssDarkOrange = 
  \factory -> factory.named "DarkOrange" (ElmColor.rgb 255 140 0)

cssDarkOrchid : Color.NubColorDescriptor rec
cssDarkOrchid = 
  \factory -> factory.named "DarkOrchid" (ElmColor.rgb 153 50 204)

cssDarkRed : Color.NubColorDescriptor rec
cssDarkRed = 
  \factory -> factory.named "DarkRed" (ElmColor.rgb 139 0 0)

cssDarkSalmon : Color.NubColorDescriptor rec
cssDarkSalmon = 
  \factory -> factory.named "DarkSalmon" (ElmColor.rgb 233 150 122)

cssDarkSeaGreen : Color.NubColorDescriptor rec
cssDarkSeaGreen = 
  \factory -> factory.named "DarkSeaGreen" (ElmColor.rgb 143 188 143)

cssDarkSlateBlue : Color.NubColorDescriptor rec
cssDarkSlateBlue = 
  \factory -> factory.named "DarkSlateBlue" (ElmColor.rgb 72 61 139)

cssDarkSlateGray : Color.NubColorDescriptor rec
cssDarkSlateGray = 
  \factory -> factory.named "DarkSlateGray" (ElmColor.rgb 47 79 79)

cssDarkTurquoise : Color.NubColorDescriptor rec
cssDarkTurquoise = 
  \factory -> factory.named "DarkTurquoise" (ElmColor.rgb 0 206 209)

cssDarkViolet : Color.NubColorDescriptor rec
cssDarkViolet = 
  \factory -> factory.named "DarkViolet" (ElmColor.rgb 148 0 211)

cssDeepPink : Color.NubColorDescriptor rec
cssDeepPink = 
  \factory -> factory.named "DeepPink" (ElmColor.rgb 255 20 147)

cssDeepSkyBlue : Color.NubColorDescriptor rec
cssDeepSkyBlue = 
  \factory -> factory.named "DeepSkyBlue" (ElmColor.rgb 0 191 255)

cssDimGray : Color.NubColorDescriptor rec
cssDimGray = 
  \factory -> factory.named "DimGray" (ElmColor.rgb 105 105 105)

cssDodgerBlue : Color.NubColorDescriptor rec
cssDodgerBlue = 
  \factory -> factory.named "DodgerBlue" (ElmColor.rgb 30 144 255)

cssFireBrick : Color.NubColorDescriptor rec
cssFireBrick = 
  \factory -> factory.named "FireBrick" (ElmColor.rgb 178 34 34)

cssFloralWhite : Color.NubColorDescriptor rec
cssFloralWhite = 
  \factory -> factory.named "FloralWhite" (ElmColor.rgb 255 250 240)

cssForestGreen : Color.NubColorDescriptor rec
cssForestGreen = 
  \factory -> factory.named "ForestGreen" (ElmColor.rgb 34 139 34)

cssFuchsia : Color.NubColorDescriptor rec
cssFuchsia = 
  \factory -> factory.named "Fuchsia" (ElmColor.rgb 255 0 255)

cssGainsboro : Color.NubColorDescriptor rec
cssGainsboro = 
  \factory -> factory.named "Gainsboro" (ElmColor.rgb 220 220 220)

cssGhostWhite : Color.NubColorDescriptor rec
cssGhostWhite = 
  \factory -> factory.named "GhostWhite" (ElmColor.rgb 248 248 255)

cssGold : Color.NubColorDescriptor rec
cssGold = 
  \factory -> factory.named "Gold" (ElmColor.rgb 255 215 0)

cssGoldenRod : Color.NubColorDescriptor rec
cssGoldenRod = 
  \factory -> factory.named "GoldenRod" (ElmColor.rgb 218 165 32)

cssGray : Color.NubColorDescriptor rec
cssGray = 
  \factory -> factory.named "Gray" (ElmColor.rgb 128 128 128)

cssGreen : Color.NubColorDescriptor rec
cssGreen = 
  \factory -> factory.named "Green" (ElmColor.rgb 0 128 0)

cssGreenYellow : Color.NubColorDescriptor rec
cssGreenYellow = 
  \factory -> factory.named "GreenYellow" (ElmColor.rgb 173 255 47)

cssHoneyDew : Color.NubColorDescriptor rec
cssHoneyDew = 
  \factory -> factory.named "HoneyDew" (ElmColor.rgb 240 255 240)

cssHotPink : Color.NubColorDescriptor rec
cssHotPink = 
  \factory -> factory.named "HotPink" (ElmColor.rgb 255 105 180)

cssIndianRed  : Color.NubColorDescriptor rec
cssIndianRed  = 
  \factory -> factory.named "IndianRed " (ElmColor.rgb 205 92 92)

cssIndigo  : Color.NubColorDescriptor rec
cssIndigo  = 
  \factory -> factory.named "Indigo " (ElmColor.rgb 75 0 130)

cssIvory : Color.NubColorDescriptor rec
cssIvory = 
  \factory -> factory.named "Ivory" (ElmColor.rgb 255 255 240)

cssKhaki : Color.NubColorDescriptor rec
cssKhaki = 
  \factory -> factory.named "Khaki" (ElmColor.rgb 240 230 140)

cssLavender : Color.NubColorDescriptor rec
cssLavender = 
  \factory -> factory.named "Lavender" (ElmColor.rgb 230 230 250)

cssLavenderBlush : Color.NubColorDescriptor rec
cssLavenderBlush = 
  \factory -> factory.named "LavenderBlush" (ElmColor.rgb 255 240 245)

cssLawnGreen : Color.NubColorDescriptor rec
cssLawnGreen = 
  \factory -> factory.named "LawnGreen" (ElmColor.rgb 124 252 0)

cssLemonChiffon : Color.NubColorDescriptor rec
cssLemonChiffon = 
  \factory -> factory.named "LemonChiffon" (ElmColor.rgb 255 250 205)

cssLightBlue : Color.NubColorDescriptor rec
cssLightBlue = 
  \factory -> factory.named "LightBlue" (ElmColor.rgb 173 216 230)

cssLightCoral : Color.NubColorDescriptor rec
cssLightCoral = 
  \factory -> factory.named "LightCoral" (ElmColor.rgb 240 128 128)

cssLightCyan : Color.NubColorDescriptor rec
cssLightCyan = 
  \factory -> factory.named "LightCyan" (ElmColor.rgb 224 255 255)

cssLightGoldenRodYellow : Color.NubColorDescriptor rec
cssLightGoldenRodYellow = 
  \factory -> factory.named "LightGoldenRodYellow" (ElmColor.rgb 250 250 210)

cssLightGray : Color.NubColorDescriptor rec
cssLightGray = 
  \factory -> factory.named "LightGray" (ElmColor.rgb 211 211 211)

cssLightGreen : Color.NubColorDescriptor rec
cssLightGreen = 
  \factory -> factory.named "LightGreen" (ElmColor.rgb 144 238 144)

cssLightPink : Color.NubColorDescriptor rec
cssLightPink = 
  \factory -> factory.named "LightPink" (ElmColor.rgb 255 182 193)

cssLightSalmon : Color.NubColorDescriptor rec
cssLightSalmon = 
  \factory -> factory.named "LightSalmon" (ElmColor.rgb 255 160 122)

cssLightSeaGreen : Color.NubColorDescriptor rec
cssLightSeaGreen = 
  \factory -> factory.named "LightSeaGreen" (ElmColor.rgb 32 178 170)

cssLightSkyBlue : Color.NubColorDescriptor rec
cssLightSkyBlue = 
  \factory -> factory.named "LightSkyBlue" (ElmColor.rgb 135 206 250)

cssLightSlateGray : Color.NubColorDescriptor rec
cssLightSlateGray = 
  \factory -> factory.named "LightSlateGray" (ElmColor.rgb 119 136 153)

cssLightSteelBlue : Color.NubColorDescriptor rec
cssLightSteelBlue = 
  \factory -> factory.named "LightSteelBlue" (ElmColor.rgb 176 196 222)

cssLightYellow : Color.NubColorDescriptor rec
cssLightYellow = 
  \factory -> factory.named "LightYellow" (ElmColor.rgb 255 255 224)

cssLime : Color.NubColorDescriptor rec
cssLime = 
  \factory -> factory.named "Lime" (ElmColor.rgb 0 255 0)

cssLimeGreen : Color.NubColorDescriptor rec
cssLimeGreen = 
  \factory -> factory.named "LimeGreen" (ElmColor.rgb 50 205 50)

cssLinen : Color.NubColorDescriptor rec
cssLinen = 
  \factory -> factory.named "Linen" (ElmColor.rgb 250 240 230)

cssMagenta : Color.NubColorDescriptor rec
cssMagenta = 
  \factory -> factory.named "Magenta" (ElmColor.rgb 255 0 255)

cssMaroon : Color.NubColorDescriptor rec
cssMaroon = 
  \factory -> factory.named "Maroon" (ElmColor.rgb 128 0 0)

cssMediumAquaMarine : Color.NubColorDescriptor rec
cssMediumAquaMarine = 
  \factory -> factory.named "MediumAquaMarine" (ElmColor.rgb 102 205 170)

cssMediumBlue : Color.NubColorDescriptor rec
cssMediumBlue = 
  \factory -> factory.named "MediumBlue" (ElmColor.rgb 0 0 205)

cssMediumOrchid : Color.NubColorDescriptor rec
cssMediumOrchid = 
  \factory -> factory.named "MediumOrchid" (ElmColor.rgb 186 85 211)

cssMediumPurple : Color.NubColorDescriptor rec
cssMediumPurple = 
  \factory -> factory.named "MediumPurple" (ElmColor.rgb 147 112 219)

cssMediumSeaGreen : Color.NubColorDescriptor rec
cssMediumSeaGreen = 
  \factory -> factory.named "MediumSeaGreen" (ElmColor.rgb 60 179 113)

cssMediumSlateBlue : Color.NubColorDescriptor rec
cssMediumSlateBlue = 
  \factory -> factory.named "MediumSlateBlue" (ElmColor.rgb 123 104 238)

cssMediumSpringGreen : Color.NubColorDescriptor rec
cssMediumSpringGreen = 
  \factory -> factory.named "MediumSpringGreen" (ElmColor.rgb 0 250 154)

cssMediumTurquoise : Color.NubColorDescriptor rec
cssMediumTurquoise = 
  \factory -> factory.named "MediumTurquoise" (ElmColor.rgb 72 209 204)

cssMediumVioletRed : Color.NubColorDescriptor rec
cssMediumVioletRed = 
  \factory -> factory.named "MediumVioletRed" (ElmColor.rgb 199 21 133)

cssMidnightBlue : Color.NubColorDescriptor rec
cssMidnightBlue = 
  \factory -> factory.named "MidnightBlue" (ElmColor.rgb 25 25 112)

cssMintCream : Color.NubColorDescriptor rec
cssMintCream = 
  \factory -> factory.named "MintCream" (ElmColor.rgb 245 255 250)

cssMistyRose : Color.NubColorDescriptor rec
cssMistyRose = 
  \factory -> factory.named "MistyRose" (ElmColor.rgb 255 228 225)

cssMoccasin : Color.NubColorDescriptor rec
cssMoccasin = 
  \factory -> factory.named "Moccasin" (ElmColor.rgb 255 228 181)

cssNavajoWhite : Color.NubColorDescriptor rec
cssNavajoWhite = 
  \factory -> factory.named "NavajoWhite" (ElmColor.rgb 255 222 173)

cssNavy : Color.NubColorDescriptor rec
cssNavy = 
  \factory -> factory.named "Navy" (ElmColor.rgb 0 0 128)

cssOldLace : Color.NubColorDescriptor rec
cssOldLace = 
  \factory -> factory.named "OldLace" (ElmColor.rgb 253 245 230)

cssOlive : Color.NubColorDescriptor rec
cssOlive = 
  \factory -> factory.named "Olive" (ElmColor.rgb 128 128 0)

cssOliveDrab : Color.NubColorDescriptor rec
cssOliveDrab = 
  \factory -> factory.named "OliveDrab" (ElmColor.rgb 107 142 35)

cssOrange : Color.NubColorDescriptor rec
cssOrange = 
  \factory -> factory.named "Orange" (ElmColor.rgb 255 165 0)

cssOrangeRed : Color.NubColorDescriptor rec
cssOrangeRed = 
  \factory -> factory.named "OrangeRed" (ElmColor.rgb 255 69 0)

cssOrchid : Color.NubColorDescriptor rec
cssOrchid = 
  \factory -> factory.named "Orchid" (ElmColor.rgb 218 112 214)

cssPaleGoldenRod : Color.NubColorDescriptor rec
cssPaleGoldenRod = 
  \factory -> factory.named "PaleGoldenRod" (ElmColor.rgb 238 232 170)

cssPaleGreen : Color.NubColorDescriptor rec
cssPaleGreen = 
  \factory -> factory.named "PaleGreen" (ElmColor.rgb 152 251 152)

cssPaleTurquoise : Color.NubColorDescriptor rec
cssPaleTurquoise = 
  \factory -> factory.named "PaleTurquoise" (ElmColor.rgb 175 238 238)

cssPaleVioletRed : Color.NubColorDescriptor rec
cssPaleVioletRed = 
  \factory -> factory.named "PaleVioletRed" (ElmColor.rgb 219 112 147)

cssPapayaWhip : Color.NubColorDescriptor rec
cssPapayaWhip = 
  \factory -> factory.named "PapayaWhip" (ElmColor.rgb 255 239 213)

cssPeachPuff : Color.NubColorDescriptor rec
cssPeachPuff = 
  \factory -> factory.named "PeachPuff" (ElmColor.rgb 255 218 185)

cssPeru : Color.NubColorDescriptor rec
cssPeru = 
  \factory -> factory.named "Peru" (ElmColor.rgb 205 133 63)

cssPink : Color.NubColorDescriptor rec
cssPink = 
  \factory -> factory.named "Pink" (ElmColor.rgb 255 192 203)

cssPlum : Color.NubColorDescriptor rec
cssPlum = 
  \factory -> factory.named "Plum" (ElmColor.rgb 221 160 221)

cssPowderBlue : Color.NubColorDescriptor rec
cssPowderBlue = 
  \factory -> factory.named "PowderBlue" (ElmColor.rgb 176 224 230)

cssPurple : Color.NubColorDescriptor rec
cssPurple = 
  \factory -> factory.named "Purple" (ElmColor.rgb 128 0 128)

cssRebeccaPurple : Color.NubColorDescriptor rec
cssRebeccaPurple = 
  \factory -> factory.named "RebeccaPurple" (ElmColor.rgb 102 51 153)

cssRed : Color.NubColorDescriptor rec
cssRed = 
  \factory -> factory.named "Red" (ElmColor.rgb 255 0 0)

cssRosyBrown : Color.NubColorDescriptor rec
cssRosyBrown = 
  \factory -> factory.named "RosyBrown" (ElmColor.rgb 188 143 143)

cssRoyalBlue : Color.NubColorDescriptor rec
cssRoyalBlue = 
  \factory -> factory.named "RoyalBlue" (ElmColor.rgb 65 105 225)

cssSaddleBrown : Color.NubColorDescriptor rec
cssSaddleBrown = 
  \factory -> factory.named "SaddleBrown" (ElmColor.rgb 139 69 19)

cssSalmon : Color.NubColorDescriptor rec
cssSalmon = 
  \factory -> factory.named "Salmon" (ElmColor.rgb 250 128 114)

cssSandyBrown : Color.NubColorDescriptor rec
cssSandyBrown = 
  \factory -> factory.named "SandyBrown" (ElmColor.rgb 244 164 96)

cssSeaGreen : Color.NubColorDescriptor rec
cssSeaGreen = 
  \factory -> factory.named "SeaGreen" (ElmColor.rgb 46 139 87)

cssSeaShell : Color.NubColorDescriptor rec
cssSeaShell = 
  \factory -> factory.named "SeaShell" (ElmColor.rgb 255 245 238)

cssSienna : Color.NubColorDescriptor rec
cssSienna = 
  \factory -> factory.named "Sienna" (ElmColor.rgb 160 82 45)

cssSilver : Color.NubColorDescriptor rec
cssSilver = 
  \factory -> factory.named "Silver" (ElmColor.rgb 192 192 192)

cssSkyBlue : Color.NubColorDescriptor rec
cssSkyBlue = 
  \factory -> factory.named "SkyBlue" (ElmColor.rgb 135 206 235)

cssSlateBlue : Color.NubColorDescriptor rec
cssSlateBlue = 
  \factory -> factory.named "SlateBlue" (ElmColor.rgb 106 90 205)

cssSlateGray : Color.NubColorDescriptor rec
cssSlateGray = 
  \factory -> factory.named "SlateGray" (ElmColor.rgb 112 128 144)

cssSnow : Color.NubColorDescriptor rec
cssSnow = 
  \factory -> factory.named "Snow" (ElmColor.rgb 255 250 250)

cssSpringGreen : Color.NubColorDescriptor rec
cssSpringGreen = 
  \factory -> factory.named "SpringGreen" (ElmColor.rgb 0 255 127)

cssSteelBlue : Color.NubColorDescriptor rec
cssSteelBlue = 
  \factory -> factory.named "SteelBlue" (ElmColor.rgb 70 130 180)

cssTan : Color.NubColorDescriptor rec
cssTan = 
  \factory -> factory.named "Tan" (ElmColor.rgb 210 180 140)

cssTeal : Color.NubColorDescriptor rec
cssTeal = 
  \factory -> factory.named "Teal" (ElmColor.rgb 0 128 128)

cssThistle : Color.NubColorDescriptor rec
cssThistle = 
  \factory -> factory.named "Thistle" (ElmColor.rgb 216 191 216)

cssTomato : Color.NubColorDescriptor rec
cssTomato = 
  \factory -> factory.named "Tomato" (ElmColor.rgb 255 99 71)

cssTurquoise : Color.NubColorDescriptor rec
cssTurquoise = 
  \factory -> factory.named "Turquoise" (ElmColor.rgb 64 224 208)

cssViolet : Color.NubColorDescriptor rec
cssViolet = 
  \factory -> factory.named "Violet" (ElmColor.rgb 238 130 238)

cssWheat : Color.NubColorDescriptor rec
cssWheat = 
  \factory -> factory.named "Wheat" (ElmColor.rgb 245 222 179)

cssWhite : Color.NubColorDescriptor rec
cssWhite = 
  \factory -> factory.named "White" (ElmColor.rgb 255 255 255)

cssWhiteSmoke : Color.NubColorDescriptor rec
cssWhiteSmoke = 
  \factory -> factory.named "WhiteSmoke" (ElmColor.rgb 245 245 245)

cssYellow : Color.NubColorDescriptor rec
cssYellow = 
  \factory -> factory.named "Yellow" (ElmColor.rgb 255 255 0)

cssYellowGreen : Color.NubColorDescriptor rec
cssYellowGreen = 
  \factory -> factory.named "YellowGreen" (ElmColor.rgb 154 205 50)

-------------------------------------------------------------------------------
-- Elm colors.

red : Color.NubColorDescriptor rec
red = \factory -> ElmColor.red |> factory.rgbaColor

orange : Color.NubColorDescriptor rec
orange = \factory -> ElmColor.orange |> factory.rgbaColor

yellow : Color.NubColorDescriptor rec
yellow = \factory -> ElmColor.yellow |> factory.rgbaColor

green : Color.NubColorDescriptor rec
green = \factory -> ElmColor.green |> factory.rgbaColor

blue : Color.NubColorDescriptor rec
blue = \factory -> ElmColor.blue |> factory.rgbaColor

purple : Color.NubColorDescriptor rec
purple = \factory -> ElmColor.purple |> factory.rgbaColor

brown : Color.NubColorDescriptor rec
brown = \factory -> ElmColor.brown |> factory.rgbaColor



lightRed : Color.NubColorDescriptor rec
lightRed = \factory -> ElmColor.lightRed |> factory.rgbaColor

lightOrange : Color.NubColorDescriptor rec
lightOrange = \factory -> ElmColor.lightOrange |> factory.rgbaColor

lightYellow : Color.NubColorDescriptor rec
lightYellow = \factory -> ElmColor.lightYellow |> factory.rgbaColor

lightGreen : Color.NubColorDescriptor rec
lightGreen = \factory -> ElmColor.lightGreen |> factory.rgbaColor

lightBlue : Color.NubColorDescriptor rec
lightBlue = \factory -> ElmColor.lightBlue |> factory.rgbaColor

lightPurple : Color.NubColorDescriptor rec
lightPurple = \factory -> ElmColor.lightPurple |> factory.rgbaColor

lightBrown : Color.NubColorDescriptor rec
lightBrown = \factory -> ElmColor.lightBrown |> factory.rgbaColor



darkRed : Color.NubColorDescriptor rec
darkRed = \factory -> ElmColor.darkRed |> factory.rgbaColor

darkOrange : Color.NubColorDescriptor rec
darkOrange = \factory -> ElmColor.darkOrange |> factory.rgbaColor

darkYellow : Color.NubColorDescriptor rec
darkYellow = \factory -> ElmColor.darkYellow |> factory.rgbaColor

darkGreen : Color.NubColorDescriptor rec
darkGreen = \factory -> ElmColor.darkGreen |> factory.rgbaColor

darkBlue : Color.NubColorDescriptor rec
darkBlue = \factory -> ElmColor.darkBlue |> factory.rgbaColor

darkPurple : Color.NubColorDescriptor rec
darkPurple = \factory -> ElmColor.darkPurple |> factory.rgbaColor

darkBrown : Color.NubColorDescriptor rec
darkBrown = \factory -> ElmColor.darkBrown |> factory.rgbaColor



white : Color.NubColorDescriptor rec
white = \factory -> ElmColor.white |> factory.rgbaColor

lightGrey : Color.NubColorDescriptor rec
lightGrey = \factory -> ElmColor.lightGrey |> factory.rgbaColor

lightGray : Color.NubColorDescriptor rec
lightGray = \factory -> ElmColor.lightGray |> factory.rgbaColor

grey : Color.NubColorDescriptor rec
grey = \factory -> ElmColor.grey |> factory.rgbaColor

gray : Color.NubColorDescriptor rec
gray = \factory -> ElmColor.gray |> factory.rgbaColor

darkGrey : Color.NubColorDescriptor rec
darkGrey = \factory -> ElmColor.darkGrey |> factory.rgbaColor

darkGray : Color.NubColorDescriptor rec
darkGray = \factory -> ElmColor.darkGray |> factory.rgbaColor

lightCharcoal : Color.NubColorDescriptor rec 
lightCharcoal = \factory -> ElmColor.lightCharcoal |> factory.rgbaColor

charcoal : Color.NubColorDescriptor rec
charcoal = \factory -> ElmColor.charcoal |> factory.rgbaColor

darkCharcoal : Color.NubColorDescriptor rec
darkCharcoal = \factory -> ElmColor.darkCharcoal |> factory.rgbaColor

black : Color.NubColorDescriptor rec
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
-- TODO simplify this.
disc : List.ListStyleTypeDescriptor
disc = \factory -> factory.disc

armenian : List.ListStyleTypeDescriptor
armenian = \factory -> factory.armenian

circleListStyle : List.ListStyleTypeDescriptor
circleListStyle = \factory -> factory.circleListStyleType

cjkIdeographic : List.ListStyleTypeDescriptor
cjkIdeographic = \factory -> factory.cjkIdeographic

decimal : List.ListStyleTypeDescriptor
decimal = \factory -> factory.decimal

decimalLeadingZero : List.ListStyleTypeDescriptor
decimalLeadingZero = \factory -> factory.decimalLeadingZero

georgian : List.ListStyleTypeDescriptor
georgian = \factory -> factory.georgian

hebrew : List.ListStyleTypeDescriptor
hebrew = \factory -> factory.hebrew

hiragana : List.ListStyleTypeDescriptor
hiragana = \factory -> factory.hiragana

hiraganaIroha : List.ListStyleTypeDescriptor
hiraganaIroha = \factory -> factory.hiraganaIroha

katakana : List.ListStyleTypeDescriptor
katakana = \factory -> factory.katakana

katakanaIroha : List.ListStyleTypeDescriptor
katakanaIroha = \factory -> factory.katakanaIroha

lowerAlpha : List.ListStyleTypeDescriptor
lowerAlpha = \factory -> factory.lowerAlpha

lowerGreek : List.ListStyleTypeDescriptor
lowerGreek = \factory -> factory.lowerGreek

lowerLatin : List.ListStyleTypeDescriptor
lowerLatin = \factory -> factory.lowerLatin

lowerRoman : List.ListStyleTypeDescriptor
lowerRoman = \factory -> factory.lowerRoman

square : List.ListStyleTypeDescriptor
square = \factory -> factory.square

upperAlpha : List.ListStyleTypeDescriptor
upperAlpha = \factory -> factory.upperAlpha

upperLatin : List.ListStyleTypeDescriptor
upperLatin = \factory -> factory.upperLatin

upperRoman : List.ListStyleTypeDescriptor
upperRoman = \factory -> factory.upperRoman
