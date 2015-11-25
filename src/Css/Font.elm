module Css.Font
  (
  -- * Color.

    fontColor
  , color

  -- * Font-family.

  , fontFamily, family, families
  , sansSerif, serif, monospace, cursive, fantasy

  -- * Font-size.

  , fontSize
  , xxSmall, xSmall, small, medium, large, xLarge, xxLarge, smaller, larger

  -- * Font-style

  , fontStyle
  , italic, oblique

  -- * Font-variant.

  , fontVariant
  , smallCaps

  -- * Font-weight

  , fontWeight
  , bold, bolder, lighter
  , weight

  -- * Line-height.

  , lineHeight
  
  -- * Generic font property.
    
  , font
  , aFont, withLineHeight, withWeight, withVariant, withStyle

  -- * Named fonts.

  , caption, icon, menu, messageBox, smallCaption, statusBar
  
  ) where

import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Stylesheet as Stylesheet
import Css.Internal.Typography.Font as Font
import Css.Internal.Typography.Font.Family as Family
import Css.Internal.Typography.Font.Size as FontSize
import Css.Internal.Typography.Font.Style as FontStyle
import Css.Internal.Typography.Font.Variant as Variant
import Css.Internal.Typography.Font.Weight as Weight

-------------------------------------------------------------------------------

color : Color.ColorDescriptor -> Stylesheet.PropertyRuleAppender
color colorDescriptor = 
  let theColor = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "color" theColor

-- | An alias for `color`.
fontColor : Color.ColorDescriptor -> Stylesheet.PropertyRuleAppender
fontColor = color

-------------------------------------------------------------------------------
  
fontFamily : Family.FontFamilyDescriptor -> 
             Stylesheet.PropertyRuleAppender
fontFamily familyDescriptor = 
  let familiesValue = familyDescriptor Family.fontFamilyFactory
  in Stylesheet.simpleProperty "font-family" familiesValue

family : String -> Family.FontFamilyDescriptor
family familyName = \factory -> factory.customFamily familyName
  
families : List String -> 
           List Family.GenericFontFamilyDescriptor -> 
           Family.FontFamilyDescriptor
families customFamilyNames genericFamilyDescriptors =
  \factory -> factory.families customFamilyNames genericFamilyDescriptors      

sansSerif : Family.GenericFontFamilyDescriptor 
sansSerif = \factory -> factory.family "sans-serif"


serif : Family.GenericFontFamilyDescriptor 
serif = \factory -> factory.family "serif"


monospace : Family.GenericFontFamilyDescriptor 
monospace = \factory -> factory.family "monospace"


cursive : Family.GenericFontFamilyDescriptor 
cursive = \factory -> factory.family "cursive"


fantasy : Family.GenericFontFamilyDescriptor
fantasy = \factory -> factory.family "fantasy"


-------------------------------------------------------------------------------


fontSize : FontSize.FontSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
fontSize sizeDescriptor = 
  let fontSizeValue = sizeDescriptor FontSize.fontSizeFactory
  in Stylesheet.simpleProperty "font-size" fontSizeValue


xxSmall : FontSize.FontSizeDescriptor sz 
xxSmall = \factory -> factory.fontSize "xx-small"


xSmall : FontSize.FontSizeDescriptor sz  
xSmall = \factory -> factory.fontSize "x-small"


small : FontSize.FontSizeDescriptor sz 
small = \factory -> factory.fontSize "small"


medium : FontSize.FontSizeDescriptor sz
medium = \factory -> factory.fontSize "medium"


large : FontSize.FontSizeDescriptor sz
large = \factory -> factory.fontSize "large"


xLarge : FontSize.FontSizeDescriptor sz 
xLarge = \factory -> factory.fontSize "x-large"


xxLarge : FontSize.FontSizeDescriptor sz
xxLarge = \factory -> factory.fontSize "xx-large"


smaller : FontSize.FontSizeDescriptor sz 
smaller = \factory -> factory.fontSize "smaller"


larger : FontSize.FontSizeDescriptor sz
larger = \factory -> factory.fontSize "larger"


-------------------------------------------------------------------------------


fontStyle : FontStyle.FontStyleDescriptor -> Stylesheet.PropertyRuleAppender
fontStyle styleDescriptor = 
  let fontStyleValue = styleDescriptor FontStyle.fontStyleFactory
  in Stylesheet.simpleProperty "font-style" fontStyleValue


italic : FontStyle.NubFontStyleDescriptor rec
italic = \factory -> factory.style "italic"


oblique : FontStyle.NubFontStyleDescriptor rec
oblique = \factory -> factory.style "oblique"


-------------------------------------------------------------------------------

-- TODO - Need a second version to take a list of variants
fontVariant : Variant.FontVariantDescriptor -> Stylesheet.PropertyRuleAppender
fontVariant variantDescriptor = 
  let fontVariantValue = variantDescriptor Variant.fontVariantFactory
  in Stylesheet.simpleProperty "font-variant" fontVariantValue

smallCaps : Variant.NubFontVariantDescriptor rec
smallCaps = \factory -> factory.variant "small-caps"

-- TODO - There are many more of these now.
-- See https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant
-------------------------------------------------------------------------------

fontWeight : Weight.FontWeightDescriptor -> Stylesheet.PropertyRuleAppender
fontWeight descriptor = 
  let fontWeightValue = descriptor Weight.fontWeightFactory
  in Stylesheet.simpleProperty "font-weight" fontWeightValue


bold : Weight.NubFontWeightDescriptor rec
bold = \factory -> factory.weight "bold"


bolder : Weight.NubFontWeightDescriptor rec
bolder = \factory -> factory.weight "bolder"


lighter : Weight.NubFontWeightDescriptor rec
lighter = \factory -> factory.weight "lighter"


weight : Int -> Weight.NubFontWeightDescriptor rec
weight i = \factory -> factory.weight (toString i)

-------------------------------------------------------------------------------

lineHeight : Linear.SizeDescriptorWithNormal sz -> 
             Stylesheet.PropertyRuleAppender
lineHeight descriptor = 
  let lineHeightValue = descriptor Linear.sizeFactoryWithNormal
  in Stylesheet.simpleProperty "line-height" lineHeightValue

-------------------------------------------------------------------------------

font : Font.FontDescriptor a sz -> Stylesheet.PropertyRuleAppender
font fontDescriptor = 
  let value = fontDescriptor Font.fontFactory |> Font.fontValue
  in Stylesheet.simpleProperty "font" value

aFont : Linear.NubSizeDescriptor {} sz -> 
        List String -> 
        List Family.GenericFontFamilyDescriptor -> 
        Font.ComposedFontDescriptor sz
aFont sizeDescriptor customFonts genericFontDescriptors =
  \compositeFactory -> 
    let genericFontFrom descriptor = descriptor Family.genericFontFamilyFactory
        genericFonts = List.map genericFontFrom genericFontDescriptors
    in compositeFactory.leaf sizeDescriptor customFonts genericFonts

withLineHeight : Linear.NubSizeDescriptor {} sz -> 
                 Font.ComposedFontDescriptor sz -> 
                 Font.ComposedFontDescriptor sz
withLineHeight lineHeightDescriptor innerDescriptor =
  \compositeFactory ->
    let lineHeight = lineHeightDescriptor Linear.nubSizeFactory
    in Font.addLineHeight lineHeight innerDescriptor compositeFactory

withWeight : Weight.NubFontWeightDescriptor {} -> 
             Font.ComposedFontDescriptor sz -> 
             Font.ComposedFontDescriptor sz
withWeight weightDescriptor innerDescriptor = 
  \compositeFactory ->
     let weight = weightDescriptor Weight.nubFontWeightFactory 
     in Font.addWeight weight innerDescriptor compositeFactory
  
withVariant : Variant.NubFontVariantDescriptor {} -> 
              Font.ComposedFontDescriptor sz -> 
              Font.ComposedFontDescriptor sz
withVariant variantDescriptor innerDescriptor = 
  \compositeFactory -> 
     let variant = variantDescriptor Variant.nubFontVariantFactory
     in Font.addVariant variant innerDescriptor compositeFactory

withStyle : FontStyle.NubFontStyleDescriptor {} -> 
            Font.ComposedFontDescriptor sz -> 
            Font.ComposedFontDescriptor sz
withStyle styleDescriptor innerDescriptor = 
  \compositeFactory ->
     let style = styleDescriptor FontStyle.nubFontStyleFactory
     in Font.addStyle style innerDescriptor compositeFactory

caption : Font.FontDescriptor {} sz
caption = \factory -> factory.named "caption"

icon : Font.FontDescriptor {} sz
icon = \factory -> factory.named "icon"

menu : Font.FontDescriptor {} sz
menu = \factory -> factory.named "menu"

messageBox : Font.FontDescriptor {} sz
messageBox = \factory -> factory.named "message-box"

smallCaption : Font.FontDescriptor {} sz
smallCaption = \factory -> factory.named "small-caption"

statusBar : Font.FontDescriptor {} sz
statusBar = \factory -> factory.named "status-bar"
