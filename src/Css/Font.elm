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
import Css.Internal.Font as Font
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

color : Color.ColorDescriptor -> Stylesheet.PropertyRuleAppender
color colorDescriptor = 
  let theColor = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "color" theColor

-- | An alias for `color`.
fontColor : Color.ColorDescriptor -> Stylesheet.PropertyRuleAppender
fontColor = color

-------------------------------------------------------------------------------
  
fontFamily : Font.FontFamilyDescriptor -> 
             Stylesheet.PropertyRuleAppender
fontFamily familyDescriptor = 
  let familiesValue = familyDescriptor Font.fontFamilyFactory
  in Stylesheet.simpleProperty "font-family" familiesValue

family : String -> Font.FontFamilyDescriptor
family familyName = \factory -> factory.customFamily familyName
  
families : List String -> 
           List Font.GenericFontFamilyDescriptor -> 
           Font.FontFamilyDescriptor
families customFamilyNames genericFamilyDescriptors =
  \factory -> factory.families customFamilyNames genericFamilyDescriptors      

sansSerif : Font.GenericFontFamilyDescriptor 
sansSerif = \factory -> factory.family "sans-serif"


serif : Font.GenericFontFamilyDescriptor 
serif = \factory -> factory.family "serif"


monospace : Font.GenericFontFamilyDescriptor 
monospace = \factory -> factory.family "monospace"


cursive : Font.GenericFontFamilyDescriptor 
cursive = \factory -> factory.family "cursive"


fantasy : Font.GenericFontFamilyDescriptor
fantasy = \factory -> factory.family "fantasy"


-------------------------------------------------------------------------------


fontSize : Font.FontSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
fontSize sizeDescriptor = 
  let fontSizeValue = sizeDescriptor Font.fontSizeFactory
  in Stylesheet.simpleProperty "font-size" fontSizeValue


xxSmall : Font.FontSizeDescriptor sz 
xxSmall = \factory -> factory.fontSize "xx-small"


xSmall : Font.FontSizeDescriptor sz  
xSmall = \factory -> factory.fontSize "x-small"


small : Font.FontSizeDescriptor sz 
small = \factory -> factory.fontSize "small"


medium : Font.FontSizeDescriptor sz
medium = \factory -> factory.fontSize "medium"


large : Font.FontSizeDescriptor sz
large = \factory -> factory.fontSize "large"


xLarge : Font.FontSizeDescriptor sz 
xLarge = \factory -> factory.fontSize "x-large"


xxLarge : Font.FontSizeDescriptor sz
xxLarge = \factory -> factory.fontSize "xx-large"


smaller : Font.FontSizeDescriptor sz 
smaller = \factory -> factory.fontSize "smaller"


larger : Font.FontSizeDescriptor sz
larger = \factory -> factory.fontSize "larger"


-------------------------------------------------------------------------------


fontStyle : Font.FontStyleDescriptor -> Stylesheet.PropertyRuleAppender
fontStyle styleDescriptor = 
  let fontStyleValue = styleDescriptor Font.fontStyleFactory
  in Stylesheet.simpleProperty "font-style" fontStyleValue


italic : Font.NubFontStyleDescriptor rec
italic = \factory -> factory.style "italic"


oblique : Font.NubFontStyleDescriptor rec
oblique = \factory -> factory.style "oblique"


-------------------------------------------------------------------------------

-- TODO - Need a second version to take a list of variants
fontVariant : Font.FontVariantDescriptor -> Stylesheet.PropertyRuleAppender
fontVariant variantDescriptor = 
  let fontVariantValue = variantDescriptor Font.fontVariantFactory
  in Stylesheet.simpleProperty "font-variant" fontVariantValue

smallCaps : Font.NubFontVariantDescriptor rec
smallCaps = \factory -> factory.variant "small-caps"

-- TODO - There are many more of these now.
-- See https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant
-------------------------------------------------------------------------------

fontWeight : Font.FontWeightDescriptor -> Stylesheet.PropertyRuleAppender
fontWeight descriptor = 
  let fontWeightValue = descriptor Font.fontWeightFactory
  in Stylesheet.simpleProperty "font-weight" fontWeightValue


bold : Font.NubFontWeightDescriptor rec
bold = \factory -> factory.weight "bold"


bolder : Font.NubFontWeightDescriptor rec
bolder = \factory -> factory.weight "bolder"


lighter : Font.NubFontWeightDescriptor rec
lighter = \factory -> factory.weight "lighter"


weight : Int -> Font.NubFontWeightDescriptor rec
weight i = \factory -> factory.weight (toString i)

-------------------------------------------------------------------------------

lineHeight : Linear.SizeDescriptorWithNormal sz -> Stylesheet.PropertyRuleAppender
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
        List Font.GenericFontFamilyDescriptor -> 
        Font.ComposedFontDescriptor sz
aFont sizeDescriptor customFonts genericFontDescriptors =
  \compositeFactory -> 
    let genericFontFrom descriptor = descriptor Font.genericFontFamilyFactory
        genericFonts = List.map genericFontFrom genericFontDescriptors
    in compositeFactory.leaf sizeDescriptor customFonts genericFonts

withLineHeight : Linear.NubSizeDescriptor {} sz -> 
                 Font.ComposedFontDescriptor sz -> 
                 Font.ComposedFontDescriptor sz
withLineHeight lineHeightDescriptor innerDescriptor =
  \compositeFactory ->
    let lineHeight = lineHeightDescriptor Linear.nubSizeFactory
    in Font.addLineHeight lineHeight innerDescriptor compositeFactory

withWeight : Font.NubFontWeightDescriptor {} -> 
             Font.ComposedFontDescriptor sz -> 
             Font.ComposedFontDescriptor sz
withWeight weightDescriptor innerDescriptor = 
  \compositeFactory ->
     let weight = weightDescriptor Font.nubFontWeightFactory 
     in Font.addWeight weight innerDescriptor compositeFactory
  
withVariant : Font.NubFontVariantDescriptor {} -> 
              Font.ComposedFontDescriptor sz -> 
              Font.ComposedFontDescriptor sz
withVariant variantDescriptor innerDescriptor = 
  \compositeFactory -> 
     let variant = variantDescriptor Font.nubFontVariantFactory
     in Font.addVariant variant innerDescriptor compositeFactory

withStyle : Font.NubFontStyleDescriptor {} -> 
            Font.ComposedFontDescriptor sz -> 
            Font.ComposedFontDescriptor sz
withStyle styleDescriptor innerDescriptor = 
  \compositeFactory ->
     let style = styleDescriptor Font.nubFontStyleFactory
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
