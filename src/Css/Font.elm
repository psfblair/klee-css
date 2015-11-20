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

color : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
color colorDescriptor = 
  let theColor = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "color" theColor

-- | An alias for `color`.
fontColor : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
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


-- TODO Test that we can pass size descriptors here too.
fontSize : Font.FontSizeDescriptor -> Stylesheet.PropertyRuleAppender
fontSize sizeDescriptor = 
  let fontSizeValue = sizeDescriptor Font.fontSizeFactory
  in Stylesheet.simpleProperty "font-size" fontSizeValue


xxSmall : Font.FontSizeDescriptor
xxSmall = \factory -> factory.size "xx-small"


xSmall : Font.FontSizeDescriptor 
xSmall = \factory -> factory.size "x-small"


small : Font.FontSizeDescriptor 
small = \factory -> factory.size "small"


medium : Font.FontSizeDescriptor
medium = \factory -> factory.size "medium"


large : Font.FontSizeDescriptor 
large = \factory -> factory.size "large"


xLarge : Font.FontSizeDescriptor 
xLarge = \factory -> factory.size "x-large"


xxLarge : Font.FontSizeDescriptor
xxLarge = \factory -> factory.size "xx-large"


smaller : Font.FontSizeDescriptor 
smaller = \factory -> factory.size "smaller"


larger : Font.FontSizeDescriptor
larger = \factory -> factory.size "larger"


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

aFont : Linear.SizeDescriptor {} sz -> 
           List String -> 
           List Font.GenericFontFamilyDescriptor -> 
           Font.ComposedFontDescriptor sz
aFont sizeDescriptor customFonts genericFontDescriptors =
  \compositeFactory -> 
    let genericFontFrom descriptor = descriptor Font.genericFontFamilyFactory
        genericFonts = List.map genericFontFrom genericFontDescriptors
    in compositeFactory.leaf sizeDescriptor customFonts genericFonts

withLineHeight : Linear.SizeDescriptor {} sz -> 
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
