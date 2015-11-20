module Css.Font
  (
  -- * Color.

    fontColor
  , color

  -- * Font-family.

  , fontFamily
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
import Css.Internal.Property as Property
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
-- TODO Can take initial, inherit, unset
-- | The `fontFamily` style rule takes two lists of font families: zero or more
-- custom font-families and preferably one or more generic font families.
fontFamily : List String -> 
             List Font.GenericFontFamilyDescriptor -> 
             Stylesheet.PropertyRuleAppender
fontFamily customFamilies genericFamilies = 
  let customFontValues = 
        customFamilies 
        |> List.map Property.toLiteral 
        |> List.map Property.literalValue
        
      genericValueFrom genericFamilyDescriptor = 
        genericFamilyDescriptor Font.genericFontFamilyFactory
        
      genericValues = 
        genericFamilies
        |> List.map genericValueFrom 
        |> List.map Font.genericFontFamilyValue
        
      allValues = customFontValues ++ genericValues
      valueFactory = Property.commaListValue identity
      
   in Stylesheet.simpleProperty "font-family" (valueFactory allValues)

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


italic : Font.FontStyleDescriptor
italic = \factory -> factory.style "italic"


oblique : Font.FontStyleDescriptor
oblique = \factory -> factory.style "oblique"


-------------------------------------------------------------------------------

-- TODO - Need a second version to take a list of variants
fontVariant : Font.FontVariantDescriptor -> Stylesheet.PropertyRuleAppender
fontVariant variantDescriptor = 
  let fontVariantValue = variantDescriptor Font.fontVariantFactory
  in Stylesheet.simpleProperty "font-variant" fontVariantValue

smallCaps : Font.FontVariantDescriptor
smallCaps = \factory -> factory.variant "small-caps"

-- TODO - There are many more of these now.
-- See https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant
-------------------------------------------------------------------------------

fontWeight : Font.FontWeightDescriptor -> Stylesheet.PropertyRuleAppender
fontWeight descriptor = 
  let fontWeightValue = descriptor Font.fontWeightFactory
  in Stylesheet.simpleProperty "font-weight" fontWeightValue


bold : Font.FontWeightDescriptor
bold = \factory -> factory.weight "bold"


bolder : Font.FontWeightDescriptor 
bolder = \factory -> factory.weight "bolder"


lighter : Font.FontWeightDescriptor
lighter = \factory -> factory.weight "lighter"


weight : Int -> Font.FontWeightDescriptor
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
withLineHeight lineHeightDescriptor compositeDescriptor =
  \compositeFactory ->
    let composedFont = compositeDescriptor compositeFactory
    in Font.addLineHeight composedFont lineHeightDescriptor

withWeight : Font.FontWeightDescriptor -> 
             Font.ComposedFontDescriptor sz -> 
             Font.ComposedFontDescriptor sz
withWeight weightDescriptor innerDescriptor = 
  \compositeFactory ->
     let weight = weightDescriptor Font.fontWeightFactory 
     in Font.addWeight weight innerDescriptor compositeFactory
  
withVariant : Font.FontVariantDescriptor -> 
              Font.ComposedFontDescriptor sz -> 
              Font.ComposedFontDescriptor sz
withVariant variantDescriptor innerDescriptor = 
  \compositeFactory -> 
     let variant = variantDescriptor Font.fontVariantFactory
     in Font.addVariant variant innerDescriptor compositeFactory

withStyle : Font.FontStyleDescriptor -> 
            Font.ComposedFontDescriptor sz -> 
            Font.ComposedFontDescriptor sz
withStyle styleDescriptor innerDescriptor = 
  \compositeFactory ->
     let style = styleDescriptor Font.fontStyleFactory
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
