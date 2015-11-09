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

import Css.Internal.Property exposing (toLiteral, literalValue, commaListValue)
import Css.Internal.Stylesheet exposing (simpleProperty, PropertyRuleAppender)
import Css.Internal.Color exposing 
  (ColorDescriptor, colorFactory, colorValue)
import Css.Internal.Size exposing 
  (Size, SizeDescriptor, sizeFactory, sizeValue)

import Css.Internal.Font exposing (..)  

-------------------------------------------------------------------------------
color : ColorDescriptor {} -> PropertyRuleAppender
color colorDescriptor = 
  let colour = colorDescriptor colorFactory
  in simpleProperty "color" (colorValue colour)

-- | An alias for `color`.
fontColor : ColorDescriptor {} -> PropertyRuleAppender
fontColor colorDescriptor = 
  let colour = colorDescriptor colorFactory
  in simpleProperty "color" (colorValue colour)

-------------------------------------------------------------------------------
-- TODO Can take initial, inherit, unset
-- | The `fontFamily` style rule takes two lists of font families: zero or more
-- custom font-families and preferably one or more generic font families.
fontFamily : List String -> List GenericFontFamilyDescriptor -> PropertyRuleAppender
fontFamily customFamilies genericFamilies = 
  let customLiteralValues = 
        customFamilies |> List.map toLiteral |> List.map literalValue
      genericValues = 
        List.map (\descriptor -> descriptor genericFontFamilyFactory) genericFamilies
        |> List.map genericFontFamilyValue
      valueFactory = commaListValue identity
   in simpleProperty "font-family" (valueFactory (customLiteralValues ++ genericValues))

sansSerif : GenericFontFamilyDescriptor 
sansSerif factory = factory.family "sans-serif"


serif : GenericFontFamilyDescriptor 
serif factory = factory.family "serif"


monospace : GenericFontFamilyDescriptor 
monospace factory = factory.family "monospace"


cursive : GenericFontFamilyDescriptor 
cursive factory = factory.family "cursive"


fantasy : GenericFontFamilyDescriptor
fantasy factory = factory.family "fantasy"


-------------------------------------------------------------------------------


-- TODO Test that we can pass size descriptors here too.
fontSize : FontSizeDescriptor -> PropertyRuleAppender
fontSize sizeDescriptor = 
  simpleProperty "font-size" (sizeDescriptor fontSizeFactory |> fontSizeValue) 


xxSmall : FontSizeDescriptor
xxSmall factory = factory.size "xx-small"


xSmall : FontSizeDescriptor 
xSmall factory = factory.size "x-small"


small : FontSizeDescriptor 
small factory = factory.size "small"


medium : FontSizeDescriptor
medium factory = factory.size "medium"


large : FontSizeDescriptor 
large factory = factory.size "large"


xLarge : FontSizeDescriptor 
xLarge factory = factory.size "x-large"


xxLarge : FontSizeDescriptor
xxLarge factory = factory.size "xx-large"


smaller : FontSizeDescriptor 
smaller factory = factory.size "smaller"


larger : FontSizeDescriptor
larger factory = factory.size "larger"


-------------------------------------------------------------------------------


fontStyle : FontStyleDescriptor -> PropertyRuleAppender
fontStyle styleDescriptor = 
  simpleProperty "font-style" (styleDescriptor fontStyleFactory |> fontStyleValue) 


italic : FontStyleDescriptor
italic factory = factory.style "italic"


oblique : FontStyleDescriptor
oblique factory = factory.style "oblique"


-------------------------------------------------------------------------------

-- TODO - Need a second version to take a list of variants
fontVariant : FontVariantDescriptor -> PropertyRuleAppender
fontVariant variantDescriptor = 
  simpleProperty "font-variant" (variantDescriptor fontVariantFactory |> fontVariantValue) 

smallCaps : FontVariantDescriptor
smallCaps factory = factory.variant "small-caps"

-- TODO - There are many more of these now. See https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant
-------------------------------------------------------------------------------

fontWeight : FontWeightDescriptor -> PropertyRuleAppender
fontWeight descriptor = 
  simpleProperty "font-weight" (descriptor fontWeightFactory |> fontWeightValue) 


bold : FontWeightDescriptor
bold factory = factory.weight "bold"


bolder : FontWeightDescriptor 
bolder factory = factory.weight "bolder"


lighter : FontWeightDescriptor
lighter factory = factory.weight "lighter"


weight : Int -> FontWeightDescriptor
weight i factory = factory.weight (toString i)

-------------------------------------------------------------------------------
-- TODO also takes normal, initial, inherit, unset, other
lineHeight : SizeDescriptor (Size c) c -> PropertyRuleAppender
lineHeight descriptor = 
  simpleProperty "line-height" (descriptor sizeFactory |> sizeValue) 

-------------------------------------------------------------------------------

font : FontDescriptor a sz -> PropertyRuleAppender
font fontDescriptor = 
  simpleProperty "font" (fontDescriptor fontFactory |> fontValue)

{- Equivalent to
aFont : SizeDescriptor (Size sz) sz -> 
           List String -> 
           List GenericFontFamily -> 
           FontFactory sz -> 
           ComposedFont sz
-}  
aFont : SizeDescriptor (Size sz) sz -> 
           List String -> 
           List GenericFontFamilyDescriptor -> 
           ComposedFontDescriptor sz
aFont sizeDescriptor customFonts genericFontDescriptors compositeFactory =
  let size = sizeDescriptor sizeFactory
      genericFontFrom familyDescriptor = familyDescriptor genericFontFamilyFactory
      genericFonts = List.map genericFontFrom genericFontDescriptors
  in compositeFactory.leaf size customFonts genericFonts

{- Equivalent to
withLineHeight :  SizeDescriptor (Size sz) sz -> 
                  (FontFactory sz -> ComposedFont sz)
                  FontFactory sz -> 
                  ComposedFont sz
-}
withLineHeight : SizeDescriptor (Size sz) sz -> 
                 ComposedFontDescriptor sz -> 
                 ComposedFontDescriptor sz
withLineHeight lineHeightDescriptor compositeDescriptor compositeFactory =
   let composedFont = compositeDescriptor compositeFactory
       rewrapWithLineHeight fontWithComponents lineHeight = 
         case fontWithComponents.fontComponents of
           -- If withLineHeight is called twice, the later (outer) one wins, which 
           -- means that if this leaf has already been created, so don't touch it.
           WithLineHeight _ _ _ _ as leaf -> fontWithComponents
           BaseComponent size customFonts genericFonts -> 
             let components = 
               WithLineHeight size lineHeight customFonts genericFonts
             in { font = CompositeFont components, fontComponents = components }
           WithWeight weight innerComposedFont -> 
            let components = 
              WithWeight weight (rewrapWithLineHeight innerComposedFont lineHeight)
             in { font = CompositeFont components, fontComponents = components }
           WithVariant variant innerComposedFont ->
             let components =
               WithVariant variant (rewrapWithLineHeight innerComposedFont lineHeight)
             in { font = CompositeFont components, fontComponents = components }
           WithStyle style innerComposedFont -> 
             let components =
               WithStyle style (rewrapWithLineHeight innerComposedFont lineHeight)
             in { font = CompositeFont components, fontComponents = components }
   in rewrapWithLineHeight composedFont (lineHeightDescriptor sizeFactory)

{- Equivalent to 
withWeight : FontWeight -> 
             (FontFactory sz -> ComposedFont sz)
             FontFactory sz -> 
             ComposedFont sz
-}
withWeight : FontWeightDescriptor -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withWeight weightDescriptor innerDescriptor compositeFactory =
   let weight = weightDescriptor fontWeightFactory 
       innerFont = innerDescriptor compositeFactory
   in compositeFactory.composite (WithWeight weight) innerFont
  
{- Equivalent to 
withVariant : FontVariant -> 
              (FontFactory sz -> ComposedFont sz)
              FontFactory sz -> 
              ComposedFont sz
-}
withVariant : FontVariantDescriptor -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withVariant variantDescriptor innerDescriptor compositeFactory =
   let variant = variantDescriptor fontVariantFactory
       innerFont = innerDescriptor compositeFactory
   in compositeFactory.composite (WithVariant variant) innerFont

{- Equivalent to 
withStyle : FontStyle -> 
            (FontFactory sz -> ComposedFont sz)
            FontFactory sz -> 
            ComposedFont sz
-}
withStyle : FontStyleDescriptor -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withStyle styleDescriptor innerDescriptor compositeFactory =
   let style = styleDescriptor fontStyleFactory
       innerFont = innerDescriptor compositeFactory
   in compositeFactory.composite (WithStyle style) innerFont

caption : FontDescriptor {} sz
caption factory = factory.named "caption"

icon : FontDescriptor {} sz
icon factory = factory.named "icon"

menu : FontDescriptor {} sz
menu factory = factory.named "menu"

messageBox : FontDescriptor {} sz
messageBox factory = factory.named "message-box"

smallCaption : FontDescriptor {} sz
smallCaption factory = factory.named "small-caption"

statusBar : FontDescriptor {} sz
statusBar factory = factory.named "status-bar"
