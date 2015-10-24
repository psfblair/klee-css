module Css.Font
  (
  -- * Generic font property.
  font

  -- * Color.

  , fontColor
  , color

  -- * Font-family.

  , fontFamily
  , sansSerif
  , serif
  , monospace
  , cursive
  , fantasy

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

  -- * Named fonts.

  , caption, icon, menu, messageBox, smallCaption, statusBar

  -- * Line-height.

  , lineHeight
  ) where

import Css.Internal.Property exposing 
  ( Literal (..)
  , stringKey
  , literalValueFactory, valueValueFactory
  , commaListValueFactory
  )
import Css.Internal.Stylesheet exposing (key, PropertyRuleAppender)
import Css.Internal.Color exposing 
  (ColorDescriptor, colorFactory, colorValueFactory)
import Css.Internal.Size exposing 
  (Size, SizeDescriptor, sizeFactory, sizeValueFactory)

import Css.Internal.Font exposing (..)  

-------------------------------------------------------------------------------

font : FontDescriptor a sz -> PropertyRuleAppender
font fontDescriptor = 
  key (stringKey "font") (fontDescriptor fontFactory) fontValueFactory

{- Equivalent to
baseFont : SizeDescriptor (Size sz) sz -> 
           List String -> 
           List GenericFontFamily -> 
           FontFactory sz -> 
           ComposedFont sz
-}  
baseFont : SizeDescriptor (Size sz) sz -> 
           List String -> 
           List GenericFontFamily -> 
           ComposedFontDescriptor sz
baseFont sizeDescriptor customFonts genericFonts compositeFactory =
  let size = sizeDescriptor sizeFactory
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
withWeight : FontWeight -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withWeight weight descriptor compositeFactory =
   let innerFont = descriptor compositeFactory
   in compositeFactory.composite (WithWeight weight) innerFont
  
{- Equivalent to 
withVariant : FontVariant -> 
              (FontFactory sz -> ComposedFont sz)
              FontFactory sz -> 
              ComposedFont sz
-}
withVariant : FontVariant -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withVariant variant descriptor compositeFactory =
   let innerFont = descriptor compositeFactory
   in compositeFactory.composite (WithVariant variant) innerFont

{- Equivalent to 
withStyle : FontStyle -> 
            (FontFactory sz -> ComposedFont sz)
            FontFactory sz -> 
            ComposedFont sz
-}
withStyle : FontStyle -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withStyle style descriptor compositeFactory =
   let innerFont = descriptor compositeFactory
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

-------------------------------------------------------------------------------
color : ColorDescriptor {} -> PropertyRuleAppender
color colorDescriptor = 
  let colour = colorDescriptor colorFactory
  in key (stringKey "color") colour colorValueFactory

-- | An alias for `color`.
fontColor : ColorDescriptor {} -> PropertyRuleAppender
fontColor colorDescriptor = 
  let colour = colorDescriptor colorFactory
  in key (stringKey "color") colour colorValueFactory

-------------------------------------------------------------------------------
-- | The `fontFamily` style rule takes two lists of font families: zero or more
-- custom font-families and preferably one or more generic font families.
fontFamily : List String -> List GenericFontFamilyDescriptor -> PropertyRuleAppender
fontFamily customFamilies genericFamilies = 
  let customLiteralValues = 
        customFamilies |> List.map Literal |> List.map literalValueFactory.value
      genericValues = 
        List.map (\descriptor -> descriptor genericFontFamilyFactory) genericFamilies
        |> List.map genericFontFamilyValueFactory.value
      valueFactory = commaListValueFactory valueValueFactory
   in key (stringKey "font-family") (customLiteralValues ++ genericValues) valueFactory

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


-- TODO Test that we can pass size descriptors here too.
fontSize : FontSizeDescriptor -> PropertyRuleAppender
fontSize sizeDescriptor = 
  key (stringKey "font-size") (sizeDescriptor fontSizeFactory) fontSizeValueFactory


-------------------------------------------------------------------------------


fontStyle : FontStyleDescriptor -> PropertyRuleAppender
fontStyle styleDescriptor = 
  key (stringKey "font-style") (styleDescriptor fontStyleFactory) fontStyleValueFactory


italic : FontStyleDescriptor
italic factory = factory.style "italic"


oblique : FontStyleDescriptor
oblique factory = factory.style "oblique"


-------------------------------------------------------------------------------


fontVariant : FontVariantDescriptor -> PropertyRuleAppender
fontVariant variantDescriptor = 
  key (stringKey "font-variant") (variantDescriptor fontVariantFactory) fontVariantValueFactory

smallCaps : FontVariantDescriptor
smallCaps factory = factory.variant "small-caps"

-------------------------------------------------------------------------------

fontWeight : FontWeightDescriptor -> PropertyRuleAppender
fontWeight descriptor = 
  key (stringKey "font-weight") (descriptor fontWeightFactory) fontWeightValueFactory


bold : FontWeightDescriptor
bold factory = factory.weight "bold"


bolder : FontWeightDescriptor 
bolder factory = factory.weight "bolder"


lighter : FontWeightDescriptor
lighter factory = factory.weight "lighter"


weight : Int -> FontWeightDescriptor
weight i factory = factory.weight (toString i)

-------------------------------------------------------------------------------

lineHeight : SizeDescriptor (Size c) c -> PropertyRuleAppender
lineHeight descriptor = 
  key (stringKey "line-height") (descriptor sizeFactory) sizeValueFactory
