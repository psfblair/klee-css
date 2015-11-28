module Css.Typography
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
  
  -- * Letter and word-spacing.

  , letterSpacing
  , wordSpacing

  -- * Text-rendering.

  , textRendering
  , optimizeSpeed, optimizeLegibility, geometricPrecision

  -- * Text-shadow.

  , textShadow, textShadows
  , aShadow, shadowBlur, shadowColor

  -- * Text-indent.

  , textIndent
  , eachLine, hanging
  , indent

  -- * Text-direction.

  , direction
  , ltr
  , rtl

  -- * Text-align.

  , textAlign
  , justify, matchParent, start, end
  , alignSide

  -- * White-space.

  , whiteSpace
  , pre, nowrap, preWrap, preLine

  -- * Text-decoration.

  , textDecoration
  , textDecorationLine
  , underline, overline, lineThrough
  , textDecorationColor
  , textDecorationStyle

  -- * Text-transform.

  , textTransform
  , capitalize, uppercase, lowercase, fullWidth

  -- * List styles.
    
  , listStyleType
  , listStylePosition
  , inside, outside

  , listStyleImage
  , imageUrl

  , listStyle
  , withListType, withListPosition, withListImage
  
  ) where

import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides
import Css.Internal.List as List
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke
import Css.Internal.Stylesheet as Stylesheet
import Css.Internal.Typography.Font as Font
import Css.Internal.Typography.Font.Family as Family
import Css.Internal.Typography.Font.Size as FontSize
import Css.Internal.Typography.Font.Style as FontStyle
import Css.Internal.Typography.Font.Variant as Variant
import Css.Internal.Typography.Font.Weight as Weight
import Css.Internal.Typography.Text.Align as Align
import Css.Internal.Typography.Text.Decoration as Decoration
import Css.Internal.Typography.Text.Direction as Direction
import Css.Internal.Typography.Text.Indent as Indent
import Css.Internal.Typography.Text.Rendering as Rendering
import Css.Internal.Typography.Text.Shadow as Shadow
import Css.Internal.Typography.Text.Transform as Transform
import Css.Internal.Typography.Text.Whitespace as Whitespace

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

font : Font.FontDescriptor a -> Stylesheet.PropertyRuleAppender
font fontDescriptor = 
  let value = fontDescriptor Font.fontFactory |> Font.fontValue
  in Stylesheet.simpleProperty "font" value

aFont : Linear.NubSizeDescriptor {} szFont -> 
        List String -> 
        List Family.GenericFontFamilyDescriptor -> 
        Font.ComposedFontDescriptor
aFont sizeDescriptor customFonts genericFontDescriptors =
  \compositeFactory -> 
    let sizeVal = sizeDescriptor Linear.nubSizeFactory
        genericFontFrom descriptor = descriptor Family.genericFontFamilyFactory
        genericFonts = List.map genericFontFrom genericFontDescriptors
    in compositeFactory.leaf sizeVal customFonts genericFonts

withLineHeight : Linear.NubSizeDescriptor {} szLinHeight -> 
                 Font.ComposedFontDescriptor -> 
                 Font.ComposedFontDescriptor
withLineHeight lineHeightDescriptor innerDescriptor =
  \compositeFactory -> 
    let lineHeightVal = lineHeightDescriptor Linear.nubSizeFactory
    in  Font.addLineHeight lineHeightVal innerDescriptor compositeFactory

withWeight : Weight.NubFontWeightDescriptor {} -> 
             Font.ComposedFontDescriptor -> 
             Font.ComposedFontDescriptor
withWeight weightDescriptor innerDescriptor = 
  \compositeFactory ->
     let weight = weightDescriptor Weight.nubFontWeightFactory 
     in Font.addWeight weight innerDescriptor compositeFactory
  
withVariant : Variant.NubFontVariantDescriptor {} -> 
              Font.ComposedFontDescriptor -> 
              Font.ComposedFontDescriptor
withVariant variantDescriptor innerDescriptor = 
  \compositeFactory -> 
     let variant = variantDescriptor Variant.nubFontVariantFactory
     in Font.addVariant variant innerDescriptor compositeFactory

withStyle : FontStyle.NubFontStyleDescriptor {} -> 
            Font.ComposedFontDescriptor -> 
            Font.ComposedFontDescriptor
withStyle styleDescriptor innerDescriptor = 
  \compositeFactory ->
     let style = styleDescriptor FontStyle.nubFontStyleFactory
     in Font.addStyle style innerDescriptor compositeFactory

caption : Font.FontDescriptor {}
caption = \factory -> factory.named "caption"

icon : Font.FontDescriptor {}
icon = \factory -> factory.named "icon"

menu : Font.FontDescriptor {}
menu = \factory -> factory.named "menu"

messageBox : Font.FontDescriptor {}
messageBox = \factory -> factory.named "message-box"

smallCaption : Font.FontDescriptor {}
smallCaption = \factory -> factory.named "small-caption"

statusBar : Font.FontDescriptor {}
statusBar = \factory -> factory.named "status-bar"


-------------------------------------------------------------------------------

letterSpacing : Linear.SizeDescriptorWithNormal sz -> 
                Stylesheet.PropertyRuleAppender
letterSpacing sizeDescriptor =
  let sizeVal = sizeDescriptor Linear.sizeFactoryWithNormal 
  in Stylesheet.simpleProperty "letter-spacing" sizeVal

wordSpacing : Linear.SizeDescriptorWithNormal sz -> 
              Stylesheet.PropertyRuleAppender
wordSpacing sizeDescriptor =
  let sizeVal = sizeDescriptor Linear.sizeFactoryWithNormal 
  in Stylesheet.simpleProperty "word-spacing" sizeVal

-------------------------------------------------------------------------------

textRendering : Rendering.TextRenderingDescriptor -> Stylesheet.PropertyRuleAppender
textRendering descriptor =
  let renderValue = descriptor Rendering.textRenderingFactory
  in Stylesheet.simpleProperty "text-rendering" renderValue

optimizeSpeed : Rendering.TextRenderingDescriptor
optimizeSpeed = \factory -> factory.speedOptimize

optimizeLegibility : Rendering.TextRenderingDescriptor
optimizeLegibility = \factory -> factory.legibilityOptimize

geometricPrecision : Rendering.TextRenderingDescriptor
geometricPrecision = \factory -> factory.preciseGeometry

-------------------------------------------------------------------------------
-- also takes none, initial, inherit
-- blur-radius and color are optional
-- More than one shadow can be added; e.g.:
--   text-shadow: 0 0 3px #FF0000, 0 0 5px #0000FF;
textShadow : Shadow.TextShadowDescriptor a hSz vSz blrSz -> 
             Stylesheet.PropertyRuleAppender
textShadow descriptor  =
  let shadowValue = descriptor Shadow.textShadowFactory |> Shadow.textShadowValue
  in Stylesheet.simpleProperty "text-shadow" shadowValue

-- This is still over-constrained in that each element of the list has to have the
-- same combination of absolute and relative positions.
textShadows : List (Shadow.TextShadowDescriptor a hSz vSz blrSz) -> 
              Stylesheet.PropertyRuleAppender
textShadows descriptors =
  let applyDescriptor desc = desc Shadow.textShadowFactory 
      values = List.map applyDescriptor descriptors
      valueFactory = Property.commaListValue Shadow.textShadowValue
  in Stylesheet.simpleProperty "text-shadow" (valueFactory values)
  
aShadow : Linear.NubSizeDescriptor {} hSz -> 
          Linear.NubSizeDescriptor {} vSz -> 
          Shadow.CompositeTextShadowDescriptor hSz vSz blrSz
aShadow horizontalDescriptor verticalDescriptor factory =
  factory.baseShadow horizontalDescriptor verticalDescriptor

shadowBlur : Linear.NubSizeDescriptor {} blrSz ->
             Shadow.CompositeTextShadowDescriptor hSz vSz blrSz -> 
             Shadow.CompositeTextShadowDescriptor hSz vSz blrSz
shadowBlur blurDescriptor innerDescriptor factory =
  let innerCompositeShadow = innerDescriptor factory
  in factory.withBlurRadius blurDescriptor innerCompositeShadow.textShadow

shadowColor : Color.NubColorDescriptor {} ->
              Shadow.CompositeTextShadowDescriptor hSz vSz blrSz -> 
              Shadow.CompositeTextShadowDescriptor hSz vSz blrSz
shadowColor colorDescriptor innerDescriptor factory =
  let innerCompositeShadow = innerDescriptor factory
  in factory.withColor colorDescriptor innerCompositeShadow.textShadow
  
-------------------------------------------------------------------------------

textIndent : Indent.TextIndentDescriptor a -> Stylesheet.PropertyRuleAppender
textIndent descriptor = 
  let indentValue = descriptor Indent.textIndentFactory
  in Stylesheet.simpleProperty "text-indent" indentValue

eachLine: Indent.TextIndentDescriptor a
eachLine = \factory -> factory.indentEachLine

hanging : Indent.TextIndentDescriptor a
hanging = \factory -> factory.hangingIndent

indent : Linear.NubSizeDescriptor {} a -> Indent.TextIndentDescriptor a
indent sizeDescriptor = \factory -> factory.textIndent sizeDescriptor

-------------------------------------------------------------------------------

direction : Direction.TextDirectionDescriptor -> Stylesheet.PropertyRuleAppender
direction descriptor = 
  let directionValue = descriptor Direction.textDirectionFactory
  in Stylesheet.simpleProperty "direction" directionValue

rtl : Direction.TextDirectionDescriptor
rtl = \factory -> factory.rightToLeft

ltr : Direction.TextDirectionDescriptor
ltr = \factory -> factory.leftToRight

-------------------------------------------------------------------------------

textAlign : Align.TextAlignDescriptor -> Stylesheet.PropertyRuleAppender
textAlign descriptor  = 
  let alignmentValue = descriptor Align.textAlignFactory
  in Stylesheet.simpleProperty "text-align" alignmentValue

start : Align.TextAlignDescriptor
start = \factory -> factory.start

end : Align.TextAlignDescriptor
end = \factory -> factory.end

justify : Align.TextAlignDescriptor
justify = \factory -> factory.justify

justifyAll : Align.TextAlignDescriptor
justifyAll = \factory -> factory.justifyAll

matchParent : Align.TextAlignDescriptor
matchParent = \factory -> factory.matchParent

alignSide : Sides.HorizontalSide -> Align.TextAlignDescriptor
alignSide side = \factory -> factory.alignWithSide side

-------------------------------------------------------------------------------

whiteSpace : Whitespace.WhiteSpaceDescriptor -> Stylesheet.PropertyRuleAppender
whiteSpace descriptor = 
  let whiteSpaceVal = descriptor Whitespace.whiteSpaceFactory
  in Stylesheet.simpleProperty "white-space" whiteSpaceVal

nowrap : Whitespace.WhiteSpaceDescriptor
nowrap = \factory -> factory.noWrap

pre : Whitespace.WhiteSpaceDescriptor
pre = \factory -> factory.pre

preWrap : Whitespace.WhiteSpaceDescriptor
preWrap = \factory -> factory.preWrap

preLine : Whitespace.WhiteSpaceDescriptor
preLine = \factory -> factory.preLine

-------------------------------------------------------------------------------
-- TODO There are more complex values for decoration, e.g., 
-- text-decoration: underline red; 
-- text-decoration: underline wavy red;
textDecoration : Decoration.TextDecorationDescriptor -> 
                 Stylesheet.PropertyRuleAppender
textDecoration descriptor = 
  let decorationValue = descriptor Decoration.textDecorationFactory
  in Stylesheet.simpleProperty "text-decoration" decorationValue

-- TODO There are more complex values for text-decoration-line, e.g., 
-- text-decoration: underline overline; 
-- text-decoration: overline underline line-through;
textDecorationLine : Decoration.TextDecorationDescriptor -> 
                     Stylesheet.PropertyRuleAppender
textDecorationLine descriptor = 
  let decorationValue = descriptor Decoration.textDecorationFactory
  in Stylesheet.simpleProperty "text-decoration-line" decorationValue

underline : Decoration.TextDecorationDescriptor
underline = \factory -> factory.underline

overline : Decoration.TextDecorationDescriptor
overline = \factory -> factory.overline

lineThrough : Decoration.TextDecorationDescriptor
lineThrough = \factory -> factory.lineThrough

-------------------------------------------------------------------------------

textDecorationColor : Color.ColorDescriptor -> Stylesheet.PropertyRuleAppender
textDecorationColor descriptor = 
  let colorVal = descriptor Color.colorFactory
  in Stylesheet.simpleProperty "text-decoration-color" colorVal

textDecorationStyle : Stroke.StrokeDescriptor {} -> 
                      Stylesheet.PropertyRuleAppender
textDecorationStyle descriptor = 
  let strokeVal = descriptor Stroke.strokeFactory
  in Stylesheet.simpleProperty "text-decoration-style" strokeVal

-------------------------------------------------------------------------------

textTransform : Transform.TextTransformDescriptor -> 
                Stylesheet.PropertyRuleAppender
textTransform descriptor = 
  let transformValue = descriptor Transform.textTransformFactory
  in Stylesheet.simpleProperty "text-transform" transformValue

capitalize : Transform.TextTransformDescriptor
capitalize = \factory -> factory.capitalize

uppercase : Transform.TextTransformDescriptor
uppercase = \factory -> factory.uppercase

lowercase : Transform.TextTransformDescriptor
lowercase = \factory -> factory.lowercase

fullWidth : Transform.TextTransformDescriptor
fullWidth = \factory -> factory.fullWidth

-------------------------------------------------------------------------------

-- TODO need textOverflow, textJustify   

-------------------------------------------------------------------------------

-- TODO list-style-type can take a string argument

listStyleType : List.ListStyleTypeDescriptor -> Stylesheet.PropertyRuleAppender
listStyleType descriptor = 
  let styleValue = descriptor List.listStyleTypeFactory
  in Stylesheet.simpleProperty "list-style-type" styleValue

-------------------------------------------------------------------------------

listStylePosition : List.ListStylePositionDescriptor -> 
                    Stylesheet.PropertyRuleAppender
listStylePosition descriptor = 
  let positionValue = descriptor List.listStylePositionFactory
  in Stylesheet.simpleProperty "list-style-position" positionValue

inside : List.ListStylePositionDescriptor
inside = \factory -> factory.inside

outside : List.ListStylePositionDescriptor
outside = \factory -> factory.outside

-------------------------------------------------------------------------------

listStyleImage : List.ListStyleImageDescriptor -> Stylesheet.PropertyRuleAppender
listStyleImage descriptor = 
  let imageValue = descriptor List.listStyleImageFactory
  in Stylesheet.simpleProperty "list-style-image" imageValue

imageUrl : String -> List.ListStyleImageDescriptor
imageUrl urlString = \factory -> factory.url urlString


-------------------------------------------------------------------------------
-- list-style-type list-style-position list-style-image . All Three are optional.
-- Also takes initial and inherit
listStyle : List.ListStyleDescriptor a -> Stylesheet.PropertyRuleAppender
listStyle descriptor = 
  let styleRecord = descriptor List.initialListStyleFactory 
      styleValue = List.listStyleValue styleRecord.listStyle
  in Stylesheet.simpleProperty "list-style" styleValue

withListType : List.ListStyleTypeDescriptor -> 
               List.ComposedListStyleDescriptor a
withListType typeDescriptor inner =
  let styleType = typeDescriptor List.listStyleTypeFactory
      innerComponents = inner.styleComponents
      newComponents = List.WithStyleType styleType innerComponents
  in List.adjoinListStyle newComponents
  
withListPosition : List.ListStylePositionDescriptor -> 
                   List.ComposedListStyleDescriptor a
withListPosition positionDescriptor inner =
  let stylePos = positionDescriptor List.listStylePositionFactory
      innerComponents = inner.styleComponents
      newComponents = List.WithStylePosition stylePos innerComponents
  in List.adjoinListStyle newComponents
  
withListImage : List.ListStyleImageDescriptor -> 
                List.ComposedListStyleDescriptor a
withListImage imageDescriptor inner =
  let imageType = imageDescriptor List.listStyleImageFactory
      innerComponents = inner.styleComponents
      newComponents = List.WithStyleImage imageType innerComponents
  in List.adjoinListStyle newComponents
