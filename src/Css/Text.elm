module Css.Text
  (
  -- * Letter and word-spacing.

    letterSpacing
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
  , underline, overline, lineThrough, blink
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

import Css.Internal.Property exposing 
  (spaceQuadrupleValue, spaceListValue, commaListValue)
import Css.Internal.Text exposing (..)

import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides
import Css.Internal.List as List
import Css.Internal.Stroke as Stroke
import Css.Internal.Stylesheet as Stylesheet

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

textRendering : TextRenderingDescriptor -> Stylesheet.PropertyRuleAppender
textRendering descriptor =
  let renderValue = descriptor textRenderingFactory |> textRenderingValue
  in Stylesheet.simpleProperty "text-rendering" renderValue

optimizeSpeed : TextRenderingDescriptor
optimizeSpeed = \factory -> factory.speedOptimize

optimizeLegibility : TextRenderingDescriptor
optimizeLegibility = \factory -> factory.legibilityOptimize

geometricPrecision : TextRenderingDescriptor
geometricPrecision = \factory -> factory.preciseGeometry

-------------------------------------------------------------------------------
-- also takes none, initial, inherit
-- blur-radius and color are optional
-- More than one shadow can be added; e.g.:
--   text-shadow: 0 0 3px #FF0000, 0 0 5px #0000FF;
textShadow : TextShadowDescriptor a hSz vSz blrSz -> 
             Stylesheet.PropertyRuleAppender
textShadow descriptor  =
  let shadowValue = descriptor textShadowFactory |> textShadowValue
  in Stylesheet.simpleProperty "text-shadow" shadowValue

-- This is still over-constrained in that each element of the list has to have the
-- same combination of absolute and relative positions.
textShadows : List (TextShadowDescriptor a hSz vSz blrSz) -> 
              Stylesheet.PropertyRuleAppender
textShadows descriptors =
  let applyDescriptor desc = desc textShadowFactory 
      values = List.map applyDescriptor descriptors
      valueFactory = commaListValue textShadowValue
  in Stylesheet.simpleProperty "text-shadow" (valueFactory values)
  
aShadow : Linear.NubSizeDescriptor {} hSz -> 
          Linear.NubSizeDescriptor {} vSz -> 
          CompositeTextShadowDescriptor hSz vSz blrSz
aShadow horizontalDescriptor verticalDescriptor factory =
  factory.baseShadow horizontalDescriptor verticalDescriptor

shadowBlur : Linear.NubSizeDescriptor {} blrSz ->
             CompositeTextShadowDescriptor hSz vSz blrSz -> 
             CompositeTextShadowDescriptor hSz vSz blrSz
shadowBlur blurDescriptor innerDescriptor factory =
  let innerCompositeShadow = innerDescriptor factory
  in factory.withBlurRadius blurDescriptor innerCompositeShadow.textShadow

shadowColor : Color.NubColorDescriptor {} ->
              CompositeTextShadowDescriptor hSz vSz blrSz -> 
              CompositeTextShadowDescriptor hSz vSz blrSz
shadowColor colorDescriptor innerDescriptor factory =
  let innerCompositeShadow = innerDescriptor factory
  in factory.withColor colorDescriptor innerCompositeShadow.textShadow
  
-------------------------------------------------------------------------------

textIndent : TextIndentDescriptor a -> Stylesheet.PropertyRuleAppender
textIndent descriptor = 
  let indentValue = descriptor textIndentFactory |> textIndentValue
  in Stylesheet.simpleProperty "text-indent" indentValue

eachLine: TextIndentDescriptor a
eachLine = \factory -> factory.indentEachLine

hanging : TextIndentDescriptor a
hanging = \factory -> factory.hangingIndent

indent : Linear.NubSizeDescriptor {} a -> TextIndentDescriptor a
indent sizeDescriptor = \factory -> factory.textIndent sizeDescriptor

-------------------------------------------------------------------------------

direction : TextDirectionDescriptor -> Stylesheet.PropertyRuleAppender
direction descriptor = 
  let directionValue = descriptor textDirectionFactory |> textDirectionValue
  in Stylesheet.simpleProperty "direction" directionValue

rtl : TextDirectionDescriptor
rtl = \factory -> factory.rightToLeft

ltr : TextDirectionDescriptor
ltr = \factory -> factory.leftToRight

-------------------------------------------------------------------------------

textAlign : TextAlignDescriptor -> Stylesheet.PropertyRuleAppender
textAlign descriptor  = 
  let alignmentValue = descriptor textAlignFactory |> textAlignValue
  in Stylesheet.simpleProperty "text-align" alignmentValue

start : TextAlignDescriptor
start = \factory -> factory.start

end : TextAlignDescriptor
end = \factory -> factory.end

justify : TextAlignDescriptor
justify = \factory -> factory.justify

justifyAll : TextAlignDescriptor
justifyAll = \factory -> factory.justifyAll

matchParent : TextAlignDescriptor
matchParent = \factory -> factory.matchParent

alignSide : Sides.HorizontalSide -> TextAlignDescriptor
alignSide side = \factory -> factory.alignWithSide side

-------------------------------------------------------------------------------

whiteSpace : WhiteSpaceDescriptor -> Stylesheet.PropertyRuleAppender
whiteSpace descriptor = 
  let whiteSpaceVal = descriptor whiteSpaceFactory |> whiteSpaceValue
  in Stylesheet.simpleProperty "white-space" whiteSpaceVal

nowrap : WhiteSpaceDescriptor
nowrap = \factory -> factory.noWrap

pre : WhiteSpaceDescriptor
pre = \factory -> factory.pre

preWrap : WhiteSpaceDescriptor
preWrap = \factory -> factory.preWrap

preLine : WhiteSpaceDescriptor
preLine = \factory -> factory.preLine

-------------------------------------------------------------------------------

textDecoration : TextDecorationDescriptor -> Stylesheet.PropertyRuleAppender
textDecoration descriptor = 
  let decorationValue = descriptor textDecorationFactory |> textDecorationValue
  in Stylesheet.simpleProperty "text-decoration" decorationValue

textDecorationLine : TextDecorationDescriptor -> Stylesheet.PropertyRuleAppender
textDecorationLine descriptor = 
  let decorationValue = descriptor textDecorationFactory |> textDecorationValue
  in Stylesheet.simpleProperty "text-decoration-line" decorationValue

underline : TextDecorationDescriptor
underline = \factory -> factory.underline

overline : TextDecorationDescriptor
overline = \factory -> factory.overline

lineThrough : TextDecorationDescriptor
lineThrough = \factory -> factory.lineThrough

blink : TextDecorationDescriptor
blink = \factory -> factory.blink

-------------------------------------------------------------------------------

textDecorationColor : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
textDecorationColor descriptor = 
  let colorVal = descriptor Color.colorFactory
  in Stylesheet.simpleProperty "text-decoration-color" colorVal

textDecorationStyle : Stroke.StrokeDescriptor {} -> Stylesheet.PropertyRuleAppender
textDecorationStyle descriptor = 
  let strokeVal = descriptor Stroke.strokeFactory
  in Stylesheet.simpleProperty "text-decoration-style" strokeVal

-------------------------------------------------------------------------------

textTransform : TextTransformDescriptor -> Stylesheet.PropertyRuleAppender
textTransform descriptor = 
  let transformValue = descriptor textTransformFactory |> textTransformValue
  in Stylesheet.simpleProperty "text-transform" transformValue

capitalize : TextTransformDescriptor
capitalize = \factory -> factory.capitalize

uppercase : TextTransformDescriptor
uppercase = \factory -> factory.uppercase

lowercase : TextTransformDescriptor
lowercase = \factory -> factory.lowercase

fullWidth : TextTransformDescriptor
fullWidth = \factory -> factory.fullWidth

-------------------------------------------------------------------------------

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
