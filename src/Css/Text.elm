module Css.Text
  (
  -- * Letter and word-spacing.

    letterSpacing
  , wordSpacing

  -- * Text-rendering.

  , textRendering
  , optimizeSpeed, optimizeLegibility, geometricPrecision

  -- * Text-shadow.

  , textShadow

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
  , textDecorationStyle
  , textDecorationLine
  , textDecorationColor
  , underline, overline, lineThrough, blink

  -- * Text-transform.

  , textTransform
  , capitalize, uppercase, lowercase, fullWidth

  -- * Content.

  , content
  , contents
  , attrContent
  , stringContent
  , uriContent
  , urlContent
  , openQuote, closeQuote, noOpenQuote, noCloseQuote

  ) where

import Css.Internal.Border exposing (StrokeDescriptor, strokeFactory, strokeValue)
import Css.Internal.Color exposing (ColorDescriptor, colorFactory, colorValue)
import Css.Internal.Size exposing (Size, SizeDescriptor, sizeFactory, sizeValue)
import Css.Internal.Property exposing (spaceQuadrupleValue, spaceListValue)
import Css.Internal.Position exposing (HorizontalSide)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)
import Css.Internal.Text exposing (..)

-------------------------------------------------------------------------------

letterSpacing : SizeDescriptor (Size a) a -> PropertyRuleAppender
letterSpacing sizeDescriptor =
  let sizeVal = sizeDescriptor sizeFactory |> sizeValue
  in simpleProperty "letter-spacing" sizeVal

wordSpacing : SizeDescriptor (Size a) a -> PropertyRuleAppender
wordSpacing sizeDescriptor =
  let sizeVal = sizeDescriptor sizeFactory |> sizeValue
  in simpleProperty "word-spacing" sizeVal

-------------------------------------------------------------------------------

textRendering : TextRenderingDescriptor -> PropertyRuleAppender
textRendering descriptor =
  let renderValue = descriptor textRenderingFactory |> textRenderingValue
  in simpleProperty "text-rendering" renderValue

optimizeSpeed : TextRenderingDescriptor
optimizeSpeed factory = factory.speedOptimize

optimizeLegibility : TextRenderingDescriptor
optimizeLegibility factory = factory.legibilityOptimize

geometricPrecision : TextRenderingDescriptor
geometricPrecision factory = factory.preciseGeometry

-------------------------------------------------------------------------------
-- TODO: also takes none, initial, inherit
-- TODO: blur-radius and color are optional
-- TODO: More than one shadow can be added; e.g.:
--   text-shadow: 0 0 3px #FF0000, 0 0 5px #0000FF;
textShadow : SizeDescriptor (Size a) a -> 
             SizeDescriptor (Size a) a -> 
             SizeDescriptor (Size a) a -> 
             ColorDescriptor {} -> 
             PropertyRuleAppender
textShadow horizontalDescriptor verticalDescriptor blurDescriptor colorDescriptor = 
  let horizontal = horizontalDescriptor sizeFactory
      vertical = verticalDescriptor sizeFactory
      blurRadius = blurDescriptor sizeFactory
      color = colorDescriptor colorFactory
      values = (horizontal,vertical,blurRadius,color)
      valueFactory = spaceQuadrupleValue sizeValue sizeValue sizeValue colorValue
  in simpleProperty "text-shadow" (valueFactory values)

-------------------------------------------------------------------------------

textIndent : TextIndentDescriptor a -> PropertyRuleAppender
textIndent descriptor = 
  let indentValue = descriptor textIndentFactory |> textIndentValue
  in simpleProperty "text-indent" indentValue

eachLine: TextIndentDescriptor a
eachLine factory = factory.indentEachLine

hanging : TextIndentDescriptor a
hanging factory = factory.hangingIndent

indent : SizeDescriptor (Size a) a -> TextIndentDescriptor a
indent sizeDescriptor factory =
  let size = sizeDescriptor sizeFactory
  in factory.textIndent size

-------------------------------------------------------------------------------

direction : TextDirectionDescriptor -> PropertyRuleAppender
direction descriptor = 
  let directionValue = descriptor textDirectionFactory |> textDirectionValue
  in simpleProperty "direction" directionValue

rtl : TextDirectionDescriptor
rtl factory = factory.rightToLeft

ltr : TextDirectionDescriptor
ltr factory = factory.leftToRight

-------------------------------------------------------------------------------

textAlign : TextAlignDescriptor -> PropertyRuleAppender
textAlign descriptor  = 
  let alignmentValue = descriptor textAlignFactory |> textAlignValue
  in simpleProperty "text-align" alignmentValue

start : TextAlignDescriptor
start factory = factory.start

end : TextAlignDescriptor
end factory = factory.end

justify : TextAlignDescriptor
justify factory = factory.justify

matchParent : TextAlignDescriptor
matchParent factory = factory.matchParent

alignSide : HorizontalSide -> TextAlignDescriptor
alignSide side factory = factory.alignWithSide side

-------------------------------------------------------------------------------

whiteSpace : WhiteSpaceDescriptor -> PropertyRuleAppender
whiteSpace descriptor = 
  let whiteSpaceVal = descriptor whiteSpaceFactory |> whiteSpaceValue
  in simpleProperty "white-space" whiteSpaceVal

nowrap : WhiteSpaceDescriptor
nowrap factory = factory.noWrap

pre : WhiteSpaceDescriptor
pre factory = factory.pre

preWrap : WhiteSpaceDescriptor
preWrap factory = factory.preWrap

preLine : WhiteSpaceDescriptor
preLine factory = factory.preLine

-------------------------------------------------------------------------------

textDecoration : TextDecorationDescriptor -> PropertyRuleAppender
textDecoration descriptor = 
  let decorationValue = descriptor textDecorationFactory |> textDecorationValue
  in simpleProperty "text-decoration" decorationValue

textDecorationLine : TextDecorationDescriptor -> PropertyRuleAppender
textDecorationLine descriptor = 
  let decorationValue = descriptor textDecorationFactory |> textDecorationValue
  in simpleProperty "text-decoration-line" decorationValue

underline : TextDecorationDescriptor
underline factory = factory.underline

overline : TextDecorationDescriptor
overline factory = factory.overline

lineThrough : TextDecorationDescriptor
lineThrough factory = factory.lineThrough

blink : TextDecorationDescriptor
blink factory = factory.blink

-------------------------------------------------------------------------------

textDecorationColor : ColorDescriptor {} -> PropertyRuleAppender
textDecorationColor descriptor = 
  let colorVal = descriptor colorFactory |> colorValue
  in simpleProperty "text-decoration-color" colorVal

textDecorationStyle : StrokeDescriptor -> PropertyRuleAppender
textDecorationStyle descriptor = 
  let strokeVal = descriptor strokeFactory |> strokeValue
  in simpleProperty "text-decoration-style" strokeVal

-------------------------------------------------------------------------------

textTransform : TextTransformDescriptor -> PropertyRuleAppender
textTransform descriptor = 
  let transformValue = descriptor textTransformFactory |> textTransformValue
  in simpleProperty "text-transform" transformValue

capitalize : TextTransformDescriptor
capitalize factory = factory.capitalize

uppercase : TextTransformDescriptor
uppercase factory = factory.uppercase

lowercase : TextTransformDescriptor
lowercase factory = factory.lowercase

fullWidth : TextTransformDescriptor
fullWidth factory = factory.fullWidth

-------------------------------------------------------------------------------
-- TODO - This property can only be used with pseudo-elements :before and :after
-- `content` can take
-- normal|none|initial|inherit
-- Or a list of the following:
-- counter|attr|string|open-quote|close-quote|no-open-quote|no-close-quote|url;

content : ContentDescriptor a -> PropertyRuleAppender
content descriptor = 
  let contentVal = descriptor contentFactory |> contentValue
  in simpleProperty "content" contentVal
  
contents : List ComposableContentDescriptor -> PropertyRuleAppender  
contents descriptors = 
  let toContent aDescriptor = aDescriptor contentFactory
      contentValues = List.map toContent descriptors 
  in simpleProperty "content" (spaceListValue contentValue contentValues)

attrContent : String -> ComposableContentDescriptor
attrContent attrName factory = factory.attributeContent attrName

stringContent : String -> ComposableContentDescriptor
stringContent str factory = factory.stringContent str

uriContent : String -> ComposableContentDescriptor
uriContent uri factory = factory.uriContent uri

urlContent : String -> ComposableContentDescriptor
urlContent url factory = factory.urlContent url

openQuote : ComposableContentDescriptor
openQuote factory = factory.openQuote

closeQuote : ComposableContentDescriptor
closeQuote factory = factory.closeQuote

noOpenQuote : ComposableContentDescriptor
noOpenQuote factory = factory.noOpenQuote

noCloseQuote : ComposableContentDescriptor
noCloseQuote factory = factory.noCloseQuote

-- TODO: counter, counters
