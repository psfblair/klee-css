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
import Css.Internal.Property exposing 
  (spaceQuadrupleValue, spaceListValue, commaListValue)
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
-- also takes none, initial, inherit
-- blur-radius and color are optional
-- More than one shadow can be added; e.g.:
--   text-shadow: 0 0 3px #FF0000, 0 0 5px #0000FF;
textShadow : TextShadowDescriptor a hSz vSz blrSz -> 
             PropertyRuleAppender
textShadow descriptor  =
  let shadowValue = descriptor textShadowFactory |> textShadowValue
  in simpleProperty "text-shadow" shadowValue

textShadows : List (TextShadowDescriptor a hSz vSz blrSz) -> PropertyRuleAppender
textShadows descriptors =
  let applyDescriptor desc = desc textShadowFactory 
      values = List.map applyDescriptor descriptors
      valueFactory = commaListValue textShadowValue
  in simpleProperty "text-shadow" (valueFactory values)
  
aShadow : SizeDescriptor (Size hSz) hSz -> 
          SizeDescriptor (Size vSz) vSz -> 
          CompositeTextShadowDescriptor hSz vSz blrSz
aShadow horizontalDescriptor verticalDescriptor factory =
  let horizontal = horizontalDescriptor sizeFactory
      vertical = verticalDescriptor sizeFactory
  in factory.baseShadow horizontal vertical

shadowBlur : SizeDescriptor (Size blrSz) blrSz ->
             CompositeTextShadowDescriptor hSz vSz blrSz -> 
             CompositeTextShadowDescriptor hSz vSz blrSz
shadowBlur blurDescriptor innerDescriptor factory =
  let radius = blurDescriptor sizeFactory
      innerCompositeShadow = innerDescriptor factory
  in factory.withBlurRadius radius innerCompositeShadow.textShadow

shadowColor : SizeDescriptor (Size blrSz) blrSz ->
              CompositeTextShadowDescriptor hSz vSz blrSz -> 
              CompositeTextShadowDescriptor hSz vSz blrSz
shadowColor blurDescriptor innerDescriptor factory =
  let radius = blurDescriptor sizeFactory
      innerCompositeShadow = innerDescriptor factory
  in factory.withBlurRadius radius innerCompositeShadow.textShadow

  
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

-- TODO: counter, counters: 

-- counter() has two forms: 'counter(name)' or 'counter(name, style)'. 
-- formatted in the indicated list-type style (disc, circle, square, etc.; 
-- 'decimal' by default).  
-- If the name is 'none', 'inherit' or 'initial', the declaration is ignored;
-- we won't type check that.

-- counter :

-- counters() has two forms: 'counters(name, string)' or 'counters(name, string, style)'. 
-- value of all counters with the given name in scope at this pseudo-element, 
-- from outermost to innermost separated by the specified string. The counters 
-- are rendered in the indicated style ('decimal' by default). 
 
-- counters :

-- counter-increment : [<user-ident> <integer>?]+ | none
-- counterIncrement : CounterControlDescriptor -> PropertyRuleAppender

-- counter-reset : [<user-ident> <integer>?]+ | none
-- counterReset : CounterControlDescriptor -> PropertyRuleAppender
