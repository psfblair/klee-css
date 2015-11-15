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

  -- * Content.

  , content, contents
  , attrContent, stringContent, urlContent
  , openQuote, closeQuote, noOpenQuote, noCloseQuote
  
  , counter, styledCounter
  , counters, styledCounters
  , counterId
  , counterIncrement, counterIncrements, withStep
  , counterReset, counterResets, resetTo

  ) where

import Css.Internal.List exposing (ListStyleTypeDescriptor, listStyleTypeFactory)
import Css.Internal.Property exposing 
  (spaceQuadrupleValue, spaceListValue, commaListValue)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)
import Css.Internal.Text exposing (..)

import Css.Internal.Box.Border.Stroke as Stroke
import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides

-------------------------------------------------------------------------------

letterSpacing : Linear.SizeDescriptorWithNormal sz -> PropertyRuleAppender
letterSpacing sizeDescriptor =
  let sizeVal = sizeDescriptor Linear.sizeFactoryWithNormal 
  in simpleProperty "letter-spacing" sizeVal

wordSpacing : Linear.SizeDescriptorWithNormal sz -> PropertyRuleAppender
wordSpacing sizeDescriptor =
  let sizeVal = sizeDescriptor Linear.sizeFactoryWithNormal 
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

-- This is still over-constrained in that each element of the list has to have the
-- same combination of absolute and relative positions.
textShadows : List (TextShadowDescriptor a hSz vSz blrSz) -> PropertyRuleAppender
textShadows descriptors =
  let applyDescriptor desc = desc textShadowFactory 
      values = List.map applyDescriptor descriptors
      valueFactory = commaListValue textShadowValue
  in simpleProperty "text-shadow" (valueFactory values)
  
aShadow : Linear.SizeDescriptor {} hSz -> 
          Linear.SizeDescriptor {} vSz -> 
          CompositeTextShadowDescriptor hSz vSz blrSz
aShadow horizontalDescriptor verticalDescriptor factory =
  factory.baseShadow horizontalDescriptor verticalDescriptor

shadowBlur : Linear.SizeDescriptor {} blrSz ->
             CompositeTextShadowDescriptor hSz vSz blrSz -> 
             CompositeTextShadowDescriptor hSz vSz blrSz
shadowBlur blurDescriptor innerDescriptor factory =
  let innerCompositeShadow = innerDescriptor factory
  in factory.withBlurRadius blurDescriptor innerCompositeShadow.textShadow

shadowColor : Color.ColorDescriptor {} ->
              CompositeTextShadowDescriptor hSz vSz blrSz -> 
              CompositeTextShadowDescriptor hSz vSz blrSz
shadowColor colorDescriptor innerDescriptor factory =
  let innerCompositeShadow = innerDescriptor factory
  in factory.withColor colorDescriptor innerCompositeShadow.textShadow
  
-------------------------------------------------------------------------------

textIndent : TextIndentDescriptor a -> PropertyRuleAppender
textIndent descriptor = 
  let indentValue = descriptor textIndentFactory |> textIndentValue
  in simpleProperty "text-indent" indentValue

eachLine: TextIndentDescriptor a
eachLine factory = factory.indentEachLine

hanging : TextIndentDescriptor a
hanging factory = factory.hangingIndent

indent : Linear.SizeDescriptor {} a -> TextIndentDescriptor a
indent sizeDescriptor factory = factory.textIndent sizeDescriptor

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

justifyAll : TextAlignDescriptor
justifyAll factory = factory.justifyAll

matchParent : TextAlignDescriptor
matchParent factory = factory.matchParent

alignSide : Sides.HorizontalSide -> TextAlignDescriptor
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

textDecorationColor : Color.BasicColorDescriptor -> PropertyRuleAppender
textDecorationColor descriptor = 
  let colorVal = descriptor Color.colorFactory
  in simpleProperty "text-decoration-color" colorVal

textDecorationStyle : Stroke.StrokeDescriptor {} -> PropertyRuleAppender
textDecorationStyle descriptor = 
  let strokeVal = descriptor Stroke.strokeFactory
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
-- where style is a list style (disc, circle, square, etc.; 'decimal' by default).  
-- If the name is 'none', 'inherit' or 'initial', the declaration is ignored;
-- so we won't type check that.
counter : String -> ComposableContentDescriptor
counter name factory = factory.counter name Nothing
    
styledCounter : String -> ListStyleTypeDescriptor -> ComposableContentDescriptor
styledCounter name styleDescriptor factory =
  factory.counter name (Just styleDescriptor)

-- counters() has two forms: 'counters(name, string)' or 'counters(name, string, style)'.
-- where `string` is the string to nest between different levels of nested counters. 
counters : String -> String -> ComposableContentDescriptor
counters name separator factory = factory.counters name separator Nothing
    
styledCounters : String -> 
                 String -> 
                 ListStyleTypeDescriptor -> 
                 ComposableContentDescriptor
styledCounters name separator styleDescriptor factory =
  factory.counters name separator (Just styleDescriptor)

counterId : String -> CounterControlFactory a b -> a
counterId theId factory = factory.id_ theId
  
-- counter-increment : [<user-ident> <integer>?]+ | none | initial | inherit
counterIncrement : CounterIncrementDescriptor -> PropertyRuleAppender
counterIncrement descriptor = 
  let increment = descriptor counterIncrementFactory
  in simpleProperty "counter-increment" increment

counterIncrements : List CounterIncrementDescriptor -> PropertyRuleAppender
counterIncrements descriptors = 
  let applyDescriptor desc = desc counterIncrementFactory
      values = List.map applyDescriptor descriptors 
  in simpleProperty "counter-increment" (spaceListValue identity values)

withStep : String -> Int -> CounterIncrementDescriptor
withStep name step factory = factory.withStep name step
  
-- counter-reset : [<user-ident> <integer>?]+ | none | initial | inherit
counterReset : CounterResetDescriptor -> PropertyRuleAppender
counterReset descriptor = 
  let resetValue = descriptor counterResetFactory
  in simpleProperty "counter-reset" resetValue

counterResets : List CounterResetDescriptor -> PropertyRuleAppender
counterResets descriptors = 
  let applyDescriptor desc = desc counterResetFactory
      values = List.map applyDescriptor descriptors 
  in simpleProperty "counter-reset" (spaceListValue identity values)

resetTo : String -> Int -> CounterResetDescriptor
resetTo name initVal factory = factory.withInitialValue name initVal
