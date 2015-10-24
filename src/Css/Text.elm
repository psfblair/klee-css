module Css.Text
  (
  -- * Letter and word-spacing.

    letterSpacing
  , wordSpacing

  -- * Text-rendering.

  , TextRendering
  , textRendering
  , optimizeSpeed, optimizeLegibility, geometricPrecision

  -- * Text-shadow.

  , textShadow

  -- * Text-indent.

  , TextIndent
  , textIndent
  , eachLine, hanging
  , indent

  -- * Text-direction.

  , TextDirection
  , direction
  , ltr
  , rtl

  -- * Text-align.

  , TextAlign
  , textAlign
  , justify, matchParent, start, end
  , alignSide
  , alignString

  -- * White-space.

  , WhiteSpace
  , whiteSpace
  , pre, nowrap, preWrap, preLine

  -- * Text-decoration.

  , TextDecoration
  , textDecoration
  , textDecorationStyle
  , textDecorationLine
  , textDecorationColor
  , underline, overline, lineThrough, blink

  -- * Text-transform.

  , TextTransform
  , textTransform
  , capitalize, uppercase, lowercase, fullWidth

  -- * Content.

  , Content
  , content
  , contents
  , attrContent
  , stringContent
  , uriContent
  , urlContent
  , openQuote, closeQuote, noOpenQuote, noCloseQuote

  ) where

import Css.Internal.Property
import Css.Internal.Stylesheet

import Css.Background
import Css.Border
import Css.Color
import Css.Common
import Css.Size

-------------------------------------------------------------------------------

letterSpacing : Size a -> PropertyRuleAppender
letterSpacing = key "letter-spacing"

wordSpacing : Size a -> PropertyRuleAppender
wordSpacing = key "word-spacing"

-------------------------------------------------------------------------------

newtype TextRendering = TextRendering Value
  deriving (Val, Auto, Inherit, Other)

optimizeSpeed, optimizeLegibility, geometricPrecision : TextRendering

optimizeSpeed      = TextRendering "optimizeSpeed"
optimizeLegibility = TextRendering "optimizeLegibility"
geometricPrecision = TextRendering "geometricPrecision"

textRendering : TextRendering -> PropertyRuleAppender
textRendering = key "text-rendering"

-------------------------------------------------------------------------------

textShadow : Size a -> Size a -> Size a -> Color -> PropertyRuleAppender
textShadow x y w c = key "text-shadow" (x ! y ! w ! c)

-------------------------------------------------------------------------------

newtype TextIndent = TextIndent Value
  deriving (Val, Inherit, Other)

eachLine, hanging : TextIndent

eachLine = TextIndent "each-line"
hanging  = TextIndent "hanging"

indent : Size a -> TextIndent
indent = TextIndent . value

textIndent : TextIndent -> PropertyRuleAppender
textIndent = key "text-indent"

-------------------------------------------------------------------------------

newtype TextDirection = TextDirection Value
  deriving (Val, Normal, Inherit, Other)

ltr : TextDirection
ltr = TextDirection "ltr"

rtl : TextDirection
rtl = TextDirection "rtl"

direction : TextDirection -> PropertyRuleAppender
direction = key "direction"

-------------------------------------------------------------------------------

newtype TextAlign = TextAlign Value
  deriving (Val, Normal, Inherit, Other, Center)

justify, matchParent, start, end : TextAlign

justify     = TextAlign "justify"
matchParent = TextAlign "match-parent"
start       = TextAlign "start"
end         = TextAlign "end"

alignSide : Side -> TextAlign
alignSide = TextAlign . value

alignString : Char -> TextAlign
alignString = TextAlign . value . Literal . fromString . return

textAlign : TextAlign -> PropertyRuleAppender
textAlign = key "text-align"

-------------------------------------------------------------------------------

newtype WhiteSpace = WhiteSpace Value
  deriving (Val, Normal, Inherit, Other)

whiteSpace : WhiteSpace -> PropertyRuleAppender
whiteSpace = key "white-space"

pre, nowrap, preWrap, preLine : WhiteSpace

pre     = WhiteSpace "pre"
nowrap  = WhiteSpace "nowrap"
preWrap = WhiteSpace "pre-wrap"
preLine = WhiteSpace "pre-line"

-------------------------------------------------------------------------------

newtype TextDecoration = TextDecoration Value
  deriving (Val, None, Inherit, Other)

underline, overline, lineThrough, blink : TextDecoration

underline   = TextDecoration "underline"
overline    = TextDecoration "overline"
lineThrough = TextDecoration "line-through"
blink       = TextDecoration "blink"

textDecorationLine : TextDecoration -> PropertyRuleAppender
textDecorationLine = key "text-decoration-line"

textDecorationColor : Color -> PropertyRuleAppender
textDecorationColor = key "text-decoration-color"

textDecoration : TextDecoration -> PropertyRuleAppender
textDecoration = key "text-decoration"

textDecorationStyle : Stroke -> PropertyRuleAppender
textDecorationStyle = key "text-decoration-style"

-------------------------------------------------------------------------------

newtype TextTransform = TextTransform Value
  deriving (Val, None, Inherit)

capitalize, uppercase, lowercase, fullWidth : TextTransform

capitalize = TextTransform "capitalize"
uppercase  = TextTransform "uppercase"
lowercase  = TextTransform "lowercase"
fullWidth  = TextTransform "full-width"

textTransform : TextTransform -> PropertyRuleAppender
textTransform = key "text-transform"

-------------------------------------------------------------------------------

newtype Content = Content Value
  deriving (Val, None, Normal, Inherit, Initial)

attrContent : Text -> Content
attrContent a = Content ("attr(" <> value a <> ")")

stringContent : Text -> Content
stringContent = Content . value . Literal

uriContent : Text -> Content
uriContent u = Content ("uri(" <> value (Literal u) <> ")")

urlContent : Text -> Content
urlContent u = Content ("url(" <> value (Literal u) <> ")")

openQuote, closeQuote, noOpenQuote, noCloseQuote : Content

openQuote    = Content "open-quote"
closeQuote   = Content "close-quote"
noOpenQuote  = Content "no-open-quote"
noCloseQuote = Content "no-close-quote"

content : Content -> PropertyRuleAppender
content = key "content"

contents : [Content] -> PropertyRuleAppender
contents cs = key "content" (noCommas cs)

-- TODO: counters
