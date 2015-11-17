module Css.Pseudo
  (
  -- 6.6.1.1 link pseudo-classes
  
  link, visited  
       
  -- 6.6.1.2 user action pseudo-classes
  
  , hover, active, focus
  
  -- 6.6.2 target pseudo-class
  
  , target
  
  -- 6.6.3 language pseudo-class
  
  , lang          
  
  -- 6.6.4 UI element states pseudo-classes
  
  , enabled, disabled, checked, indeterminate 
  
  -- 6.6.5 Structural pseudo-classes
  
  , root, nthChild, nthLastChild, nthOfType, nthLastOfType 
  , firstChild, lastChild, firstOfType, lastOfType
  , onlyChild, onlyOfType    
  , empty, notPs         
  
  -- More recent pseudo-classes
  
  , default_, valid, invalid       
  , inRange, outOfRange    
  , required, optional      
  , readOnly, readWrite     
  , dir, fullscreen  
  
  -- Pseudo-elements
  
  , firstLine, firstLetter
  , after, before     
  , selection, backdrop   
  
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

import Css.Internal.Content as Content
import Css.Internal.List as List
import Css.Internal.Property as Property
import Css.Internal.Selector exposing (filterFromString)
import Css.Internal.SelectorCombinators exposing (func)
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

-- Pseudo-classes 
-- See http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#pseudo-classes

-- 6.6.1 Dynamic pseudo-classes
-- 6.6.1.1 link pseudo-classes
link            = ":link"           |> filterFromString -- CSS level 1
visited         = ":visited"        |> filterFromString -- CSS level 1

-- 6.6.1.2 user action pseudo-classes
hover           = ":hover"          |> filterFromString -- CSS level 2
active          = ":active"         |> filterFromString -- CSS level 1
focus           = ":focus"          |> filterFromString -- CSS level 2

-- 6.6.2 target pseudo-class
target          = ":target"         |> filterFromString -- CSS level 3

-- 6.6.3 language pseudo-class
lang          n = func "lang"             [n]           -- CSS level 2

-- 6.6.4 UI element states pseudo-classes
enabled         = ":enabled"        |> filterFromString -- CSS level 3
disabled        = ":disabled"       |> filterFromString -- CSS level 3
checked         = ":checked"        |> filterFromString -- CSS level 3
indeterminate   = ":indeterminate"  |> filterFromString -- CSS level 3

-- 6.6.5 Structural pseudo-classes
root            = ":root"           |> filterFromString -- CSS level 3
nthChild      n = func "nth-child"        [n]           -- CSS level 3
nthLastChild  n = func "nth-last-child"   [n]           -- CSS level 3
nthOfType     n = func "nth-of-type"      [n]           -- CSS level 3
nthLastOfType n = func "nth-last-of-type" [n]           -- CSS level 3
firstChild      = ":first-child"    |> filterFromString -- CSS level 2
lastChild       = ":last-child"     |> filterFromString -- CSS level 3
firstOfType     = ":first-of-type"  |> filterFromString -- CSS level 3
lastOfType      = ":last-of-type"   |> filterFromString -- CSS level 3
onlyChild       = ":only-child"     |> filterFromString -- CSS level 3
onlyOfType      = ":only-of-type"   |> filterFromString -- CSS level 3
empty           = ":empty"          |> filterFromString -- CSS level 3
-- TODO not(x) takes a simple selector (excluding the negation pseudo-class itself)
notPs         n = func "nth-last-of-type" [n]           -- CSS level 3         

-- For the following, see https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-classes
default_        = ":default"        |> filterFromString -- CSS basic UI Module level 3
valid           = ":valid"          |> filterFromString -- CSS basic UI Module level 3
invalid         = ":invalid"        |> filterFromString -- CSS basic UI Module level 3
inRange         = ":in-range"       |> filterFromString -- CSS basic UI Module level 3
outOfRange      = ":out-of-range"   |> filterFromString -- CSS basic UI Module level 3
required        = ":required"       |> filterFromString -- CSS basic UI Module level 3
optional        = ":optional"       |> filterFromString -- CSS basic UI Module level 3
readOnly        = ":read-only"      |> filterFromString -- CSS basic UI Module level 3
readWrite       = ":read-write"     |> filterFromString -- CSS basic UI Module level 3

dir           n = func ":dir"             [n]           -- HTML5
fullscreen      = ":fullscreen"     |> filterFromString -- Fullscreen API
-------------------------------------------------------------------------------

-- Pseudo-elements
-- See http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#pseudo-elements

-- TODO Only one pseudo-element may appear per selector, and if present it must 
-- appear after the sequence of simple selectors that represents the subjects of 
-- the selector.

-- TODO? ::first-line and ::first-letter can only have an effect when attached to 
-- a block-like container such as a block box, inline-block, table-caption, or 
-- table-cell.
firstLine       = "::first-line"    |> filterFromString -- CSS level 1 (with one colon)
firstLetter     = "::first-letter"  |> filterFromString -- CSS level 1 (with one colon)

after           = "::after"         |> filterFromString -- CSS level 2 (with one colon)
before          = "::before"        |> filterFromString -- CSS level 2 (with one colon)

selection       = "::selection"     |> filterFromString -- CSS Pseudo-Elements Level 4 (working draft)
backdrop        = "::backdrop"      |> filterFromString -- Fullscreen API

-------------------------------------------------------------------------------

-- TODO - This property can only be used with pseudo-elements :before and :after
-- `content` can take
-- normal|none|initial|inherit
-- Or a list of the following:
-- counter|attr|string|open-quote|close-quote|no-open-quote|no-close-quote|url;

content : Content.ContentDescriptor a -> Stylesheet.PropertyRuleAppender
content descriptor = 
  let contentVal = descriptor Content.contentFactory |> Content.contentValue
  in Stylesheet.simpleProperty "content" contentVal
  
contents : List Content.ComposableContentDescriptor -> 
           Stylesheet.PropertyRuleAppender  
contents descriptors = 
  let toContent aDescriptor = aDescriptor Content.contentFactory
      contentValues = List.map toContent descriptors 
      combinedValues = Property.spaceListValue Content.contentValue contentValues
  in Stylesheet.simpleProperty "content" combinedValues

attrContent : String -> Content.ComposableContentDescriptor
attrContent attrName = \factory -> factory.attributeContent attrName

stringContent : String -> Content.ComposableContentDescriptor
stringContent str = \factory -> factory.stringContent str

urlContent : String -> Content.ComposableContentDescriptor
urlContent url = \factory -> factory.urlContent url

openQuote : Content.ComposableContentDescriptor
openQuote = \factory -> factory.openQuote

closeQuote : Content.ComposableContentDescriptor
closeQuote = \factory -> factory.closeQuote

noOpenQuote : Content.ComposableContentDescriptor
noOpenQuote = \factory -> factory.noOpenQuote

noCloseQuote : Content.ComposableContentDescriptor
noCloseQuote = \factory -> factory.noCloseQuote

-------------------------------------------------------------------------------

-- counter() has two forms: 'counter(name)' or 'counter(name, style)'. 
-- where style is a list style (disc, circle, square, etc.; 'decimal' by default).  
-- If the name is 'none', 'inherit' or 'initial', the declaration is ignored;
-- so we won't type check that.
counter : String -> Content.ComposableContentDescriptor
counter name = \factory -> factory.counter name Nothing
    
styledCounter : String -> 
                List.ListStyleTypeDescriptor -> 
                Content.ComposableContentDescriptor
styledCounter name styleDescriptor =
  \factory -> factory.counter name (Just styleDescriptor)

-- counters() has two forms: 'counters(name, string)' or 'counters(name, string, style)'.
-- where `string` is the string to nest between different levels of nested counters. 
counters : String -> String -> Content.ComposableContentDescriptor
counters name separator = \factory -> factory.counters name separator Nothing
    
styledCounters : String -> 
                 String -> 
                 List.ListStyleTypeDescriptor -> 
                 Content.ComposableContentDescriptor
styledCounters name separator styleDescriptor factory =
  factory.counters name separator (Just styleDescriptor)

counterId : String -> Content.CounterControlFactory a b -> a
counterId theId = \factory -> factory.id_ theId
  
-- counter-increment : [<user-ident> <integer>?]+ | none | initial | inherit
counterIncrement : Content.CounterIncrementDescriptor -> 
                   Stylesheet.PropertyRuleAppender
counterIncrement descriptor = 
  let increment = descriptor Content.counterIncrementFactory
  in Stylesheet.simpleProperty "counter-increment" increment

counterIncrements : List Content.CounterIncrementDescriptor -> 
                    Stylesheet.PropertyRuleAppender
counterIncrements descriptors = 
  let applyDescriptor desc = desc Content.counterIncrementFactory
      values = List.map applyDescriptor descriptors 
      combinedValues = Property.spaceListValue identity values
  in Stylesheet.simpleProperty "counter-increment" combinedValues

withStep : String -> Int -> Content.CounterIncrementDescriptor
withStep name step = \factory -> factory.withStep name step
  
-- counter-reset : [<user-ident> <integer>?]+ | none | initial | inherit
counterReset : Content.CounterResetDescriptor -> 
               Stylesheet.PropertyRuleAppender
counterReset descriptor = 
  let resetValue = descriptor Content.counterResetFactory
  in Stylesheet.simpleProperty "counter-reset" resetValue

counterResets : List Content.CounterResetDescriptor -> 
                Stylesheet.PropertyRuleAppender
counterResets descriptors = 
  let applyDescriptor desc = desc Content.counterResetFactory
      values = List.map applyDescriptor descriptors 
      combinedValues = Property.spaceListValue identity values
  in Stylesheet.simpleProperty "counter-reset" combinedValues

resetTo : String -> Int -> Content.CounterResetDescriptor
resetTo name initVal = \factory -> factory.withInitialValue name initVal
