module Css.Pseudo where

import Css.Internal.Selector exposing (filterFromString)
import Css.Internal.SelectorCombinators exposing (func)

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
