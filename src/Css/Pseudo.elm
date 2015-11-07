module Css.Pseudo where

import Css.Internal.Selector exposing (filterFromString)
import Css.Internal.SelectorCombinators exposing (func)

-------------------------------------------------------------------------------

-- List of specific pseudo classes, from:
-- https://developer.mozilla.org/en-US/docs/CSS/Pseudo-classes
after         = ":after"          |> filterFromString
before        = ":before"         |> filterFromString


link          = ":link"           |> filterFromString
visited       = ":visited"        |> filterFromString
active        = ":active"         |> filterFromString
hover         = ":hover"          |> filterFromString
focus         = ":focus"          |> filterFromString
firstChild    = ":first-child"    |> filterFromString
lastChild     = ":last-child"     |> filterFromString


checked       = ":checked"        |> filterFromString
default_      = ":default"        |> filterFromString
disabled      = ":disabled"       |> filterFromString
empty         = ":empty"          |> filterFromString
enabled       = ":enabled"        |> filterFromString
firstOfType   = ":first-of-type"  |> filterFromString
indeterminate = ":indeterminate"  |> filterFromString
inRange       = ":in-range"       |> filterFromString
invalid       = ":invalid"        |> filterFromString
lastOfType    = ":last-of-type"   |> filterFromString
onlyChild     = ":only-child"     |> filterFromString
onlyOfType    = ":only-of-type"   |> filterFromString
optional      = ":optional"       |> filterFromString
outOfRange    = ":out-of-range"   |> filterFromString
required      = ":required"       |> filterFromString
root          = ":root"           |> filterFromString
target        = ":target"         |> filterFromString
valid         = ":valid"          |> filterFromString

lang          n = func "lang"             [n]
nthChild      n = func "nth-child"        [n]
nthLastChild  n = func "nth-last-child"   [n]
nthLastOfType n = func "nth-last-of-type" [n]
nthOfType     n = func "nth-of-type"      [n]

-- TODO not(X)
