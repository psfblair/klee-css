module Css.Common where

import Css.Property exposing (Value, PrefixedOrNot, stringValueWrapper)

{-| A bunch of records of functions representing common values shared between
multiple CSS properties, like `Auto`, `Inherit`, `None`, `Normal` and several more.
-}
type alias All      a = {  all      : a }
type alias Auto     a = {  auto     : a }
type alias Baseline a = {  baseline : a }
type alias Center   a = {  center   : a }
type alias Inherit  a = {  inherit  : a }
type alias None     a = {  none     : a }
type alias Normal   a = {  normal   : a }
type alias Visible  a = {  visible  : a }
type alias Hidden   a = {  hidden   : a }
type alias Initial  a = {  initial  : a }
type alias Unset    a = {  unset    : a }

-- The Other type alias is used to escape from the type safety introduced by
-- embedding CSS properties in the typed world. `Other` allows you to extract
-- a specific value type out of any `Value`.
type alias Other a = {  other: Value -> a }

allWrapper : All Value
allWrapper  = { all = stringValueWrapper.value "all" }

autoWrapper : Auto Value
autoWrapper = { auto = stringValueWrapper.value "auto" }

baselineWrapper : Baseline Value
baselineWrapper = { baseline = stringValueWrapper.value "baseline" }

centerWrapper : Center Value
centerWrapper = { center = stringValueWrapper.value "center" }

inheritWrapper : Inherit Value
inheritWrapper = { inherit  = stringValueWrapper.value "inherit" }

normalWrapper : Normal Value
normalWrapper = { normal = stringValueWrapper.value "normal" }

noneWrapper : None Value
noneWrapper = { none = stringValueWrapper.value "none" }

visibleWrapper : Visible Value
visibleWrapper = { visible  = stringValueWrapper.value "visible" }

hiddenWrapper : Hidden Value
hiddenWrapper = { hidden = stringValueWrapper.value "hidden" }

initialWrapper : Initial Value
initialWrapper = { initial  = stringValueWrapper.value "initial" }

unsetWrapper : Unset Value
unsetWrapper = { unset = stringValueWrapper.value "unset" }

otherWrapper : Other Value
otherWrapper = { other = identity }

{-| List of browser prefixes to make experimental properties work in
different browsers. -}

browsers : PrefixedOrNot
browsers =
  Css.Property.Prefixed
  [ ( "-webkit-", "" )
  , (    "-moz-", "" )
  , (     "-ms-", "" )
  , (      "-o-", "" )
  , (         "", "" )
  ]

{-| Syntax for CSS function call. -}

call : String -> String -> String
call fn arg = fn ++ "(" ++ arg ++ ")"
