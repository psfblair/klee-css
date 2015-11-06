module Css.Common (
     browsers, webkit_, moz_, ms_, o_
   , emptyPrefix, toPrefix, toPrefixes
   
  , call

  , sym
  , sym2
  , sym3

  , all
  , auto
  , baseline
  , center
  , inherit
  , normal
  , none
  , visible
  , hidden
  , initial
  , unset
  , other
  , otherPrefixed
  
  ) where

import Css.Internal.Browser as Browser
import Css.Internal.Property exposing 
  ( Value, Prefixed
  , toPrefixed, appendToPrefixedRoot, prefixedValue, stringValue
  )
import Css.Internal.Stylesheet exposing (CssAppender)
import Css.Internal.Common exposing (..)

-------------------------------------------------------------------------------

{-| List of browser prefixes to make experimental properties work in
different browsers. -}
type alias BrowserPrefix = Browser.BrowserPrefix

browsers : Prefixed
browsers = toPrefixes [ webkit_, moz_, ms_, o_, emptyPrefix ]

toPrefix : String -> BrowserPrefix
toPrefix = Browser.toPrefix

toPrefixes : List BrowserPrefix -> Prefixed
toPrefixes browserPrefixes =
  let toPair prefix = (prefix, "")
  in browserPrefixes |> List.map toPair |> toPrefixed
    
webkit_ : BrowserPrefix
webkit_ = Browser.toPrefix "-webkit-"

moz_ : BrowserPrefix
moz_ = Browser.toPrefix "-moz-"

ms_ : BrowserPrefix
ms_ = Browser.toPrefix "-ms-"

o_ : BrowserPrefix
o_ = Browser.toPrefix "-o-"

emptyPrefix : BrowserPrefix
emptyPrefix = Browser.toPrefix ""

-------------------------------------------------------------------------------

{-| Syntax for CSS function call. -}

call : String -> String -> String
call fn arg = fn ++ "(" ++ arg ++ ")"

-------------------------------------------------------------------------------

{- Shorthands for properties that can be applied separately to multiple sides of a box. -}

sym : (a -> a -> a -> a -> (CssAppender b)) -> a -> (CssAppender b)
sym fourSidedFunction value = 
  fourSidedFunction value value value value

sym3 : (a -> a -> a -> a -> (CssAppender b)) -> a -> a -> a -> (CssAppender b)
sym3 fourSidedFunction topBottom left right = 
  fourSidedFunction topBottom left topBottom right

sym2 : (a -> a -> a -> a -> (CssAppender b)) -> a -> a -> (CssAppender b)
sym2 fourSidedFunction topBottom leftRight = 
  fourSidedFunction topBottom leftRight topBottom leftRight

-------------------------------------------------------------------------------

{-| Functions that represent common values shared between multiple CSS properties, such as
 `auto`, `inherit`, `none`, `normal` and several more. These are generic and can be passed to
  property descriptors that take many different type; e.g., borderStyle, borderWidth, etc.
-}
all : All a rec -> a
all factory = factory.all_

auto : Auto a rec -> a
auto factory = factory.auto_

baseline : Baseline a rec -> a
baseline factory = factory.baseline_

center : Center a rec -> a
center factory = factory.center_

inherit : Inherit a rec -> a
inherit factory = factory.inherit_

normal : Normal a rec -> a
normal factory = factory.normal_

none : None a rec -> a
none factory = factory.none_

visible : Visible a rec -> a
visible factory = factory.visible_

hidden : Hidden a rec -> a
hidden factory = factory.hidden_

initial : Initial a rec -> a
initial factory = factory.initial_

unset : Unset a rec -> a
unset factory = factory.unset_

{- The generic `other` and `otherPrefixed` value descriptors are used to escape 
from the type safety introduced by embedding CSS properties in the typed world. 
These functions allow you to extract a specific value type out of any string 
(or list of prefixes with a string) and use it as the value for any property 
that accepts `Other.`
-}
other : String -> Other a rec -> a
other str factory = str |> stringValue |> factory.other_ 

otherPrefixed : List BrowserPrefix -> String -> Other a rec -> a
otherPrefixed browserPrefixes val factory =
    let prefixed = toPrefixes browserPrefixes 
    in appendToPrefixedRoot prefixed val |> prefixedValue |> factory.other_ 
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
