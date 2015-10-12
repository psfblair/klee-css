module Css.Common (
   browsers
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

  , All
  , Auto
  , Baseline
  , Center
  , Inherit
  , None
  , Normal
  , Visible
  , Hidden
  , Initial
  , Unset
  , Other

  , allValueFactory
  , autoValueFactory
  , baselineValueFactory
  , centerValueFactory
  , inheritValueFactory
  , normalValueFactory
  , noneValueFactory
  , visibleValueFactory
  , hiddenValueFactory
  , initialValueFactory
  , unsetValueFactory
  , otherValueFactory

  ) where

import Css.Internal.Property exposing (Value, PrefixedOrNot (..), stringValueFactory)
import Css.Internal.Stylesheet exposing (CssAppender)

-------------------------------------------------------------------------------

{-| List of browser prefixes to make experimental properties work in
different browsers. -}

browsers : PrefixedOrNot
browsers =
  Prefixed
  [ ( "-webkit-", "" )
  , (    "-moz-", "" )
  , (     "-ms-", "" )
  , (      "-o-", "" )
  , (         "", "" )
  ]

-------------------------------------------------------------------------------

{-| Syntax for CSS function call. -}

call : String -> String -> String
call fn arg = fn ++ "(" ++ arg ++ ")"

-------------------------------------------------------------------------------

{- Shorthands for properties that can be applied separately to multiple sides of a box. -}

sym : (a -> a -> a -> a -> (CssAppender b)) -> a -> (CssAppender b)
sym fourSidedFunction value = fourSidedFunction value value value value

sym3 : (a -> a -> a -> a -> (CssAppender b)) -> a -> a -> a -> (CssAppender b)
sym3 fourSidedFunction topBottom left right = fourSidedFunction topBottom left topBottom right

sym2 : (a -> a -> a -> a -> (CssAppender b)) -> a -> a -> (CssAppender b)
sym2 fourSidedFunction topBottom leftRight = fourSidedFunction topBottom leftRight topBottom leftRight

-------------------------------------------------------------------------------

{-| Functions that represent common values shared between multiple CSS properties, such as
 `auto`, `inherit`, `none`, `normal` and several more. These are generic and can be passed to
  property descriptors that take many different type; e.g., borderStyle, borderWidth, etc.
-}
all : All a -> a
all factory = factory.all

auto : Auto a -> a
auto factory = factory.auto

baseline : Baseline a -> a
baseline factory = factory.baseline

center : Center a -> a
center factory = factory.center

inherit : Inherit a -> a
inherit factory = factory.inherit

normal : Normal a -> a
normal factory = factory.normal

none : None a -> a
none factory = factory.none

visible : Visible a -> a
visible factory = factory.visible

hidden : Hidden a -> a
hidden factory = factory.hidden

initial : Initial a -> a
initial factory = factory.initial

unset : Unset a -> a
unset factory = factory.unset

{- The generic `other` value descriptor is used to escape from the type safety
introduced by embedding CSS properties in the typed world. The `other` function
allows you to extract a specific value type out of any `Value` and use it as
the value for any property that accepts `Other.`
-}
other : Value -> Other a -> a
other value factory = factory.other value

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- The following functions are not part of the DSL. They integrate the generic
-- values into the rest of the Css framework.

{- The following are types that represent records of functions for implementing
common values shared between multiple CSS properties, like `auto`, `inherit`,
`none`, `normal` and several more. These are used by modules within elm-css and
not intended to be used as part of the Css DSL.

Implementation notes:

In Haskell's Clay, the use of type classes and GeneralizedNewtypeDeriving allows
types such as Stroke and Size to derive from the default implementations of the
typeclasses Val, Other, Inherit, Auto, and None. This makes the Haskell code
quite succinct.

In elm-css we instead pass around records of functions to wrap the types. These
are referred to as factories. Example:

 So then suppose we have a border-stroke property:

      borderStyle solid

 If this expression were to return a Stroke, then what would you do about:

      borderStyle none

 which should return a None? So we create a union type which is

 type Stroke
   = Stroke Value
   | NoStroke
   ... etc

and then the argument passed to borderStroke will be a strokeFactory, and
borderStroke will then call the factory and wrap the result in the appropriate
union type constructor. The factory is a record type that contains functions for
all the various types that need to be wrapped; i.e.

  type alias StrokeFactory =
    { stroke: String -> Stroke
    , none: Stroke
      ... etc.
    }

The implementation of the strokeFactory will then look like this:

strokeFactory =
  {
    stroke str = Stroke str
    none = NoStroke
      ... etc.
  }

The None that is wrapped in the NoStroke is itself a record containing a function
none that turns a None into a value, so that when it comes time to convert the
Stroke to a Value, we have a ValueFactory Stroke which looks like this:

strokeValueFactory =
  { value stroke =
      case stroke of
        Stroke val -> val
        NoStroke -> noneFactory.none
        ... etc
  }

To go back to borderStyle, the implementation will look like this:

  borderStyle strokeDescriptor =
    let stroke = strokeDescriptor strokeFactory
    ...

Now you can get a stroke by calling both
    borderStroke solid
    borderStroke none

where solid is:
    solid factory = factory.stroke "solid"

and none (defined above) is:
    none factory = factory.none

Both produce an instance of the Stroke union type. This is made possible by
structural typing: The StrokeFactory record type has a field none, which also
makes that record of type None. Thus, the none function above, which takes a
record of type None, will accept a record of type StrokeFactory. This means that
the none function can be passed to borderStroke, which requires a function that
takes a StrokeFactory.
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
type alias Other    a = {  other: Value -> a }

{- Implementations of the function records for implementing the creation of generic
property values such as `all`, `none`, etc. . Again, these are used internally by
elm-css in other modules and not intended to be used as part of the public Css DSL.
-}
allValueFactory : All Value
allValueFactory  = { all = stringValueFactory.value "all" }

autoValueFactory : Auto Value
autoValueFactory = { auto = stringValueFactory.value "auto" }

baselineValueFactory : Baseline Value
baselineValueFactory = { baseline = stringValueFactory.value "baseline" }

centerValueFactory : Center Value
centerValueFactory = { center = stringValueFactory.value "center" }

inheritValueFactory : Inherit Value
inheritValueFactory = { inherit  = stringValueFactory.value "inherit" }

normalValueFactory : Normal Value
normalValueFactory = { normal = stringValueFactory.value "normal" }

noneValueFactory : None Value
noneValueFactory = { none = stringValueFactory.value "none" }

visibleValueFactory : Visible Value
visibleValueFactory = { visible  = stringValueFactory.value "visible" }

hiddenValueFactory : Hidden Value
hiddenValueFactory = { hidden = stringValueFactory.value "hidden" }

initialValueFactory : Initial Value
initialValueFactory = { initial  = stringValueFactory.value "initial" }

unsetValueFactory : Unset Value
unsetValueFactory = { unset = stringValueFactory.value "unset" }

otherValueFactory : Other Value
otherValueFactory = { other value = value }
