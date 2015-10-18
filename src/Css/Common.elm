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

  , allValue 
  , autoValue 
  , baselineValue 
  , centerValue 
  , inheritValue 
  , normalValue 
  , noneValue 
  , visibleValue 
  , hiddenValue 
  , initialValue 
  , unsetValue 
  , otherValue 

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

{- The generic `other` value descriptor is used to escape from the type safety
introduced by embedding CSS properties in the typed world. The `other` function
allows you to extract a specific value type out of any `Value` and use it as
the value for any property that accepts `Other.`
-}
other : String -> Other a rec -> a
other str factory = factory.other_ str

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

type alias All      a rec = { rec | all_      : a }
type alias Auto     a rec = { rec | auto_     : a }
type alias Baseline a rec = { rec | baseline_ : a }
type alias Center   a rec = { rec | center_   : a }
type alias Inherit  a rec = { rec | inherit_  : a }
type alias None     a rec = { rec | none_     : a }
type alias Normal   a rec = { rec | normal_   : a }
type alias Visible  a rec = { rec | visible_  : a }
type alias Hidden   a rec = { rec | hidden_   : a }
type alias Initial  a rec = { rec | initial_  : a }
type alias Unset    a rec = { rec | unset_    : a }
type alias Other    a rec = { rec | other_    : String -> a }

{- Implementations of the function records for implementing the creation of generic
property values such as `all`, `none`, etc. . Again, these are used internally by
elm-css in other modules and not intended to be used as part of the public Css DSL.
-}
allValue : Value
allValue = stringValueFactory.value "all"

autoValue : Value
autoValue = stringValueFactory.value "auto" 

baselineValue : Value
baselineValue = stringValueFactory.value "baseline" 

centerValue : Value
centerValue = stringValueFactory.value "center" 

inheritValue : Value
inheritValue = stringValueFactory.value "inherit" 

normalValue : Value
normalValue = stringValueFactory.value "normal" 

noneValue : Value
noneValue = stringValueFactory.value "none" 

visibleValue : Value
visibleValue = stringValueFactory.value "visible" 

hiddenValue : Value
hiddenValue = stringValueFactory.value "hidden" 

initialValue : Value
initialValue = stringValueFactory.value "initial" 

unsetValue : Value
unsetValue = stringValueFactory.value "unset" 

otherValue : String -> Value
otherValue str = stringValueFactory.value str 
