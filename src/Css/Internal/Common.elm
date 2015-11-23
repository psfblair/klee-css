module Css.Internal.Common 
  ( All
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

  , addCommonValues
  ) where
  
import Css.Internal.Property as Property

{- The following are types that represent records of functions for implementing
common values shared between multiple CSS properties, like `auto`, `inherit`,
`none`, `normal` and several more. These are used by modules within css-elm and
not intended to be used as part of the Css DSL.

Implementation notes:

In Haskell's Clay, the use of type classes and GeneralizedNewtypeDeriving allows
types such as Stroke and Size to derive from the default implementations of the
typeclasses Val, Other, Inherit, Auto, and None. This makes the Haskell code
quite succinct.

In css-elm we instead pass around records of functions to wrap the types. These
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
Stroke to a Value, we have a Value Stroke which looks like this:

strokeValue =
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
{- Other takes a Value so that it can be either a SimpleValue or PrefixedValue. -}
type alias Other    a rec = { rec | other_    : Property.Value -> a }

{- Implementations of the function records for implementing the creation of generic
property values such as `all`, `none`, etc. . Again, these are used internally by
css-elm in other modules and not intended to be used as part of the public Css DSL.
-}
allValue : Property.Value
allValue = Property.stringValue "all"

autoValue : Property.Value
autoValue = Property.stringValue "auto" 

baselineValue : Property.Value
baselineValue = Property.stringValue "baseline" 

centerValue : Property.Value
centerValue = Property.stringValue "center" 

inheritValue : Property.Value
inheritValue = Property.stringValue "inherit" 

normalValue : Property.Value
normalValue = Property.stringValue "normal" 

noneValue : Property.Value
noneValue = Property.stringValue "none" 

visibleValue : Property.Value
visibleValue = Property.stringValue "visible" 

hiddenValue : Property.Value
hiddenValue = Property.stringValue "hidden" 

initialValue : Property.Value
initialValue = Property.stringValue "initial" 

unsetValue : Property.Value
unsetValue = Property.stringValue "unset" 

otherValue : Property.Value -> Property.Value
otherValue val = val

addCommonValues : a -> { a | initial_ : Property.Value
                           , inherit_ : Property.Value
                           , unset_   : Property.Value }
addCommonValues wrappingFactory = 
    let withInitial = { wrappingFactory | initial_ = initialValue }
        withInherit = { withInitial     | inherit_ = inheritValue }
        withUnset   = { withInherit     | unset_   = unsetValue   }
    in withUnset
