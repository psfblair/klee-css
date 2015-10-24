module Css.Internal.Size 
  ( Size, SizeDescriptor, Abs, Rel
  , sizeFactory, sizeValue
  , AngleDescriptor, Deg, Rad, Grad, Turn
  , appendUnits
  ) where

import Css.Internal.Property exposing
  (Value, appendValues, stringValue, floatValue)

import Css.Internal.Common exposing
  (autoValue, normalValue, inheritValue, noneValue, otherValue)

-------------------------------------------------------------------------------

-- See the bottom of this file for why there are two type parameters.
type alias SizeDescriptor a c = SizeFactory a c -> a -- c is the constraint type (Abs or Rel)

type Size a -- Phantom type, for type safety. The type parameter is for Abs or Rel.
  = Size Value
  | AutoSize
  | NormalSize
  | InheritSize
  | NoSize
  | OtherSize String

-- | Sizes can be relative like percentages or rems.
type Rel = Rel

-- | Sizes can be absolute like pixels, points, etc.
type Abs = Abs

{-
We won't make an equivalent for Clay's making Size Abs and Size Rel instances of the
Num and Fractional typeclasses so that fromInteger = px . fromInteger, 
fromRational = px . fromRational for Size Abs and 
fromInteger = pct . fromInteger, fromRational = pct . fromRational for Size Rel.
In elm-css, units will always need to be specified.
-}

type alias SizeFactory a c = -- c is the constraint type (Abs or Rel)
  { size: Value -> a -- This has to be more open than Size so we can use sizes in other ways. See e.g., Css.Display.verticalAlign.
  , auto: Size c
  , normal: Size c
  , inherit: Size c
  , none: Size c
  , other: String -> Size c
  }

sizeFactory : SizeFactory (Size a) a
sizeFactory =
  {
    size value = Size value
  , auto = AutoSize
  , normal = NormalSize
  , inherit = InheritSize
  , none = NoSize
  , other str = OtherSize str
  }

sizeValue : Size a -> Value
sizeValue size =
  case size of
    Size val -> val
    AutoSize -> autoValue
    NormalSize -> normalValue
    InheritSize -> inheritValue
    NoSize -> noneValue
    OtherSize str -> otherValue str
    
-------------------------------------------------------------------------------

type alias AngleDescriptor a = AngleFactory a -> Angle a

type Angle a -- Phantom type, for type safety. The type parameter is for Deg, Rad, etc..
  = Angle Value
  | AutoAngle
  | InheritAngle
  | OtherAngle String

type Deg = Deg
type Rad = Rad
type Grad = Grad
type Turn = Turn

-------------------------------------------------------------------------------
-- Again, we won't make an equivalent for Clay's making Angle Deg, Angle Rad, 
-- Angle Grad, and Angle Turn instances of the Num and Fractional typeclasses 
-- so that for Angle Deg fromInteger = deg . fromInteger,
-- fromRational = deg . fromRational and similarly for the others.
-- In elm-css, units will always need to be specified.
-------------------------------------------------------------------------------

type alias AngleFactory a =
  { angle: Value -> Angle a
  , auto: Angle a
  , inherit: Angle a
  , other: String -> Angle a
  }

angleFactory : AngleFactory a
angleFactory =
  { angle value = Angle value
  , auto = AutoAngle
  , inherit = InheritAngle
  , other str = OtherAngle str
  }

angleValue : Angle a -> Value 
angleValue angle =
  case angle of
    Angle val -> val
    AutoAngle -> autoValue
    InheritAngle -> inheritValue
    OtherAngle str -> otherValue str

appendUnits : Float -> String -> Value
appendUnits qty unit =
  appendValues (floatValue qty) (stringValue unit)
  
