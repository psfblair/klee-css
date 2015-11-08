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
  | OtherSize Value

-- | Sizes can be relative like percentages or rems.
type Rel = Rel

-- | Sizes can be absolute like pixels, points, etc.
type Abs = Abs

{-
We won't make an equivalent for Clay's making Size Abs and Size Rel instances of the
Num and Fractional typeclasses so that fromInteger = px . fromInteger, 
fromRational = px . fromRational for Size Abs and 
fromInteger = pct . fromInteger, fromRational = pct . fromRational for Size Rel.
In css-elm, units will always need to be specified.
-}

type alias SizeFactory a c = -- c is the constraint type (Abs or Rel)
  { size: Value -> a -- This has to be more open than Size so we can use sizes in other ways. See e.g., Css.Display.verticalAlign.
  , other: Value -> Size c
  }

sizeFactory : SizeFactory (Size a) a
sizeFactory =
  {
    size value = Size value
  , other val = OtherSize val
  }

sizeValue : Size a -> Value
sizeValue size =
  case size of
    Size val -> val
    OtherSize val -> otherValue val
    
-------------------------------------------------------------------------------

type alias AngleDescriptor a = AngleFactory a -> Angle a

type Angle a -- Phantom type, for type safety. The type parameter is for Deg, Rad, etc..
  = Angle Value
  | OtherAngle Value

type Deg = Deg
type Rad = Rad
type Grad = Grad
type Turn = Turn

-------------------------------------------------------------------------------
-- Again, we won't make an equivalent for Clay's making Angle Deg, Angle Rad, 
-- Angle Grad, and Angle Turn instances of the Num and Fractional typeclasses 
-- so that for Angle Deg fromInteger = deg . fromInteger,
-- fromRational = deg . fromRational and similarly for the others.
-- In css-elm, units will always need to be specified.
-------------------------------------------------------------------------------

type alias AngleFactory a =
  { angle: Value -> Angle a
  , other: Value -> Angle a
  }

angleFactory : AngleFactory a
angleFactory =
  { angle value = Angle value
  , other val = OtherAngle val
  }

angleValue : Angle a -> Value 
angleValue angle =
  case angle of
    Angle val -> val
    OtherAngle val -> otherValue val

appendUnits : Float -> String -> Value
appendUnits qty unit =
  appendValues (floatValue qty) (stringValue unit)
  
