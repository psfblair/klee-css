module Css.Internal.Geometry.Angle
  ( AngleDescriptor, Deg, Rad, Grad, Turn
  
  -- * Constructing angles

  , deg, rad, grad, turn
  
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias AngleDescriptor a = AngleFactory a -> Angle a

type Angle a -- Phantom type, for type safety. The type parameter is for Deg, Rad, etc..
  = Angle Property.Value
  | OtherAngle Property.Value

type Deg = Deg
type Rad = Rad
type Grad = Grad
type Turn = Turn

-------------------------------------------------------------------------------

-- | Angle in degrees.
deg : Float -> AngleDescriptor Deg
deg amount = \factory -> factory.angle (Property.appendUnits amount "deg")

-- | Angle in radians.
rad : Float -> AngleDescriptor Rad
rad amount = \factory -> factory.angle (Property.appendUnits amount "rad")

-- | Angle in gradians (also knows as gons or grades).
grad : Float -> AngleDescriptor Grad
grad amount = \factory -> factory.angle (Property.appendUnits amount "grad")

-- | Angle in turns.
turn : Float -> AngleDescriptor Turn
turn amount = \factory -> factory.angle (Property.appendUnits amount "turn")

-------------------------------------------------------------------------------

type alias AngleFactory a =
  { angle: Property.Value -> Angle a
  , other: Property.Value -> Angle a
  }

angleFactory : AngleFactory a
angleFactory =
  { angle value = Angle value
  , other val = OtherAngle val
  }

angleValue : Angle a -> Property.Value 
angleValue angle =
  case angle of
    Angle val -> val
    OtherAngle val -> Common.otherValue val
