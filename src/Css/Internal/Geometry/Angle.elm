module Css.Internal.Geometry.Angle
  ( AngleDescriptor, Deg, Rad, Grad, Turn) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias AngleDescriptor a = AngleFactory a -> Angle a

type Angle a -- The type parameter is for Deg, Rad, etc..
  = Angle a Property.Value
  | OtherAngle Property.Value

type Deg = Deg
type Rad = Rad
type Grad = Grad
type Turn = Turn

-------------------------------------------------------------------------------

type alias AngleFactory a =
  { degrees: Property.Value -> Angle Deg
  , radians: Property.Value -> Angle Rad
  , gradians: Property.Value -> Angle Grad
  , turns: Property.Value -> Angle Turn
  , other: Property.Value -> Angle a
  }

angleFactory : AngleFactory a
angleFactory =
  { degrees value = Angle Deg value
  , radians value = Angle Rad value
  , gradians value = Angle Grad value
  , turns value = Angle Turn value
  , other val = OtherAngle val
  }

angleValue : Angle a -> Property.Value 
angleValue angle =
  case angle of
    Angle unit val -> val
    OtherAngle val -> Common.otherValue val
