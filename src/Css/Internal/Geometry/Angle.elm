module Css.Internal.Geometry.Angle
  ( AngleDescriptor, Deg, Rad, Grad, Turn) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias AngleDescriptor ang = AngleFactory ang -> Angle ang

type Angle ang -- The type parameter is for Deg, Rad, etc..
  = Angle ang Property.Value
  | OtherAngle Property.Value

type Deg = Deg
type Rad = Rad
type Grad = Grad
type Turn = Turn

-------------------------------------------------------------------------------

type alias AngleFactory ang =
  { degrees: Property.Value -> Angle Deg
  , radians: Property.Value -> Angle Rad
  , gradians: Property.Value -> Angle Grad
  , turns: Property.Value -> Angle Turn
  , other: Property.Value -> Angle ang
  }

angleFactory : AngleFactory ang
angleFactory =
  { degrees value = Angle Deg value
  , radians value = Angle Rad value
  , gradians value = Angle Grad value
  , turns value = Angle Turn value
  , other val = OtherAngle val
  }

angleValue : Angle ang -> Property.Value 
angleValue angle =
  case angle of
    Angle unit val -> val
    OtherAngle val -> Common.otherValue val
