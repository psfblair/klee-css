module Css.Internal.Geometry.Linear.Absolute
  ( Abs
  
  -- * Linear size constructors

  , cm, mm, inches
  , px, pt, pc
  
  ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

-- | Sizes can be absolute like pixels, points, etc.
type Abs = Abs

-- | Size in centimeters.
cm : Float -> Linear.SizeDescriptor a Abs
cm length = \factory -> factory.size (Property.appendUnits length "cm")

-- | Size in millimeters.
mm : Float -> Linear.SizeDescriptor a Abs
mm length = \factory -> factory.size (Property.appendUnits length "mm")

-- | Size in inches (1in = 2.54 cm).
inches : Float -> Linear.SizeDescriptor a Abs
inches length = \factory -> factory.size (Property.appendUnits length "in")

-- | Size in pixels.
px : Float -> Linear.SizeDescriptor a Abs
px length = \factory -> factory.size (Property.appendUnits length "px")

-- | Size in points (1pt = 1/72 of 1in).
pt : Float -> Linear.SizeDescriptor a Abs
pt length = \factory -> factory.size (Property.appendUnits length "pt")

-- | Size in picas (1pc = 12pt).
pc : Float -> Linear.SizeDescriptor a Abs
pc length = \factory -> factory.size (Property.appendUnits length "pc")
  
