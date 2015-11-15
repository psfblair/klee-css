module Css.Internal.Geometry.Linear.Absolute
  ( Abs
  
  -- * Linear size constructors
  
  , abs0
  , cm, mm, inches
  , px, pt, pc
  
  ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

-- | Sizes can be absolute like pixels, points, etc.
type Abs = Abs

absolute : Property.Value -> Linear.Size Abs
absolute lengthValue = Linear.toSize Abs lengthValue

-- | Zero absolute size.
abs0 : Linear.SizeDescriptor a Abs
abs0 = 
  let zeroSize = Property.stringValue "0"
  in \factory -> factory.size (absolute zeroSize)

-- | Size in centimeters.
cm : Float -> Linear.SizeDescriptor a Abs
cm length = 
  let lengthValue = Property.appendUnits length "cm"
  in \factory -> factory.size (absolute lengthValue)

-- | Size in millimeters.
mm : Float -> Linear.SizeDescriptor a Abs
mm length = 
  let lengthValue = Property.appendUnits length "mm"
  in \factory -> factory.size (absolute lengthValue)

-- | Size in inches (1in = 2.54 cm).
inches : Float -> Linear.SizeDescriptor a Abs
inches length = 
  let lengthValue = Property.appendUnits length "in"
  in \factory -> factory.size (absolute lengthValue)

-- | Size in pixels.
px : Float -> Linear.SizeDescriptor a Abs
px length = 
  let lengthValue = Property.appendUnits length "px"
  in \factory -> factory.size (absolute lengthValue)

-- | Size in points (1pt = 1/72 of 1in).
pt : Float -> Linear.SizeDescriptor a Abs
pt length = 
  let lengthValue = Property.appendUnits length "pt"
  in \factory -> factory.size (absolute lengthValue)

-- | Size in picas (1pc = 12pt).
pc : Float -> Linear.SizeDescriptor a Abs
pc length = 
  let lengthValue = Property.appendUnits length "pc"
  in \factory -> factory.size (absolute lengthValue)
  
