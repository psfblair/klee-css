module Css.Size (

  -- * Size type
    nil
  , unitless

  -- * Size constructors

  , cm
  , mm
  , inches
  , px
  , pt
  , pc
  , em
  , ex
  , pct
  , srem
  , vw
  , vh
  , vmin
  , vmax

  -- * Constructing angles

  , deg
  , rad
  , grad
  , turn
  ) where

import Css.Internal.Property exposing
  ( appendValues, stringValueFactory, floatValueFactory
  )

import Css.Internal.Size exposing (..)

-------------------------------------------------------------------------------
-- This module is tested via the tests of modules that use sizes and angles.
-------------------------------------------------------------------------------

-- | Zero size.
nil : SizeDescriptor a c
nil = \factory -> factory.size (stringValueFactory.value "0")

-- | Unitless size (as recommended for line-height).
unitless : Float -> SizeDescriptor a c
unitless length = \factory -> factory.size (floatValueFactory.value length)

-- | Size in centimeters.
cm : Float -> SizeDescriptor a Abs
cm length = \factory -> factory.size (appendUnits length "cm")

-- | Size in millimeters.
mm : Float -> SizeDescriptor a Abs
mm length = \factory -> factory.size (appendUnits length "mm")

-- | Size in inches (1in = 2.54 cm).
inches : Float -> SizeDescriptor a Abs
inches length = \factory -> factory.size (appendUnits length "in")

-- | Size in pixels.
px : Float -> SizeDescriptor a Abs
px length = \factory -> factory.size (appendUnits length "px")

-- | Size in points (1pt = 1/72 of 1in).
pt : Float -> SizeDescriptor a Abs
pt length = \factory -> factory.size (appendUnits length "pt")

-- | Size in picas (1pc = 12pt).
pc : Float -> SizeDescriptor a Abs
pc length = \factory -> factory.size (appendUnits length "pc")

-------------------------------------------------------------------------------

-- | Size in em's (computed value of the font-size).
em : Float -> SizeDescriptor a Rel
em length = \factory -> factory.size (appendUnits length "em")

-- Double -> Size Rel| Size in ex'es (x-height of the first avaliable font).
ex : Float -> SizeDescriptor a Rel
ex length = \factory -> factory.size (appendUnits length "ex")

-- | Size in percents.
pct : Float -> SizeDescriptor a Rel
pct length = \factory -> factory.size (appendUnits length "%")

-- | Size in rem's (em's, but always relative to the root element).
-- renamed to srem in order not to collide with Basics.rem
srem : Float -> SizeDescriptor a Rel
srem length = \factory -> factory.size (appendUnits length "rem")

-- | Size in vw's (1vw = 1% of viewport width).
vw : Float -> SizeDescriptor a Rel
vw length = \factory -> factory.size (appendUnits length "vw")

-- | Size in vh's (1vh = 1% of viewport height).
vh : Float -> SizeDescriptor a Rel
vh length = \factory -> factory.size (appendUnits length "vh")

-- | Size in vmin's (the smaller of vw or vh).
vmin : Float -> SizeDescriptor a Rel
vmin length = \factory -> factory.size (appendUnits length "vmin")

-- | Size in vmax's (the larger of vw or vh).
vmax : Float -> SizeDescriptor a Rel
vmax length = \factory -> factory.size (appendUnits length "vmax")

-------------------------------------------------------------------------------

-- | Angle in degrees.
deg : Float -> AngleDescriptor Deg
deg amount = \factory -> factory.angle (appendUnits amount "deg")

-- | Angle in radians.
rad : Float -> AngleDescriptor Rad
rad amount = \factory -> factory.angle (appendUnits amount "rad")

-- | Angle in gradians (also knows as gons or grades).
grad : Float -> AngleDescriptor Grad
grad amount = \factory -> factory.angle (appendUnits amount "grad")

-- | Angle in turns.
turn : Float -> AngleDescriptor Turn
turn amount = \factory -> factory.angle (appendUnits amount "turn")
