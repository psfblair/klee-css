module Css.Size (

  -- * Size type
    Size
  , Abs
  , Rel
  , nil
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
  , rem
  , vw
  , vh
  , vmin
  , vmax

  -- * Angle type

  , Angle
  , Deg
  , Rad
  , Grad
  , Turn

  -- * Constructing angles

  , deg
  , rad
  , grad
  , turn

  -- * For use by other modules
  , SizeDescriptor, sizeFactory, sizeValueFactory
  ) where

import Css.Internal.Property exposing
  ( Value, ValueFactory
  , appendUnits, stringValueFactory, floatValueFactory
  )

import Css.Common exposing
  ( Auto, Normal, Inherit, None, Other
  , autoValueFactory, normalValueFactory, inheritValueFactory, noneValueFactory
  , otherValueFactory
  )

-------------------------------------------------------------------------------

-- | Sizes can be relative like percentages or rems.
type Rel = Rel

-- | Sizes can be absolute like pixels, points, etc.
type Abs = Abs

type Size a -- Phantom type, for type safety. The type parameter is for Abs or Rel.
  = Size Value
  | AutoSize
  | NormalSize
  | InheritSize
  | NoSize
  | OtherSize Value

type alias SizeFactory a =
  { size: Value -> Size a
  , auto: Size a
  , normal: Size a
  , inherit: Size a
  , none: Size a
  , other: Value -> Size a
  }

type alias SizeDescriptor a = SizeFactory a -> Size a

sizeFactory : SizeFactory a
sizeFactory =
  {
    size value = Size value
  , auto = AutoSize
  , normal = NormalSize
  , inherit = InheritSize
  , none = NoSize
  , other val = OtherSize val
  }

sizeValueFactory : ValueFactory (Size a)
sizeValueFactory =
  { value size =
      case size of
        Size val -> val
        AutoSize -> autoValueFactory.auto
        NormalSize -> normalValueFactory.normal
        InheritSize -> inheritValueFactory.inherit
        NoSize -> noneValueFactory.none
        OtherSize val -> otherValueFactory.other val
  }

-- | Zero size.
nil : SizeDescriptor a
nil = \factory -> factory.size (stringValueFactory.value "0")

-- | Unitless size (as recommended for line-height).
unitless : Float -> SizeDescriptor a
unitless length = \factory -> factory.size (floatValueFactory.value length)

-- | Size in centimeters.
cm : Float -> SizeDescriptor Abs
cm length = \factory -> factory.size (appendUnits length "cm")

-- | Size in millimeters.
mm : Float -> SizeDescriptor Abs
mm length = \factory -> factory.size (appendUnits length "mm")

-- | Size in inches (1in = 2.54 cm).
inches : Float -> SizeDescriptor Abs
inches length = \factory -> factory.size (appendUnits length "in")

-- | Size in pixels.
px : Float -> SizeDescriptor Abs
px length = \factory -> factory.size (appendUnits length "px")

-- | Size in points (1pt = 1/72 of 1in).
pt : Float -> SizeDescriptor Abs
pt length = \factory -> factory.size (appendUnits length "pt")

-- | Size in picas (1pc = 12pt).
pc : Float -> SizeDescriptor Abs
pc length = \factory -> factory.size (appendUnits length "pc")

-------------------------------------------------------------------------------

-- | Size in em's (computed value of the font-size).
em : Float -> SizeDescriptor Rel
em length = \factory -> factory.size (appendUnits length "em")

-- Double -> Size Rel| Size in ex'es (x-height of the first avaliable font).
ex : Float -> SizeDescriptor Rel
ex length = \factory -> factory.size (appendUnits length "ex")

-- | Size in percents.
pct : Float -> SizeDescriptor Rel
pct length = \factory -> factory.size (appendUnits length "%")

-- | Size in rem's (em's, but always relative to the root element).
rem : Float -> SizeDescriptor Rel
rem length = \factory -> factory.size (appendUnits length "rem")

-- | Size in vw's (1vw = 1% of viewport width).
vw : Float -> SizeDescriptor Rel
vw length = \factory -> factory.size (appendUnits length "vw")

-- | Size in vh's (1vh = 1% of viewport height).
vh : Float -> SizeDescriptor Rel
vh length = \factory -> factory.size (appendUnits length "vh")

-- | Size in vmin's (the smaller of vw or vh).
vmin : Float -> SizeDescriptor Rel
vmin length = \factory -> factory.size (appendUnits length "vmin")

-- | Size in vmax's (the larger of vw or vh).
vmax : Float -> SizeDescriptor Rel
vmax length = \factory -> factory.size (appendUnits length "vmax")

-------------------------------------------------------------------------------
{-
We won't make an equivalent for Clay's making Size Abs and Size Rel instances of the
Num and Fractional typeclasses so that fromInteger = px . fromInteger, fromRational = px . fromRational
for Size Abs and fromInteger = pct . fromInteger, fromRational = pct . fromRational for Size Rel.
In elm-css, units will always need to be specified.
-}
-------------------------------------------------------------------------------

type Deg = Deg
type Rad = Rad
type Grad = Grad
type Turn = Turn

type Angle a -- Phantom type, for type safety. The type parameter is for Deg, Rad, etc..
  = Angle Value
  | AutoAngle
  | InheritAngle
  | OtherAngle Value

type alias AngleFactory a =
  { angle: Value -> Angle a
  , auto: Angle a
  , inherit: Angle a
  , other: Value -> Angle a
  }

type alias AngleDescriptor a = AngleFactory a -> Angle a

angleFactory : AngleFactory a
angleFactory =
  { angle value = Angle value
  , auto = AutoAngle
  , inherit = InheritAngle
  , other value = OtherAngle value
  }

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

-------------------------------------------------------------------------------
-- Again, we won't make an equivalent for Clay's making Angle Deg, Angle Rad, Angle Grad, and Angle Turn
-- instances of the Num and Fractional typeclasses so that for Angle Deg fromInteger = deg . fromInteger,
-- fromRational = deg . fromRational and similarly for the others.
-- In elm-css, units will always need to be specified.
-------------------------------------------------------------------------------
