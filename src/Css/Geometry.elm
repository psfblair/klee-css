module Css.Geometry 
  (
  -- * Size constructors

    nil, unitless
  , cm, mm, inches
  , px, pt, pc
  , pct
  , em, srem, ex
  , vw, vh, vmin, vmax

  -- * Constructing angles

  , deg, rad, grad, turn
  
  -- * Constructing positions
    
  , sideLeft, sideCenter, sideRight
  , sideTop, sideMiddle, sideBottom

  -- * Positioning properties.
  , top, left, bottom, right

  -- * Sizing properties.
  , width, height, minWidth, minHeight, maxWidth, maxHeight

  -- * Padding.
  , padding
  , paddingTop, paddingLeft, paddingRight, paddingBottom

  -- * Margin.
  , margin
  , marginTop, marginLeft, marginRight, marginBottom
  ) where

import Css.Internal.Geometry.Angle as Angle
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Linear.Absolute as Absolute
import Css.Internal.Geometry.Linear.Relative as Relative
import Css.Internal.Geometry.Sides as Sides
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

-- | Zero size.
nil : Linear.SizeDescriptor a c
nil = Linear.nil

-- | Unitless size (as recommended for line-height).
unitless : Float -> Linear.SizeDescriptor a c
unitless = Linear.unitless

-------------------------------------------------------------------------------
-- Absolute sizes

-- | Size in centimeters.
cm : Float -> Linear.SizeDescriptor a Absolute.Abs
cm = Absolute.cm

-- | Size in millimeters.
mm : Float -> Linear.SizeDescriptor a Absolute.Abs
mm = Absolute.mm

-- | Size in inches (1in = 2.54 cm).
inches : Float -> Linear.SizeDescriptor a Absolute.Abs
inches = Absolute.inches

-- | Size in pixels.
px : Float -> Linear.SizeDescriptor a Absolute.Abs
px = Absolute.px

-- | Size in points (1pt = 1/72 of 1in).
pt : Float -> Linear.SizeDescriptor a Absolute.Abs
pt = Absolute.pt

-- | Size in picas (1pc = 12pt).
pc : Float -> Linear.SizeDescriptor a Absolute.Abs
pc = Absolute.pc

-------------------------------------------------------------------------------
-- Relative sizes

-- | Size in percents.
pct : Float -> Linear.SizeDescriptor a Relative.Rel
pct = Relative.pct

-- | Size in em's (computed value of the font-size).
em : Float -> Linear.SizeDescriptor a Relative.Rel
em = Relative.em

-- | Size in rem's (em's, but always relative to the root element).
-- renamed to srem in order not to collide with Basics.rem
srem : Float -> Linear.SizeDescriptor a Relative.Rel
srem = Relative.srem

-- Double -> Size Rel| Size in ex'es (x-height of the first avaliable font).
ex : Float -> Linear.SizeDescriptor a Relative.Rel
ex = Relative.ex

-- | Size in vw's (1vw = 1% of viewport width).
vw : Float -> Linear.SizeDescriptor a Relative.Rel
vw = Relative.vw

-- | Size in vh's (1vh = 1% of viewport height).
vh : Float -> Linear.SizeDescriptor a Relative.Rel
vh = Relative.vh

-- | Size in vmin's (the smaller of vw or vh).
vmin : Float -> Linear.SizeDescriptor a Relative.Rel
vmin = Relative.vmin

-- | Size in vmax's (the larger of vw or vh).
vmax : Float -> Linear.SizeDescriptor a Relative.Rel
vmax = Relative.vmax

-------------------------------------------------------------------------------

-- | Angle in degrees.
deg : Float -> Angle.AngleDescriptor Angle.Deg
deg = Angle.deg

-- | Angle in radians.
rad : Float -> Angle.AngleDescriptor Angle.Rad
rad = Angle.rad

-- | Angle in gradians (also knows as gons or grades).
grad : Float -> Angle.AngleDescriptor Angle.Grad
grad = Angle.grad

-- | Angle in turns.
turn : Float -> Angle.AngleDescriptor Angle.Turn
turn = Angle.turn

-------------------------------------------------------------------------------
-- Positioning

sideLeft : Sides.HorizontalSide
sideLeft = Sides.sideLeft

sideCenter : Sides.HorizontalSide
sideCenter = Sides.sideCenter

sideRight : Sides.HorizontalSide
sideRight = Sides.sideRight

sideTop : Sides.VerticalSide
sideTop = Sides.sideTop

sideMiddle : Sides.VerticalSide
sideMiddle = Sides.sideMiddle

sideBottom : Sides.VerticalSide
sideBottom = Sides.sideBottom

-------------------------------------------------------------------------------

top : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
top = Linear.top 

left : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
left = Linear.left

bottom : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
bottom = Linear.bottom

right : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
right = Linear.right

width : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
width = Linear.width

height : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
height = Linear.height

minWidth : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
minWidth = Linear.minWidth

minHeight : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
minHeight = Linear.minHeight

maxWidth : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
maxWidth = Linear.maxWidth

maxHeight : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
maxHeight = Linear.maxHeight

-------------------------------------------------------------------------------
padding : Linear.SizeDescriptor (Linear.Size a) a -> 
          Linear.SizeDescriptor (Linear.Size a) a -> 
          Linear.SizeDescriptor (Linear.Size a) a -> 
          Linear.SizeDescriptor (Linear.Size a) a -> 
          Stylesheet.PropertyRuleAppender
padding = Linear.padding

paddingTop : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
paddingTop = Linear.paddingTop

paddingLeft : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
paddingLeft = Linear.paddingLeft

paddingRight : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
paddingRight = Linear.paddingRight

paddingBottom : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
paddingBottom = Linear.paddingBottom

-------------------------------------------------------------------------------
margin : Linear.SizeDescriptor (Linear.Size a) a -> 
         Linear.SizeDescriptor (Linear.Size a) a -> 
         Linear.SizeDescriptor (Linear.Size a) a -> 
         Linear.SizeDescriptor (Linear.Size a) a -> 
         Stylesheet.PropertyRuleAppender
margin = Linear.margin

marginTop : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
marginTop = Linear.marginTop

marginLeft : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
marginLeft = Linear.marginLeft

marginRight : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
marginRight = Linear.marginRight

marginBottom : Linear.SizeDescriptor (Linear.Size a) a -> Stylesheet.PropertyRuleAppender
marginBottom = Linear.marginBottom

  
  
