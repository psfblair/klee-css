module Css.Geometry 
  (
  -- * Linear size constructors.
    
    abs0, rel0, unitless
  , cm, mm, inches
  , px, pt, pc
  , pct
  , em, rems, ex
  , vw, vh, vmin, vmax

  -- * Linear size composers.
    
  , rect
  
  -- * Angle constructors.

  , deg, rad, grad, turn
  
  -- * Position constructors.
    
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
import Css.Internal.Geometry.Margin as Margin
import Css.Internal.Geometry.Padding as Padding
import Css.Internal.Geometry.Sides as Sides
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

-- | Unitless size (as recommended for line-height).
unitless : Float -> Linear.NubSizeDescriptor rec {}
unitless length = \factory -> factory.size (Linear.unitlessSize length)

-------------------------------------------------------------------------------
-- Absolute sizes

-- | Zero absolute size.
abs0 : Linear.NubSizeDescriptor rec Linear.Abs
abs0 = 
  let zeroSize = Property.stringValue "0"
  in \factory -> factory.size (Linear.absolute zeroSize)

-- | Size in centimeters.
cm : Float -> Linear.NubSizeDescriptor rec Linear.Abs
cm length = 
  let lengthValue = Property.appendUnits length "cm"
  in \factory -> factory.size (Linear.absolute lengthValue)

-- | Size in millimeters.
mm : Float -> Linear.NubSizeDescriptor rec Linear.Abs
mm length = 
  let lengthValue = Property.appendUnits length "mm"
  in \factory -> factory.size (Linear.absolute lengthValue)

-- | Size in inches (1in = 2.54 cm).
inches : Float -> Linear.NubSizeDescriptor rec Linear.Abs
inches length = 
  let lengthValue = Property.appendUnits length "in"
  in \factory -> factory.size (Linear.absolute lengthValue)

-- | Size in pixels.
px : Float -> Linear.NubSizeDescriptor rec Linear.Abs
px length = 
  let lengthValue = Property.appendUnits length "px"
  in \factory -> factory.size (Linear.absolute lengthValue)

-- | Size in points (1pt = 1/72 of 1in).
pt : Float -> Linear.NubSizeDescriptor rec Linear.Abs
pt length = 
  let lengthValue = Property.appendUnits length "pt"
  in \factory -> factory.size (Linear.absolute lengthValue)

-- | Size in picas (1pc = 12pt).
pc : Float -> Linear.NubSizeDescriptor rec Linear.Abs
pc length = 
  let lengthValue = Property.appendUnits length "pc"
  in \factory -> factory.size (Linear.absolute lengthValue)

-------------------------------------------------------------------------------
-- Relative sizes

-- | Zero relative size.
rel0 : Linear.NubSizeDescriptor rec Linear.Rel
rel0 = 
  let zeroSize = Property.stringValue "0"
  in \factory -> factory.size (Linear.relative zeroSize)

-- | Size in percents.
pct : Float -> Linear.NubSizeDescriptor rec Linear.Rel
pct length = 
  let lengthValue = Property.appendUnits length "%"
  in \factory -> factory.size (Linear.relative lengthValue)

-- | Size in em's (computed value of the font-size).
em : Float -> Linear.NubSizeDescriptor rec Linear.Rel
em length =  
  let lengthValue = Property.appendUnits length "em"
  in \factory -> factory.size (Linear.relative lengthValue)

-- | Size in rem's (em's, but always relative to the root element).
-- named rems in order not to collide with Basics.rem
rems : Float -> Linear.NubSizeDescriptor rec Linear.Rel
rems length =  
  let lengthValue = Property.appendUnits length "rem"
  in \factory -> factory.size (Linear.relative lengthValue)

-- Double -> Size Rel| Size in ex'es (x-height of the first avaliable font).
ex : Float -> Linear.NubSizeDescriptor rec Linear.Rel
ex length =  
  let lengthValue = Property.appendUnits length "ex"
  in \factory -> factory.size (Linear.relative lengthValue)

-- | Size in vw's (1vw = 1% of viewport width).
vw : Float -> Linear.NubSizeDescriptor rec Linear.Rel
vw length =  
  let lengthValue = Property.appendUnits length "vw"
  in \factory -> factory.size (Linear.relative lengthValue)

-- | Size in vh's (1vh = 1% of viewport height).
vh : Float -> Linear.NubSizeDescriptor rec Linear.Rel
vh length =  
  let lengthValue = Property.appendUnits length "vh"
  in \factory -> factory.size (Linear.relative lengthValue)

-- | Size in vmin's (the smaller of vw or vh).
vmin : Float -> Linear.NubSizeDescriptor rec Linear.Rel
vmin length =  
  let lengthValue = Property.appendUnits length "vmin"
  in \factory -> factory.size (Linear.relative lengthValue)

-- | Size in vmax's (the larger of vw or vh).
vmax : Float -> Linear.NubSizeDescriptor rec Linear.Rel
vmax length =  
  let lengthValue = Property.appendUnits length "vmax"
  in \factory -> factory.size (Linear.relative lengthValue)

-------------------------------------------------------------------------------

-- | Angle in degrees.
deg : Float -> Angle.AngleDescriptor Angle.Deg
deg amount = \factory -> factory.degrees (Property.appendUnits amount "deg")

-- | Angle in radians.
rad : Float -> Angle.AngleDescriptor Angle.Rad
rad amount = \factory -> factory.radians (Property.appendUnits amount "rad")

-- | Angle in gradians (also knows as gons or grades).
grad : Float -> Angle.AngleDescriptor Angle.Grad
grad amount = \factory -> factory.gradians (Property.appendUnits amount "grad")

-- | Angle in turns.
turn : Float -> Angle.AngleDescriptor Angle.Turn
turn amount = \factory -> factory.turns (Property.appendUnits amount "turn")

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

top : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
top sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.autoSizableFactory 
  in Stylesheet.simpleProperty "top" sizeValue

left : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
left sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.autoSizableFactory 
  in Stylesheet.simpleProperty "left" sizeValue

bottom : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
bottom sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.autoSizableFactory 
  in Stylesheet.simpleProperty "bottom" sizeValue

right : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
right sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.autoSizableFactory 
  in Stylesheet.simpleProperty "right" sizeValue

width : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
width sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.autoSizableFactory 
  in Stylesheet.simpleProperty "width" sizeValue

height : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
height sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.autoSizableFactory 
  in Stylesheet.simpleProperty "height" sizeValue

-------------------------------------------------------------------------------

minWidth : Linear.SizeDescriptor sz -> Stylesheet.PropertyRuleAppender
minWidth sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.basicSizeFactory 
  in Stylesheet.simpleProperty "min-width" sizeValue

minHeight : Linear.SizeDescriptor sz -> Stylesheet.PropertyRuleAppender
minHeight sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.basicSizeFactory 
  in Stylesheet.simpleProperty "min-height" sizeValue

maxWidth : Linear.SizeDescriptorWithNone sz -> Stylesheet.PropertyRuleAppender
maxWidth sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.sizeFactoryWithNone 
  in Stylesheet.simpleProperty "max-width" sizeValue

maxHeight : Linear.SizeDescriptorWithNone sz -> Stylesheet.PropertyRuleAppender
maxHeight sizeDescriptor = 
  let sizeValue = sizeDescriptor Linear.sizeFactoryWithNone 
  in Stylesheet.simpleProperty "max-height" sizeValue

-------------------------------------------------------------------------------

rect : Linear.NubSizeDescriptor {} sz ->
       Linear.NubSizeDescriptor {} sz ->
       Linear.NubSizeDescriptor {} sz ->
       Linear.NubSizeDescriptor {} sz ->
       Linear.Rect a sz rec ->
       a
rect topLength rightLength bottomLength leftLength = 
  \factory -> factory.rect_ topLength rightLength bottomLength leftLength

-------------------------------------------------------------------------------

padding : Padding.PaddingDescriptor sz -> Stylesheet.PropertyRuleAppender
padding paddingDescriptor =
  Stylesheet.simpleProperty "padding" (paddingDescriptor Padding.paddingSizeFactory)

paddingTop : Linear.SizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingTop sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-top" sizeValue

paddingLeft : Linear.SizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingLeft sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-left" sizeValue

paddingRight : Linear.SizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingRight sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-right" sizeValue

paddingBottom : Linear.SizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingBottom sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-bottom" sizeValue

-------------------------------------------------------------------------------
margin : Margin.MarginDescriptor sz -> Stylesheet.PropertyRuleAppender
margin marginDescriptor =
    Stylesheet.simpleProperty "margin" (marginDescriptor Margin.marginSizeFactory)

marginTop : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
marginTop sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.autoSizableFactory
  in Stylesheet.simpleProperty "margin-top" sizeValue

marginLeft : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
marginLeft sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.autoSizableFactory
  in Stylesheet.simpleProperty "margin-left" sizeValue

marginRight : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
marginRight sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.autoSizableFactory
  in Stylesheet.simpleProperty "margin-right" sizeValue

marginBottom : Linear.AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
marginBottom sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.autoSizableFactory
  in Stylesheet.simpleProperty "margin-bottom" sizeValue

  
  
