module Css.Internal.Geometry.Linear.Relative
  ( Rel
  
  -- * Relative size constructors
  
  , pct
  , em, srem, ex
  , vw, vh, vmin, vmax
  ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

-- | Sizes can be relative like percentages or rems.
type Rel = Rel

-- | Size in percents.
pct : Float -> Linear.SizeDescriptor a Rel
pct length = \factory -> factory.size (Property.appendUnits length "%")

-- | Size in em's (computed value of the font-size).
em : Float -> Linear.SizeDescriptor a Rel
em length = \factory -> factory.size (Property.appendUnits length "em")

-- | Size in rem's (em's, but always relative to the root element).
-- renamed to srem in order not to collide with Basics.rem
srem : Float -> Linear.SizeDescriptor a Rel
srem length = \factory -> factory.size (Property.appendUnits length "rem")

-- Double -> Size Rel| Size in ex'es (x-height of the first avaliable font).
ex : Float -> Linear.SizeDescriptor a Rel
ex length = \factory -> factory.size (Property.appendUnits length "ex")

-- | Size in vw's (1vw = 1% of viewport width).
vw : Float -> Linear.SizeDescriptor a Rel
vw length = \factory -> factory.size (Property.appendUnits length "vw")

-- | Size in vh's (1vh = 1% of viewport height).
vh : Float -> Linear.SizeDescriptor a Rel
vh length = \factory -> factory.size (Property.appendUnits length "vh")

-- | Size in vmin's (the smaller of vw or vh).
vmin : Float -> Linear.SizeDescriptor a Rel
vmin length = \factory -> factory.size (Property.appendUnits length "vmin")

-- | Size in vmax's (the larger of vw or vh).
vmax : Float -> Linear.SizeDescriptor a Rel
vmax length = \factory -> factory.size (Property.appendUnits length "vmax")
