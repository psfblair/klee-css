module Css.Internal.Geometry.Linear.Relative
  ( Rel
  -- * Relative size constructors
  
  , rel0
  , pct
  , em, srem, ex
  , vw, vh, vmin, vmax
  ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

-- | Sizes can be relative like percentages or rems.
type Rel = Rel

relative : Property.Value -> Linear.Size Rel
relative lengthValue = Linear.toSize Rel lengthValue

-- | Zero relative size.
rel0 : Linear.SizeDescriptor a Rel
rel0 = 
  let zeroSize = Property.stringValue "0"
  in \factory -> factory.size (relative zeroSize)

-- | Size in percents.
pct : Float -> Linear.SizeDescriptor a Rel
pct length = 
  let lengthValue = Property.appendUnits length "%"
  in \factory -> factory.size (relative lengthValue)

-- | Size in em's (computed value of the font-size).
em : Float -> Linear.SizeDescriptor a Rel
em length =  
  let lengthValue = Property.appendUnits length "em"
  in \factory -> factory.size (relative lengthValue)

-- | Size in rem's (em's, but always relative to the root element).
-- renamed to srem in order not to collide with Basics.rem
srem : Float -> Linear.SizeDescriptor a Rel
srem length =  
  let lengthValue = Property.appendUnits length "rem"
  in \factory -> factory.size (relative lengthValue)

-- Double -> Size Rel| Size in ex'es (x-height of the first avaliable font).
ex : Float -> Linear.SizeDescriptor a Rel
ex length =  
  let lengthValue = Property.appendUnits length "ex"
  in \factory -> factory.size (relative lengthValue)

-- | Size in vw's (1vw = 1% of viewport width).
vw : Float -> Linear.SizeDescriptor a Rel
vw length =  
  let lengthValue = Property.appendUnits length "vw"
  in \factory -> factory.size (relative lengthValue)

-- | Size in vh's (1vh = 1% of viewport height).
vh : Float -> Linear.SizeDescriptor a Rel
vh length =  
  let lengthValue = Property.appendUnits length "vh"
  in \factory -> factory.size (relative lengthValue)

-- | Size in vmin's (the smaller of vw or vh).
vmin : Float -> Linear.SizeDescriptor a Rel
vmin length =  
  let lengthValue = Property.appendUnits length "vmin"
  in \factory -> factory.size (relative lengthValue)

-- | Size in vmax's (the larger of vw or vh).
vmax : Float -> Linear.SizeDescriptor a Rel
vmax length =  
  let lengthValue = Property.appendUnits length "vmax"
  in \factory -> factory.size (relative lengthValue)
