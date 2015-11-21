module Css.Internal.Geometry.Padding
  ( PaddingDescriptor, paddingSizeFactory) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

type alias PaddingDescriptor sz =
  Linear.SizeFactory (Linear.Rect Property.Value sz {}) sz -> Property.Value

paddingSizeFactory : Linear.SizeFactory (Linear.Rect Property.Value sz {}) sz
paddingSizeFactory = 
  let basicFactory = Linear.basicSizeFactory
      rectValue topDesc rightDesc bottomDesc leftDesc = 
        let compositeDescriptor = 
              Property.spaceQuadrupleValue topDesc rightDesc bottomDesc leftDesc
            quadrupleFactory = Utils.quadrupleOf Linear.nubSizeFactory
        in compositeDescriptor quadrupleFactory
  in  { basicFactory | rect_ = rectValue  }
