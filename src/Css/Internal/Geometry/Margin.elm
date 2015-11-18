module Css.Internal.Geometry.Margin
  ( MarginDescriptor, marginSizeFactory ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

type alias MarginDescriptor sz =
  Linear.AutoSizableFactory (Linear.Rect Property.Value sz {}) sz -> Property.Value

-------------------------------------------------------------------------------

marginSizeFactory : Linear.AutoSizableFactory (Linear.Rect Property.Value sz {}) sz
marginSizeFactory =
  let basicFactory = Linear.autoSizableFactory
      rectValue topDesc rightDesc bottomDesc leftDesc = 
        let compositeDescriptor = 
              Property.spaceQuadrupleValue topDesc rightDesc bottomDesc leftDesc
            quadrupleFactory = Utils.quadrupleOf Linear.nubSizeFactory
        in compositeDescriptor quadrupleFactory
  in { basicFactory | rect_ = rectValue  }
