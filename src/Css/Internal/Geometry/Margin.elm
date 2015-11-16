module Css.Internal.Geometry.Margin
  ( MarginDescriptor, marginSizeFactory ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias MarginDescriptor sz =
  Linear.AutoSizableFactory (Linear.Rect Property.Value sz {}) sz -> Property.Value

-------------------------------------------------------------------------------

marginSizeFactory : Linear.AutoSizableFactory (Linear.Rect Property.Value sz {}) sz
marginSizeFactory =
  let basicFactory = Linear.autoSizableFactory
      rectValue topDescriptor rightDescriptor bottomDescriptor leftDescriptor = 
        let topValue     = topDescriptor     Linear.nubSizeFactory
            rightValue   = rightDescriptor   Linear.nubSizeFactory
            bottomValue  = bottomDescriptor  Linear.nubSizeFactory
            leftValue    = leftDescriptor    Linear.nubSizeFactory
            valueFactory =
              Property.spaceQuadrupleValue identity identity identity identity
        in valueFactory (topValue, rightValue, bottomValue, leftValue)
  in { basicFactory | rect_ = rectValue  }
