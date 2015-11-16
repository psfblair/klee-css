module Css.Internal.Geometry.Padding
  ( PaddingDescriptor, paddingSizeFactory) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias PaddingDescriptor sz =
  Linear.BasicSizeFactory (Linear.Rect Property.Value sz {}) sz -> Property.Value

paddingSizeFactory : Linear.BasicSizeFactory (Linear.Rect Property.Value sz {}) sz
paddingSizeFactory = 
  let basicFactory = Linear.basicSizeFactory
      rectValue topDescriptor rightDescriptor bottomDescriptor leftDescriptor = 
        let topValue     = topDescriptor    Linear.nubSizeFactory
            rightValue   = rightDescriptor  Linear.nubSizeFactory
            bottomValue  = bottomDescriptor Linear.nubSizeFactory
            leftValue    = leftDescriptor   Linear.nubSizeFactory
            valueFactory =
              Property.spaceQuadrupleValue identity identity identity identity
        in valueFactory (topValue, rightValue, bottomValue, leftValue)
  in  { basicFactory | rect_ = rectValue  }
