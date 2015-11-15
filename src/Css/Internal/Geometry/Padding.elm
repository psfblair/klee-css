module Css.Internal.Geometry.Padding
  (
  -- * Padding.
    
    PaddingDescriptor
  , padding
  , paddingTop, paddingLeft, paddingRight, paddingBottom

  ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

type alias PaddingDescriptor sz =
  Linear.BasicSizeFactory (Linear.Rect Property.Value sz {}) sz -> Property.Value

padding : PaddingDescriptor sz -> Stylesheet.PropertyRuleAppender
padding paddingDescriptor =
  Stylesheet.simpleProperty "padding" (paddingDescriptor paddingSizeFactory)

paddingTop : Linear.BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingTop sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-top" sizeValue

paddingLeft : Linear.BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingLeft sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-left" sizeValue

paddingRight : Linear.BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingRight sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-right" sizeValue

paddingBottom : Linear.BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
paddingBottom sizeDescriptor =
  let sizeValue = sizeDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "padding-bottom" sizeValue

-------------------------------------------------------------------------------

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
