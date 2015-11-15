module Css.Internal.Geometry.Margin
  (
  -- * Margin.
    
    MarginDescriptor
  , margin
  , marginTop, marginLeft, marginRight, marginBottom

  ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

type alias MarginDescriptor sz =
  Linear.AutoSizableFactory (Linear.Rect Property.Value sz {}) sz -> Property.Value

margin : MarginDescriptor sz -> Stylesheet.PropertyRuleAppender
margin marginDescriptor =
    Stylesheet.simpleProperty "margin" (marginDescriptor marginSizeFactory)

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
