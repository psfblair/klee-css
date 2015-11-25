module Css.Internal.Typography.Text.Rendering
  ( TextRenderingDescriptor
  , textRenderingFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias TextRenderingDescriptor = TextRenderingFactory -> Property.Value
  
type alias NubTextRenderingFactory rec =
  { rec | speedOptimize : Property.Value
        , legibilityOptimize : Property.Value
        , preciseGeometry : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubTextRenderingFactory : NubTextRenderingFactory {}
nubTextRenderingFactory =
  { speedOptimize = Property.stringValue "optimizeSpeed"
  , legibilityOptimize = Property.stringValue "optimizeLegibility"
  , preciseGeometry = Property.stringValue "geometricPrecision"
  , other_ val = Common.otherValue val
  }

type alias TextRenderingFactory =
  NubTextRenderingFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))
  
textRenderingFactory : TextRenderingFactory
textRenderingFactory =
  let withAuto = { nubTextRenderingFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto
