module Css.Internal.Typography.Text.Transform
  ( TextTransformDescriptor, textTransformFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias TextTransformDescriptor = TextTransformFactory -> Property.Value

type alias NubTextTransformFactory rec =
  { rec | capitalize : Property.Value
        , uppercase : Property.Value
        , lowercase : Property.Value
        , fullWidth : Property.Value
        , other_ : Property.Value -> Property.Value
  }

type alias TextTransformFactory =
  NubTextTransformFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

nubTextTransformFactory : NubTextTransformFactory {}
nubTextTransformFactory =
  { capitalize = Property.stringValue "capitalize"
  , uppercase = Property.stringValue "uppercase"
  , lowercase = Property.stringValue "lowercase"
  , fullWidth = Property.stringValue "full-width"
  , other_ val = Common.otherValue val
  }

textTransformFactory : TextTransformFactory
textTransformFactory =
  let withNone = { nubTextTransformFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone
