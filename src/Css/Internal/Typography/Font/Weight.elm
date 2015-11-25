module Css.Internal.Typography.Font.Weight
  ( NubFontWeightDescriptor, nubFontWeightFactory
  , FontWeightDescriptor, fontWeightFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias NubFontWeightDescriptor rec = 
  NubFontWeightFactory rec -> Property.Value

type alias NubFontWeightFactory rec =
  { rec | weight: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
nubFontWeightFactory : NubFontWeightFactory {}
nubFontWeightFactory =
  { weight str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias FontWeightDescriptor = FontWeightFactory -> Property.Value

type alias FontWeightFactory =
  NubFontWeightFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value {}))))
  
fontWeightFactory : FontWeightFactory
fontWeightFactory = 
  let withCommon = Common.addCommonValues nubFontWeightFactory
  in { withCommon | normal_ = Common.normalValue }
