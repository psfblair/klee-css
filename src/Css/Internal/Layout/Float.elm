module Css.Internal.Layout.Float
  ( FloatStyleDescriptor, floatStyleFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias FloatStyleDescriptor = FloatStyleFactory -> Property.Value

type alias NubFloatStyleFactory rec =
  { rec | floatStyle : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubFloatStyleFactory : NubFloatStyleFactory {}
nubFloatStyleFactory =
  { floatStyle str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias FloatStyleFactory =
  NubFloatStyleFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

floatStyleFactory : FloatStyleFactory
floatStyleFactory =
  let withNone = { nubFloatStyleFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone
