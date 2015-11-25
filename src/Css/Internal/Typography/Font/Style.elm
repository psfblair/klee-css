module Css.Internal.Typography.Font.Style
  ( NubFontStyleDescriptor, nubFontStyleFactory
  , FontStyleDescriptor, fontStyleFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias NubFontStyleDescriptor rec = 
  NubFontStyleFactory rec -> Property.Value

type alias FontStyleDescriptor = FontStyleFactory -> Property.Value

type alias NubFontStyleFactory rec =
  { rec | style: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubFontStyleFactory : NubFontStyleFactory {}
nubFontStyleFactory =
  { style str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
type alias FontStyleFactory = 
  NubFontStyleFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value {}))))
  
fontStyleFactory : FontStyleFactory
fontStyleFactory = 
  let withCommon = Common.addCommonValues nubFontStyleFactory
  in { withCommon | normal_ = Common.normalValue }
