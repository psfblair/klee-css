module Css.Internal.Typography.Font.Variant
  ( NubFontVariantDescriptor, nubFontVariantFactory
  , FontVariantDescriptor, fontVariantFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------
type alias NubFontVariantDescriptor rec = 
  NubFontVariantFactory rec -> Property.Value

type alias FontVariantDescriptor = FontVariantFactory -> Property.Value

type alias NubFontVariantFactory rec =
  { rec | variant: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubFontVariantFactory : NubFontVariantFactory {}
nubFontVariantFactory =
  { variant str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
type alias FontVariantFactory = 
  NubFontVariantFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value 
            (Common.None Property.Value {})))))
  
fontVariantFactory : FontVariantFactory
fontVariantFactory = 
  let withCommon = Common.addCommonValues nubFontVariantFactory
      withNormal = { withCommon | normal_ = Common.normalValue }
      withNone   = { withNormal | none_   = Common.noneValue   }
  in withNone
