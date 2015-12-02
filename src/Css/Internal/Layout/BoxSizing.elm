module Css.Internal.Layout.BoxSizing
  ( BoxSizingDescriptor, BoxTypeDescriptor, NubBoxTypeDescriptor
  , nubBoxTypeFactory, boxTypeFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------
type alias BoxSizingDescriptor = BoxSizingFactory -> Property.Value

type alias BoxTypeDescriptor rec = NubBoxTypeFactory rec -> Property.Value

type alias NubBoxTypeDescriptor = NubBoxTypeFactory {} -> Property.Value

-------------------------------------------------------------------------------

type alias NubBoxTypeFactory rec = 
  { rec | boxType: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubBoxTypeFactory : NubBoxTypeFactory {}
nubBoxTypeFactory =
  { boxType str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias BoxSizingFactory =
   NubBoxTypeFactory
     (Common.Initial Property.Value
       (Common.Inherit Property.Value
         (Common.Unset Property.Value {})))

boxTypeFactory : BoxSizingFactory
boxTypeFactory = Common.addCommonValues nubBoxTypeFactory
