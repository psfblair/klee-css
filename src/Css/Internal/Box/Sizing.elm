module Css.Internal.Box.Sizing
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
-- For other modules that use box types in constructing more complex descriptors.
type alias NubBoxTypeFactory rec = 
  { rec | boxType: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubBoxTypeFactory : NubBoxTypeFactory {}
nubBoxTypeFactory =
  { boxType str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias WithCommonProperties rec =
  { rec | inherit_ : Property.Value
        , initial_ : Property.Value
        , unset_ : Property.Value
  }
  
type alias BoxSizingFactory = NubBoxTypeFactory (WithCommonProperties {})

boxTypeFactory : BoxSizingFactory
boxTypeFactory = 
  let withInherit = { nubBoxTypeFactory | inherit_ = Common.inheritValue }
      withInitial = { withInherit       | initial_ = Common.initialValue }
      withUnset   = { withInitial       | unset_ = Common.unsetValue }
  in withUnset
