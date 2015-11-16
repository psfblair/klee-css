module Css.Internal.Box.Sizing
  ( BoxSizingDescriptor, BoxTypeDescriptor, BareBoxTypeDescriptor
  , boxSizeValue, boxTypeFactory, boxTypeValue
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------
type alias BoxSizingDescriptor = BoxSizingFactory -> Property.Value

type alias BoxTypeDescriptor rec = BareBoxTypeFactory rec -> Property.Value

type alias BareBoxTypeDescriptor = BareBoxTypeFactory {} -> Property.Value

-- Exported for other modules that use box types (e.g., Background).
boxSizeValue : BoxSizingDescriptor -> Property.Value
boxSizeValue descriptor = descriptor boxTypeFactory

-- For other modules that use box types in constructing more complex descriptors.
boxTypeValue : BareBoxTypeDescriptor -> Property.Value
boxTypeValue descriptor = descriptor bareBoxTypeFactory

-------------------------------------------------------------------------------
-- For other modules that use box types in constructing more complex descriptors.
type alias BareBoxTypeFactory rec = 
  { rec | boxType: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

bareBoxTypeFactory : BareBoxTypeFactory {}
bareBoxTypeFactory =
  { boxType str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias WithCommonProperties rec =
  { rec | inherit_ : Property.Value
        , initial_ : Property.Value
        , unset_ : Property.Value
  }
  
type alias BoxSizingFactory = BareBoxTypeFactory (WithCommonProperties {})

boxTypeFactory : BoxSizingFactory
boxTypeFactory = 
  let withInherit = { bareBoxTypeFactory | inherit_ = Common.inheritValue }
      withInitial = { withInherit | initial_ = Common.initialValue }
      withUnset   = { withInitial | unset_ = Common.unsetValue }
  in withUnset
