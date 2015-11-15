module Css.Internal.Box.Sizing
  ( BoxSizingDescriptor, BoxTypeDescriptor, BareBoxTypeDescriptor
  , boxSizing, paddingBox, borderBox, contentBox
  , boxSizeValue, boxTypeValue
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------
type alias BoxSizingDescriptor = BoxSizingFactory -> Property.Value

type alias BoxTypeDescriptor rec = BareBoxTypeFactory rec -> Property.Value

type alias BareBoxTypeDescriptor = BareBoxTypeFactory {} -> Property.Value

boxSizing : BoxSizingDescriptor -> Stylesheet.PropertyRuleAppender
boxSizing descriptor =
  let boxType = descriptor boxTypeFactory
  in Stylesheet.simpleProperty "box-sizing" boxType

-- These functions must be usable in cases where we don't want to accept generic
-- properties; i.e., where we are constructing more complex descriptors. So we
-- don't require that the common properties be present.
paddingBox : BoxTypeDescriptor rec
paddingBox factory = factory.boxType "padding-box"

borderBox : BoxTypeDescriptor rec
borderBox factory = factory.boxType "border-box"

contentBox : BoxTypeDescriptor rec
contentBox factory = factory.boxType "content-box"

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
