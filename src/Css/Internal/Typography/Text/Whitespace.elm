module Css.Internal.Typography.Text.Whitespace
  ( WhiteSpaceDescriptor, whiteSpaceFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias WhiteSpaceDescriptor = WhiteSpaceFactory -> Property.Value
  
type alias NubWhiteSpaceFactory rec =
  { rec | noWrap : Property.Value
        , pre : Property.Value
        , preWrap : Property.Value
        , preLine : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubWhiteSpaceFactory : NubWhiteSpaceFactory {}
nubWhiteSpaceFactory =
  { noWrap = Property.stringValue "nowrap"
  , pre = Property.stringValue "pre"
  , preWrap = Property.stringValue "pre-wrap"
  , preLine = Property.stringValue "pre-line"
  , other_ val = Common.otherValue val
  }

type alias WhiteSpaceFactory =
  NubWhiteSpaceFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value {}))))

whiteSpaceFactory : WhiteSpaceFactory
whiteSpaceFactory =
  let withNormal = { nubWhiteSpaceFactory | normal_ = Common.normalValue }
  in Common.addCommonValues withNormal
