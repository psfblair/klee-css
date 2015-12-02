module Css.Internal.Layout.ZIndex
  ( ZIndexDescriptor, zIndexFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias ZIndexDescriptor = ZIndexFactory -> Property.Value

type alias NubZIndexFactory rec =
  { rec | zIndex : Int -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubZIndexFactory : NubZIndexFactory {}
nubZIndexFactory =
  { zIndex num = Property.intValue num
  , other_ val = Common.otherValue val
  }

type alias ZIndexFactory =
  NubZIndexFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))

zIndexFactory : ZIndexFactory
zIndexFactory =
  let withAuto = { nubZIndexFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto
