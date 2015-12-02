module Css.Internal.Layout.Opacity
  ( OpacityDescriptor, opacityFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias OpacityDescriptor = OpacityFactory -> Property.Value

type alias NubOpacityFactory rec =
  { rec | opacity : Float -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubOpacityFactory : NubOpacityFactory {}
nubOpacityFactory =
  { opacity num = Property.floatValue num
  , other_ val = Common.otherValue val
  }

type alias OpacityFactory =
  NubOpacityFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

opacityFactory : OpacityFactory
opacityFactory = Common.addCommonValues nubOpacityFactory
