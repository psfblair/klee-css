module Css.Internal.Background.Size
  ( BackgroundSizeDescriptor, backgroundSizeFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 
import Css.Internal.Geometry.Linear as Linear

-------------------------------------------------------------------------------

type alias BackgroundSizeDescriptor sz = 
  BackgroundSizeFactory sz -> Property.Value

type alias NubBackgroundSizeFactory sz rec =
  { rec | backgroundSize : Linear.NubSizeDescriptor {} sz -> 
                     Linear.NubSizeDescriptor {} sz -> 
                     Property.Value
        , partial : Linear.NubSizeDescriptor {} sz -> Property.Value
        , named : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

type alias BackgroundSizeFactory sz =
  NubBackgroundSizeFactory sz
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))

nubBackgroundSizeFactory : NubBackgroundSizeFactory sz {}
nubBackgroundSizeFactory =
  { backgroundSize widthDescriptor heightDescriptor = 
      let compositeDescriptor = 
        Property.spacePairValue widthDescriptor heightDescriptor
      in compositeDescriptor (Linear.nubSizeFactory, Linear.nubSizeFactory)
  , partial widthDescriptor = 
      let compositeDescriptor = 
        Property.spacePairValue widthDescriptor identity
      in compositeDescriptor (Linear.nubSizeFactory, Common.autoValue)
  , named str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
          
backgroundSizeFactory : BackgroundSizeFactory sz
backgroundSizeFactory =
  let withAuto = { nubBackgroundSizeFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto
