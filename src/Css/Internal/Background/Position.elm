module Css.Internal.Background.Position
  ( BackgroundPositionDescriptor, backgroundPositionFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides

-------------------------------------------------------------------------------

type alias BackgroundPositionDescriptor szH szV = 
    BackgroundPositionFactory szH szV -> Property.Value

type alias NubBackgroundPositionFactory szH szV rec =
  { rec | sizedPosition : Linear.NubSizeDescriptor {} szH -> 
                          Linear.NubSizeDescriptor {} szV -> 
                          Property.Value
        , sidedPosition : Sides.HorizontalSide -> 
                          Sides.VerticalSide -> 
                          Property.Value
        , other_ : Property.Value -> Property.Value
  }

type alias BackgroundPositionFactory szH szV =
  NubBackgroundPositionFactory szH szV
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

nubBackgroundPositionFactory : NubBackgroundPositionFactory szH szV {}
nubBackgroundPositionFactory = 
  { sizedPosition horizontalDescriptor verticalDescriptor = 
      let compositeDescriptor = 
        Property.spacePairValue horizontalDescriptor verticalDescriptor
      in compositeDescriptor (Linear.nubSizeFactory, Linear.nubSizeFactory)
  , sidedPosition horizontal vertical = 
      let valueFactory = 
        Property.spacePairValue Sides.horizontalSideValue Sides.verticalSideValue
      in valueFactory (horizontal, vertical)
  , other_ val = Common.otherValue val
  }

backgroundPositionFactory : BackgroundPositionFactory szH szV
backgroundPositionFactory = Common.addCommonValues nubBackgroundPositionFactory
