module Css.Internal.Box.Outline ( OutlineDescriptor, outlineFactory) where

import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke

-------------------------------------------------------------------------------

type alias OutlineDescriptor = OutlineFactory -> Property.Value

type alias NubOutlineFactory rec =
  { rec | outline : Stroke.NubOutlineStrokeDescriptor {} -> 
                    Linear.NubSizeDescriptor {} Linear.Abs -> 
                    Color.NubColorDescriptorWithInvert {} -> 
                    Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
type alias OutlineFactory =
  NubOutlineFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

nubOutlineFactory : NubOutlineFactory {}
nubOutlineFactory = 
  let outlineValue strokeDescriptor widthDescriptor colorDescriptor =
      let compositeDescriptor = 
            Property.spaceTripleValue strokeDescriptor widthDescriptor colorDescriptor
          factory = 
            ( Stroke.nubOutlineStrokeFactory
            , Linear.nubSizeFactory
            , Color.nubColorFactoryWithInvert
            )
      in compositeDescriptor factory
  in { outline = outlineValue
     , other_ val = Common.otherValue val
     }
     
outlineFactory : OutlineFactory
outlineFactory = Common.addCommonValues nubOutlineFactory
  
