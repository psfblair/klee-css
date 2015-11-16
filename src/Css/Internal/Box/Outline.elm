module Css.Internal.Box.Outline ( OutlineDescriptor, outlineFactory) where

import Css.Internal.Box.Border.Stroke as Stroke
import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Linear.Absolute as Absolute
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias OutlineDescriptor = OutlineFactory -> Property.Value

type alias OutlineFactory =
  { outline : Stroke.NubOutlineStrokeDescriptor {} -> 
              Linear.SizeDescriptor {} Absolute.Abs -> 
              Color.NubColorDescriptorWithInvert {} -> 
              Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

outlineFactory : OutlineFactory
outlineFactory = 
  let outlineValue strokeDescriptor widthDescriptor colorDescriptor =
      let compositeDescriptor = 
            Property.spaceTripleValue strokeDescriptor widthDescriptor colorDescriptor
          factory = 
            (Stroke.nubOutlineStrokeFactory, Linear.nubSizeFactory, Color.nubColorFactoryWithInvert)
      in compositeDescriptor factory
  in { outline = outlineValue
     , initial_ = Common.initialValue
     , inherit_ = Common.inheritValue
     , unset_ = Common.unsetValue
     , other_ val = Common.otherValue val
     }
