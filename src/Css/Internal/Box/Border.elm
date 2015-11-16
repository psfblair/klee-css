module Css.Internal.Box.Border ( BorderDescriptor, borderFactory) where

import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Linear.Absolute as Absolute
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke

-------------------------------------------------------------------------------

type alias BorderDescriptor = BorderFactory -> Property.Value

type alias BorderFactory =
  { border : Stroke.NubBorderStyleDescriptor {} -> 
             Linear.SizeDescriptor {} Absolute.Abs -> 
             Color.ColorDescriptor {} -> 
             Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

borderFactory : BorderFactory
borderFactory = 
  let borderValue strokeDescriptor widthDescriptor colorDescriptor =
      let compositeDescriptor = 
            Property.spaceTripleValue strokeDescriptor widthDescriptor colorDescriptor
          factory = 
            (Stroke.nubBorderStyleStrokeFactory, Linear.nubSizeFactory, Color.nubColorFactory)
      in compositeDescriptor factory
  in { border = borderValue
     , initial_ = Common.initialValue
     , inherit_ = Common.inheritValue
     , unset_ = Common.unsetValue
     , other_ val = Common.otherValue val
     }
