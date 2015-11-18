module Css.Internal.Box.Border 
  ( BorderDescriptor, borderFactory
  , BorderWidthDescriptor, nubBorderWidthFactory
  , BasicBorderWidthDescriptor, borderWidthFactory
  ) where

import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke

-------------------------------------------------------------------------------

type alias BorderDescriptor = BorderFactory -> Property.Value

type alias BorderFactory =
  { border : Stroke.NubBorderStyleDescriptor {} -> 
             Linear.SizeDescriptor {} Linear.Abs -> 
             Color.NubColorDescriptor {} -> 
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
            ( Stroke.nubBorderStyleStrokeFactory
            , Linear.nubSizeFactory
            , Color.nubColorFactory
            )
      in compositeDescriptor factory
  in { border = borderValue
     , initial_ = Common.initialValue
     , inherit_ = Common.inheritValue
     , unset_ = Common.unsetValue
     , other_ val = Common.otherValue val
     }

-------------------------------------------------------------------------------

type alias BorderWidthDescriptor rec = BorderWidthFactory rec -> Property.Value

type alias BasicBorderWidthDescriptor rec = 
  BasicBorderWidthFactory rec -> Property.Value

type alias WithBorderWidths rec = 
  { rec | medium : Property.Value
        , thin : Property.Value
        , thick : Property.Value
  }  

type alias BorderWidthFactory rec =
  Linear.SizeFactory (WithBorderWidths rec) Linear.Abs

nubBorderWidthFactory : BorderWidthFactory {}
nubBorderWidthFactory = 
  let sizeFactory = Linear.nubSizeFactory
      withMedium = { sizeFactory | medium = Property.stringValue "medium" }
      withThin   = { withMedium  | thin   = Property.stringValue "thin"   }
      withThick  = { withThin    | thick  = Property.stringValue "thick"  }
  in withThick

type alias BasicBorderWidthFactory rec =
  Linear.BasicSizeFactory (WithBorderWidths rec) Linear.Abs

borderWidthFactory : BasicBorderWidthFactory {}
borderWidthFactory = 
  let sizeFactory = Linear.basicSizeFactory
      withMedium = { sizeFactory | medium = Property.stringValue "medium" }
      withThin   = { withMedium  | thin   = Property.stringValue "thin"   }
      withThick  = { withThin    | thick  = Property.stringValue "thick"  }
  in withThick
