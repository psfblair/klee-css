module Css.Internal.Box.Border 
  ( BorderDescriptor, borderFactory
  , NubBorderWidthDescriptor, nubBorderWidthFactory
  , BorderWidthDescriptor, borderWidthFactory
  ) where

import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke

-------------------------------------------------------------------------------

type alias BorderDescriptor = BorderFactory -> Property.Value

type alias NubBorderFactory rec = 
  { rec | border : Stroke.NubBorderStyleDescriptor {} -> 
                   Linear.NubSizeDescriptor {} Linear.Abs -> 
                   Color.NubColorDescriptor {} -> 
                   Property.Value
        , other_ : Property.Value -> Property.Value
  }

type alias BorderFactory = 
  NubBorderFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

nubBorderFactory : NubBorderFactory {}
nubBorderFactory = 
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
     , other_ val = Common.otherValue val
     }
  
borderFactory : BorderFactory
borderFactory = Common.addCommonValues nubBorderFactory

-------------------------------------------------------------------------------

type alias NubBorderWidthDescriptor rec = 
  NubBorderWidthFactory rec -> Property.Value

type alias BorderWidthDescriptor rec = 
  BorderWidthFactory rec -> Property.Value

type alias WithBorderWidths rec = 
  { rec | medium : Property.Value
        , thin : Property.Value
        , thick : Property.Value
  }  

type alias NubBorderWidthFactory rec =
  Linear.NubSizeFactory (WithBorderWidths rec) Linear.Abs

nubBorderWidthFactory : NubBorderWidthFactory {}
nubBorderWidthFactory = 
  let sizeFactory = Linear.nubSizeFactory
      withMedium = { sizeFactory | medium = Property.stringValue "medium" }
      withThin   = { withMedium  | thin   = Property.stringValue "thin"   }
      withThick  = { withThin    | thick  = Property.stringValue "thick"  }
  in withThick

type alias BorderWidthFactory rec =
  Linear.SizeFactory (WithBorderWidths rec) Linear.Abs

borderWidthFactory : BorderWidthFactory {}
borderWidthFactory = 
  let sizeFactory = Linear.basicSizeFactory
      withMedium = { sizeFactory | medium = Property.stringValue "medium" }
      withThin   = { withMedium  | thin   = Property.stringValue "thin"   }
      withThick  = { withThin    | thick  = Property.stringValue "thick"  }
  in withThick
