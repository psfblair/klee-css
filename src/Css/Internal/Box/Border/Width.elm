module Css.Internal.Box.Border.Width
  ( BorderWidthDescriptor, nubBorderWidthFactory
  , BasicBorderWidthDescriptor, borderWidthFactory
  ) where
  
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Linear.Absolute as Absolute
import Css.Internal.Property as Property

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
  Linear.SizeFactory (WithBorderWidths rec) Absolute.Abs

nubBorderWidthFactory : BorderWidthFactory {}
nubBorderWidthFactory = 
  let sizeFactory = Linear.nubSizeFactory
      withMedium = { sizeFactory | medium = Property.stringValue "medium" }
      withThin   = { withMedium  | thin   = Property.stringValue "thin"   }
      withThick  = { withThin    | thick  = Property.stringValue "thick"  }
  in withThick

type alias BasicBorderWidthFactory rec =
  Linear.BasicSizeFactory (WithBorderWidths rec) Absolute.Abs

borderWidthFactory : BasicBorderWidthFactory {}
borderWidthFactory = 
  let sizeFactory = Linear.basicSizeFactory
      withMedium = { sizeFactory | medium = Property.stringValue "medium" }
      withThin   = { withMedium  | thin   = Property.stringValue "thin"   }
      withThick  = { withThin    | thick  = Property.stringValue "thick"  }
  in withThick
