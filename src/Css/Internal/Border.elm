module Css.Internal.Border 
  ( StrokeDescriptor, strokeFactory
  , BorderWidthDescriptor, nubBorderWidthFactory
  , BasicBorderWidthDescriptor, borderWidthFactory
  ) where

import Css.Internal.Property exposing (Value, stringValue)

import Css.Internal.Common exposing 
  ( otherValue, initialValue, inheritValue, autoValue, noneValue )

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Linear.Absolute as Absolute

-------------------------------------------------------------------------------

type alias StrokeDescriptor = StrokeFactory -> Value

type alias StrokeFactory =
  {
    stroke: String -> Value
  , none_ : Value
  , inherit_ : Value
  , auto_ : Value
  , other_ : Value -> Value
  }

strokeFactory : StrokeFactory
strokeFactory =
  {
    stroke str = stringValue str
  , none_ = noneValue
  , inherit_ = inheritValue
  , auto_ = autoValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias BorderWidthDescriptor rec = BorderWidthFactory rec -> Value

type alias BasicBorderWidthDescriptor rec = BasicBorderWidthFactory rec -> Value

type alias WithBorderWidths rec = 
  { rec | medium : Value
        , thin : Value
        , thick : Value
  }  

type alias BorderWidthFactory rec =
  Linear.SizeFactory (WithBorderWidths rec) Absolute.Abs

nubBorderWidthFactory : BorderWidthFactory {}
nubBorderWidthFactory = 
  let sizeFactory = Linear.nubSizeFactory
      withMedium = { sizeFactory | medium = stringValue "medium" }
      withThin   = { withMedium  | thin   = stringValue "thin"   }
      withThick  = { withThin    | thick  = stringValue "thick"  }
  in withThick

type alias BasicBorderWidthFactory rec =
  Linear.BasicSizeFactory (WithBorderWidths rec) Absolute.Abs

borderWidthFactory : BasicBorderWidthFactory {}
borderWidthFactory = 
  let sizeFactory = Linear.basicSizeFactory
      withMedium = { sizeFactory | medium = stringValue "medium" }
      withThin   = { withMedium  | thin   = stringValue "thin"   }
      withThick  = { withThin    | thick  = stringValue "thick"  }
  in withThick
