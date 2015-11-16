module Css.Internal.Box.Border.Stroke
  ( NubStrokeDescriptor, nubStrokeFactory
  , StrokeDescriptor, strokeFactory
  , NubBorderStrokeDescriptor
  , NubBorderStyleDescriptor, nubBorderStyleStrokeFactory
  , BorderStyleDescriptor, borderStyleStrokeFactory
  , NubOutlineStrokeDescriptor, nubOutlineStrokeFactory
  , OutlineStrokeDescriptor, outlineStrokeFactory
  ) where
  
import Css.Internal.Property as Property
import Css.Internal.Common as Common

-------------------------------------------------------------------------------

type alias StrokeDescriptor rec = StrokeFactory rec -> Property.Value

type alias NubStrokeDescriptor rec = NubStrokeFactory rec -> Property.Value

type alias NubBorderStrokeDescriptor rec = 
  NubBorderStrokeFactory rec -> Property.Value

type alias NubBorderStyleDescriptor rec =
  NubBorderStyleStrokeFactory rec -> Property.Value

type alias BorderStyleDescriptor rec =
  BorderStyleStrokeFactory rec -> Property.Value

type alias NubOutlineStrokeDescriptor rec =
  NubOutlineStrokeFactory rec -> Property.Value

type alias OutlineStrokeDescriptor rec =
  OutlineStrokeFactory rec -> Property.Value

type alias NubStrokeFactory rec =
  { rec | stroke: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubStrokeFactory : NubStrokeFactory {}
nubStrokeFactory =
  { stroke str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias StrokeFactory rec = 
  NubStrokeFactory 
    (Common.Inherit Property.Value 
      (Common.Initial Property.Value 
        (Common.Unset Property.Value rec)))

strokeFactory : StrokeFactory {}
strokeFactory =
  let withInherit = { nubStrokeFactory | inherit_ = Common.inheritValue }
      withInitial = { withInherit      | initial_ = Common.initialValue }
      withUnset   = { withInitial      | unset_   = Common.unsetValue }
  in withUnset


type alias NubBorderStrokeFactory rec =  
  NubStrokeFactory (Common.None Property.Value rec)
  
nubBorderStrokeFactory : NubBorderStrokeFactory {}
nubBorderStrokeFactory = { nubStrokeFactory | none_ = Common.noneValue }

type alias BorderStrokeFactory rec =  
  StrokeFactory (Common.None Property.Value rec)

borderStrokeFactory : BorderStrokeFactory {}
borderStrokeFactory = { strokeFactory | none_ = Common.noneValue }

type alias NubBorderStyleStrokeFactory rec =  
  NubBorderStrokeFactory (Common.Hidden Property.Value rec)
  
nubBorderStyleStrokeFactory : NubBorderStyleStrokeFactory {}
nubBorderStyleStrokeFactory = 
  { nubBorderStrokeFactory | hidden_ = Common.hiddenValue }

type alias BorderStyleStrokeFactory rec = 
  BorderStrokeFactory (Common.Hidden Property.Value rec)

borderStyleStrokeFactory : BorderStyleStrokeFactory {}
borderStyleStrokeFactory = { borderStrokeFactory | hidden_ = Common.hiddenValue }

type alias NubOutlineStrokeFactory rec = 
  NubBorderStyleStrokeFactory (Common.Auto Property.Value rec)

nubOutlineStrokeFactory : NubOutlineStrokeFactory {}
nubOutlineStrokeFactory = { nubBorderStyleStrokeFactory | auto_ = Common.autoValue }

type alias OutlineStrokeFactory rec = 
  BorderStyleStrokeFactory (Common.Auto Property.Value rec)

outlineStrokeFactory : OutlineStrokeFactory {}
outlineStrokeFactory = { borderStyleStrokeFactory | auto_ = Common.autoValue }
