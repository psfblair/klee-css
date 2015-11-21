module Css.Internal.Display
  ( FloatStyleDescriptor, floatStyleFactory
  , ClearDescriptor, clearFactory
  , PositionDescriptor, positionFactory
  , DisplayDescriptor, displayFactory
  , OverflowDescriptor, overflowFactory
  , VisibilityDescriptor, visibilityFactory
  , ExtendedVisibilityDescriptor, extendedVisibilityFactory
  , ClipDescriptor, clipFactory
  , OpacityDescriptor, opacityFactory
  , ZIndexDescriptor, zIndexFactory
  , PointerEventsDescriptor, pointerEventsFactory
  , VerticalAlignDescriptor, verticalAlignFactory
  , CursorDescriptor, cursorFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias FloatStyleDescriptor = FloatStyleFactory -> Property.Value

type alias NubFloatStyleFactory rec =
  { rec | floatStyle : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubFloatStyleFactory : NubFloatStyleFactory {}
nubFloatStyleFactory =
  { floatStyle str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias FloatStyleFactory =
  NubFloatStyleFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

floatStyleFactory : FloatStyleFactory
floatStyleFactory =
  let withNone = { nubFloatStyleFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone

-------------------------------------------------------------------------------

type alias ClearDescriptor = ClearFactory -> Property.Value

type alias NubClearFactory rec =
  { rec | clear: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubClearFactory : NubClearFactory {}
nubClearFactory =
  { clear str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias ClearFactory =
  NubClearFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

clearFactory : ClearFactory
clearFactory =
  let withNone = { nubClearFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone

-------------------------------------------------------------------------------

type alias PositionDescriptor = PositionFactory -> Property.Value

type alias NubPositionFactory rec =
  { rec | position : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubPositionFactory : NubPositionFactory {}
nubPositionFactory =
  { position str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias PositionFactory =
  NubPositionFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

positionFactory : PositionFactory
positionFactory = Common.addCommonValues nubPositionFactory

-------------------------------------------------------------------------------

type alias DisplayDescriptor = DisplayFactory -> Property.Value

type alias NubDisplayFactory rec =
  { rec | display : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
nubDisplayFactory : NubDisplayFactory {}
nubDisplayFactory =
  { display str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias DisplayFactory =
  NubDisplayFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

displayFactory : DisplayFactory
displayFactory =
  let withNone = { nubDisplayFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone

-------------------------------------------------------------------------------

type alias OverflowDescriptor = OverflowFactory -> Property.Value

type alias NubOverflowFactory rec =
  { rec | overflow : String -> Property.Value
  , other_ : Property.Value -> Property.Value
  }

nubOverflowFactory : NubOverflowFactory {}
nubOverflowFactory =
  { overflow str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias OverflowFactory =
  NubOverflowFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value 
            (Common.Visible Property.Value 
              (Common.Hidden Property.Value {}))))))
  
overflowFactory : OverflowFactory
overflowFactory =
  let withAuto    = { nubOverflowFactory | auto_ = Common.autoValue       }
      withVisible = { withAuto           | visible_ = Common.visibleValue }
      withHidden  = { withVisible        | hidden_ = Common.hiddenValue   }
  in Common.addCommonValues withHidden

-------------------------------------------------------------------------------

type alias VisibilityDescriptor rec =
  VisibilityFactory rec -> Property.Value

type alias NubVisibilityFactory rec =
  { rec | visibility : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
nubVisibilityFactory : NubVisibilityFactory {}
nubVisibilityFactory =
  { visibility str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias VisibilityFactory rec =
  NubVisibilityFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value rec)))

visibilityFactory : VisibilityFactory {}
visibilityFactory = Common.addCommonValues nubVisibilityFactory

type alias ExtendedVisibilityDescriptor rec = 
  ExtendedVisibilityFactory rec -> Property.Value

type alias WithExtendedVisibility rec = 
  { rec | visible_ : Property.Value
        , hidden_ : Property.Value
  }

type alias ExtendedVisibilityFactory rec = 
  VisibilityFactory (WithExtendedVisibility rec)

extendedVisibilityFactory : ExtendedVisibilityFactory {}
extendedVisibilityFactory =
  let withVisible = { visibilityFactory | visible_ = Common.visibleValue }
      withHidden  = { withVisible       | hidden_  = Common.hiddenValue  }
  in withHidden

-------------------------------------------------------------------------------
--TODO Remove; use clip-path instead
type alias ClipDescriptor a b c d = ClipFactory a b c d -> Property.Value

type alias NubClipFactory a b c d rec =
  { rec | rect_ : Linear.NubSizeDescriptor {} a -> 
                  Linear.NubSizeDescriptor {} b -> 
                  Linear.NubSizeDescriptor {} c -> 
                  Linear.NubSizeDescriptor {} d -> 
                  Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubClipFactory : NubClipFactory a b c d {}
nubClipFactory =
  { rect_ top right bottom left = 
      let quadrupleDescriptor = Property.commaQuadrupleValue top right bottom left
          factory = 
            (Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory)
          quadrupleValue = quadrupleDescriptor factory
          prefixValue = Property.stringValue "rect("
          suffixValue = Property.stringValue ")"
      in Property.concatenateValues [ prefixValue, quadrupleValue, suffixValue ]    
  , other_ val = Common.otherValue val
  }

type alias ClipFactory a b c d =
  NubClipFactory a b c d
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))

clipFactory : ClipFactory a b c d
clipFactory =
  let withAuto = { nubClipFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto

-------------------------------------------------------------------------------

type alias OpacityDescriptor = OpacityFactory -> Property.Value

type alias NubOpacityFactory rec =
  { rec | opacity : Float -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubOpacityFactory : NubOpacityFactory {}
nubOpacityFactory =
  { opacity num = Property.floatValue num
  , other_ val = Common.otherValue val
  }

type alias OpacityFactory =
  NubOpacityFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

opacityFactory : OpacityFactory
opacityFactory = Common.addCommonValues nubOpacityFactory

-------------------------------------------------------------------------------

type alias ZIndexDescriptor = ZIndexFactory -> Property.Value

type alias NubZIndexFactory rec =
  { rec | zIndex : Int -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubZIndexFactory : NubZIndexFactory {}
nubZIndexFactory =
  { zIndex num = Property.intValue num
  , other_ val = Common.otherValue val
  }

type alias ZIndexFactory =
  NubZIndexFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))

zIndexFactory : ZIndexFactory
zIndexFactory =
  let withAuto = { nubZIndexFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto

-------------------------------------------------------------------------------

type alias PointerEventsDescriptor = PointerEventsFactory -> Property.Value

type alias NubPointerEventsFactory rec =
  { rec | pointerEvents : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubPointerEventsFactory : NubPointerEventsFactory {}
nubPointerEventsFactory =
  { pointerEvents str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
type alias PointerEventsFactory =
  NubPointerEventsFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))
  
pointerEventsFactory : PointerEventsFactory
pointerEventsFactory =
  let withAuto = { nubPointerEventsFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto

-------------------------------------------------------------------------------

type alias VerticalAlignDescriptor sz = VerticalAlignFactory sz -> Property.Value

-- Since NubSizeDescriptor is parameterized by a generic type `a` rather than
-- simply by `Size a`, that means that for dimensioned sizes it just calls
-- whatever `size` function is passed to it in the record -- that function
-- doesn't need to return a `Size`. So we can pass this factory to a 
-- NubSizeDescriptor and get a `VerticalAlignValue` out instead of a `Size`.
type alias WithVerticalAlign =
  { vAlign : String -> Property.Value
  , baseline_ : Property.Value
  }

type alias VerticalAlignFactory sz = Linear.SizeFactory WithVerticalAlign sz

verticalAlignFactory : VerticalAlignFactory {}
verticalAlignFactory =
  let sizeFactory  = Linear.basicSizeFactory
      withBaseline = { sizeFactory  | baseline_ = Common.baselineValue }
      withVAlign   = { withBaseline | vAlign = \str -> Property.stringValue str }
  in withVAlign

-------------------------------------------------------------------------------

type alias CursorDescriptor = CursorFactory -> Property.Value

type alias NubCursorFactory rec =
  { rec | cursor : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

type alias CursorFactory =
  NubCursorFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value 
            (Common.None Property.Value {})))))
  
nubCursorFactory : NubCursorFactory {}
nubCursorFactory =
  { cursor str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
cursorFactory : CursorFactory
cursorFactory =
  let withAuto = { nubCursorFactory | auto_ = Common.autoValue }
      withNone = { withAuto         | none_ = Common.noneValue }
  in Common.addCommonValues withNone
