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

type alias FloatStyleFactory =
  {
    floatStyle : String -> Property.Value
  , none_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

floatStyleFactory : FloatStyleFactory
floatStyleFactory =
  {
    floatStyle str = Property.stringValue str
  , none_ = Common.noneValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias ClearDescriptor = ClearFactory -> Property.Value

type alias ClearFactory =
  {
    clear: String -> Property.Value
  , none_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

clearFactory : ClearFactory
clearFactory =
  {
    clear str = Property.stringValue str
  , none_ = Common.noneValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias PositionDescriptor = PositionFactory -> Property.Value

type alias PositionFactory =
  {
    position : String -> Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

positionFactory : PositionFactory
positionFactory =
  {
    position str = Property.stringValue str
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias DisplayDescriptor = DisplayFactory -> Property.Value

type alias DisplayFactory =
  {
    display : String -> Property.Value
  , none_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

displayFactory : DisplayFactory
displayFactory =
  {
    display str = Property.stringValue str
  , none_ = Common.noneValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.inheritValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias OverflowDescriptor = OverflowFactory -> Property.Value

type alias OverflowFactory =
  {
    overflow : String -> Property.Value
  , visible_ : Property.Value
  , hidden_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , auto_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

overflowFactory : OverflowFactory
overflowFactory =
  {
    overflow str = Property.stringValue str
  , visible_ = Common.visibleValue
  , hidden_ = Common.hiddenValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , auto_ = Common.autoValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias VisibilityDescriptor rec =
  VisibilityFactory rec -> Property.Value

type alias VisibilityFactory rec =
  { rec | visibility : String -> Property.Value
        , inherit_ : Property.Value
        , initial_ : Property.Value
        , unset_ : Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
visibilityFactory : VisibilityFactory {}
visibilityFactory =
  {
    visibility str = Property.stringValue str
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

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

type alias ClipFactory a b c d =
  { rect_ : Linear.NubSizeDescriptor {} a -> 
            Linear.NubSizeDescriptor {} b -> 
            Linear.NubSizeDescriptor {} c -> 
            Linear.NubSizeDescriptor {} d -> 
            Property.Value
  , auto_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

clipFactory : ClipFactory a b c d
clipFactory =
  {
    rect_ top right bottom left = 
      let quadrupleDescriptor = Property.commaQuadrupleValue top right bottom left
          factory = 
            (Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory)
          quadrupleValue = quadrupleDescriptor factory
          prefixValue = Property.stringValue "rect("
          suffixValue = Property.stringValue ")"
      in Property.concatenateValues [ prefixValue, quadrupleValue, suffixValue ]    
  , auto_ = Common.autoValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias OpacityDescriptor = OpacityFactory -> Property.Value

type alias OpacityFactory =
  {
    opacity : Float -> Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

opacityFactory : OpacityFactory
opacityFactory =
  {
    opacity num = Property.floatValue num
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias ZIndexDescriptor = ZIndexFactory -> Property.Value

type alias ZIndexFactory =
  {
    zIndex : Int -> Property.Value
  , auto_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

zIndexFactory : ZIndexFactory
zIndexFactory =
  {
    zIndex num = Property.intValue num
  , auto_ = Common.autoValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias PointerEventsDescriptor = PointerEventsFactory -> Property.Value

type alias PointerEventsFactory =
  {
    pointerEvents : String -> Property.Value
  , auto_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

pointerEventsFactory : PointerEventsFactory
pointerEventsFactory =
  {
    pointerEvents str = Property.stringValue str
  , auto_ = Common.autoValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias VerticalAlignDescriptor = VerticalAlignFactory -> Property.Value

-- Since NubSizeDescriptor is parameterized by a generic type `a` rather than
-- simply by `Size a`, that means that for dimensioned sizes it just calls
-- whatever `size` function is passed to it in the record -- that function
-- doesn't need to return a `Size`. So we can pass this factory to a 
-- NubSizeDescriptor and get a `VerticalAlignValue` out instead of a `Size`.
type alias VerticalAlignFactory =
  { size : Property.Value -> Property.Value
  , vAlign : String -> Property.Value
  , baseline_ : Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

verticalAlignFactory : VerticalAlignFactory
verticalAlignFactory =
  { size value = value
  , vAlign str = Property.stringValue str
  , baseline_ = Common.baselineValue
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias CursorDescriptor = CursorFactory -> Property.Value

type alias CursorFactory =
  {
    cursor : String -> Property.Value
  , auto_ : Property.Value
  , none_ : Property.Value
  , inherit_ : Property.Value
  , initial_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

cursorFactory : CursorFactory
cursorFactory =
  { cursor str = Property.stringValue str
  , auto_ = Common.autoValue
  , none_ = Common.noneValue
  , inherit_ = Common.inheritValue
  , initial_ = Common.initialValue
  , other_ val = Common.otherValue val
  }
