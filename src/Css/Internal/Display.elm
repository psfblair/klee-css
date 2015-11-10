module Css.Internal.Display
  ( FloatStyleDescriptor, floatStyleFactory
  , ClearDescriptor, clearFactory
  , PositionDescriptor, positionFactory
  , DisplayDescriptor, displayFactory
  , OverflowDescriptor, overflowFactory
  , VisibilityDescriptor, visibilityFactory
  , ClipDescriptor, clipFactory
  , OpacityDescriptor, opacityFactory
  , ZIndexDescriptor, zIndexFactory
  , PointerEventsDescriptor, pointerEventsFactory
  , VerticalAlignDescriptor, verticalAlignFactory
  , CursorDescriptor, cursorFactory
  ) where

import Css.Internal.Property exposing 
  ( Value, concatenateValues, stringValue
  , intValue, floatValue, commaQuadrupleValue
  )

import Css.Internal.Common exposing 
  ( autoValue, baselineValue, inheritValue, initialValue
  , noneValue, visibleValue, hiddenValue, otherValue
  )
 
import Css.Internal.Size exposing (Size, sizeValue)

-------------------------------------------------------------------------------

type alias FloatStyleDescriptor = FloatStyleFactory -> Value

type alias FloatStyleFactory =
  {
    floatStyle : String -> Value
  , none_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

floatStyleFactory : FloatStyleFactory
floatStyleFactory =
  {
    floatStyle str = stringValue str
  , none_ = noneValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias ClearDescriptor = ClearFactory -> Value

type alias ClearFactory =
  {
    clear: String -> Value
  , none_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

clearFactory : ClearFactory
clearFactory =
  {
    clear str = stringValue str
  , none_ = noneValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias PositionDescriptor = PositionFactory -> Value

type alias PositionFactory =
  {
    position : String -> Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

positionFactory : PositionFactory
positionFactory =
  {
    position str = stringValue str
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias DisplayDescriptor = DisplayFactory -> Value

type alias DisplayFactory =
  {
    display : String -> Value
  , none_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

displayFactory : DisplayFactory
displayFactory =
  {
    display str = stringValue str
  , none_ = noneValue
  , inherit_ = inheritValue
  , initial_ = inheritValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias OverflowDescriptor = OverflowFactory -> Value

type alias OverflowFactory =
  {
    overflow : String -> Value
  , visible_ : Value
  , hidden_ : Value
  , inherit_ : Value
  , initial_ : Value
  , auto_ : Value
  , other_ : Value -> Value
  }

overflowFactory : OverflowFactory
overflowFactory =
  {
    overflow str = stringValue str
  , visible_ = visibleValue
  , hidden_ = hiddenValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , auto_ = autoValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias VisibilityDescriptor = VisibilityFactory -> Value

type alias VisibilityFactory =
  {
    visibility : String -> Value
  , visible_ : Value
  , hidden_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

visibilityFactory : VisibilityFactory
visibilityFactory =
  {
    visibility str = stringValue str
  , visible_ = visibleValue
  , hidden_ = hiddenValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias ClipDescriptor a b c d = ClipFactory a b c d -> Value

type alias ClipFactory a b c d =
  {
    rect : (Size a) -> (Size b) -> (Size c) -> (Size d) -> Value
  , auto_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

clipFactory : ClipFactory a b c d
clipFactory =
  {
    rect top right bottom left = 
      let szv = sizeValue
          quadrupleValue = commaQuadrupleValue szv szv szv szv (top, right, bottom, left)
          prefixValue = stringValue "rect("
          suffixValue = stringValue ")"
      in concatenateValues [ prefixValue, quadrupleValue, suffixValue ]    
  , auto_ = autoValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias OpacityDescriptor = OpacityFactory -> Value

type alias OpacityFactory =
  {
    opacity : Float -> Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

opacityFactory : OpacityFactory
opacityFactory =
  {
    opacity num = floatValue num
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias ZIndexDescriptor = ZIndexFactory -> Value

type alias ZIndexFactory =
  {
    zIndex : Int -> Value
  , auto_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

zIndexFactory : ZIndexFactory
zIndexFactory =
  {
    zIndex num = intValue num
  , auto_ = autoValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias PointerEventsDescriptor = PointerEventsFactory -> Value

type alias PointerEventsFactory =
  {
    pointerEvents : String -> Value
  , auto_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

pointerEventsFactory : PointerEventsFactory
pointerEventsFactory =
  {
    pointerEvents str = stringValue str
  , auto_ = autoValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias VerticalAlignDescriptor = VerticalAlignFactory -> Value

-- Since SizeDescriptor is parameterized by a generic type `a` rather than
-- simply by `Size a`, that means that for dimensioned sizes it just calls
-- whatever `size` function is passed to it in the record -- that function
-- doesn't need to return a `Size`. So we can pass this factory to a SizeDescriptor
-- and get a `VerticalAlignValue` out instead of a `Size`.
type alias VerticalAlignFactory =
  { size : Value -> Value
  , vAlign : String -> Value
  , baseline_ : Value
  , initial_ : Value
  , inherit_ : Value
  , other_ : Value -> Value
  }

verticalAlignFactory : VerticalAlignFactory
verticalAlignFactory =
  { size value = value
  , vAlign str = stringValue str
  , baseline_ = baselineValue
  , initial_ = initialValue
  , inherit_ = inheritValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias CursorDescriptor = CursorFactory -> Value

type alias CursorFactory =
  {
    cursor : String -> Value
  , auto_ : Value
  , none_ : Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

cursorFactory : CursorFactory
cursorFactory =
  { cursor str = stringValue str
  , auto_ = autoValue
  , none_ = noneValue
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }
