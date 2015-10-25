module Css.Internal.Display
  ( FloatStyleDescriptor, floatStyleFactory, floatStyleValue
  , ClearDescriptor, clearFactory, clearValue
  , PositionDescriptor, positionFactory, positionValue
  , DisplayDescriptor, displayFactory, displayValue
  , OverflowDescriptor, overflowFactory, overflowValue
  , VisibilityDescriptor, visibilityFactory, visibilityValue
  , ClipDescriptor, clipFactory, clipValue
  , OpacityDescriptor, opacityFactory, opacityValue
  , ZIndexDescriptor, zIndexFactory, zIndexValue
  , PointerEventsDescriptor, pointerEventsFactory, pointerEventsValue
  , VerticalAlignDescriptor, verticalAlignFactory, verticalAlignValue
  , CursorDescriptor, cursorFactory, cursorValue 
  ) where

import Css.Internal.Property exposing 
  ( Value, ValueElement, concatenateValues, stringValue
  , intValue, floatValue, commaQuadrupleValue
  )

import Css.Internal.Common exposing 
  ( autoValue, baselineValue, inheritValue, initialValue
  , noneValue, visibleValue, hiddenValue, otherValue
  )
 
import Css.Internal.Size exposing (Size, sizeValue)

-------------------------------------------------------------------------------

type alias FloatStyleDescriptor = FloatStyleFactory -> FloatStyle

type FloatStyle
  = FloatStyle String
  | NoFloat
  | InheritFloat
  | InitialFloat
  | OtherFloat ValueElement

type alias FloatStyleFactory =
  {
    floatStyle: String -> FloatStyle
  , none: FloatStyle
  , inherit: FloatStyle
  , initial: FloatStyle
  , other: ValueElement -> FloatStyle
  }

floatStyleFactory : FloatStyleFactory
floatStyleFactory =
  {
    floatStyle str = FloatStyle str
  , none = NoFloat
  , inherit = InheritFloat
  , initial = InitialFloat
  , other valElement = OtherFloat valElement
  }

floatStyleValue : FloatStyle -> Value 
floatStyleValue floatStyle =
  case floatStyle of
    FloatStyle str -> stringValue str
    NoFloat -> noneValue
    InheritFloat -> inheritValue
    InitialFloat -> initialValue
    OtherFloat valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias ClearDescriptor = ClearFactory -> Clear

type Clear
  = Clear String
  | NoClear
  | InheritClear
  | InitialClear
  | OtherClear ValueElement

type alias ClearFactory =
  {
    clear: String -> Clear
  , none: Clear
  , inherit: Clear
  , initial: Clear
  , other: ValueElement -> Clear
  }

clearFactory : ClearFactory
clearFactory =
  {
    clear str = Clear str
  , none = NoClear
  , inherit = InheritClear
  , initial = InitialClear
  , other valElement = OtherClear valElement
  }

clearValue : Clear -> Value 
clearValue clearValue =
  case clearValue of
    Clear str -> stringValue str
    NoClear -> noneValue
    InheritClear -> inheritValue
    InitialClear -> initialValue
    OtherClear str -> otherValue str

-------------------------------------------------------------------------------

type alias PositionDescriptor = PositionFactory -> Position

type Position
  = Position String
  | InheritPosition
  | InitialPosition
  | OtherPosition ValueElement

type alias PositionFactory =
  {
    position: String -> Position
  , inherit: Position
  , initial: Position
  , other: ValueElement -> Position
  }

positionFactory : PositionFactory
positionFactory =
  {
    position str = Position str
  , inherit = InheritPosition
  , initial = InitialPosition
  , other valElement = OtherPosition valElement
  }

positionValue : Position -> Value 
positionValue positionValue =
  case positionValue of
    Position str -> stringValue str
    InheritPosition -> inheritValue
    InitialPosition -> initialValue
    OtherPosition str -> otherValue str

-------------------------------------------------------------------------------

type alias DisplayDescriptor = DisplayFactory -> Display

type Display
  = Display String
  | NoDisplay
  | InheritDisplay
  | InitialDisplay
  | OtherDisplay ValueElement

type alias DisplayFactory =
  {
    display: String -> Display
  , none: Display
  , inherit: Display
  , initial: Display
  , other: ValueElement -> Display
  }

displayFactory : DisplayFactory
displayFactory =
  {
    display str = Display str
  , none = NoDisplay
  , inherit = InheritDisplay
  , initial = InitialDisplay
  , other valElement = OtherDisplay valElement
  }

displayValue : Display -> Value 
displayValue displayValue =
  case displayValue of
    Display str -> stringValue str
    NoDisplay -> noneValue
    InheritDisplay -> inheritValue
    InitialDisplay -> inheritValue
    OtherDisplay str -> otherValue str

-------------------------------------------------------------------------------

type alias OverflowDescriptor = OverflowFactory -> Overflow

type Overflow
  = Overflow String
  | VisibleOverflow
  | HiddenOverflow
  | InheritOverflow
  | InitialOverflow
  | AutoOverflow
  | OtherOverflow ValueElement

type alias OverflowFactory =
  {
    overflow: String -> Overflow
  , visible: Overflow
  , hidden: Overflow
  , inherit: Overflow
  , initial: Overflow
  , auto: Overflow
  , other: ValueElement -> Overflow
  }

overflowFactory : OverflowFactory
overflowFactory =
  {
    overflow str = Overflow str
  , visible = VisibleOverflow
  , hidden = HiddenOverflow
  , inherit = InheritOverflow
  , initial = InitialOverflow
  , auto = AutoOverflow
  , other valElement = OtherOverflow valElement
  }

overflowValue : Overflow -> Value 
overflowValue overflowValue =
  case overflowValue of
    Overflow str -> stringValue str
    VisibleOverflow -> visibleValue
    HiddenOverflow -> hiddenValue
    InheritOverflow -> inheritValue
    InitialOverflow -> initialValue
    AutoOverflow -> autoValue
    OtherOverflow str -> otherValue str

-------------------------------------------------------------------------------

type alias VisibilityDescriptor = VisibilityFactory -> Visibility

type Visibility
  = Visibility String
  | VisibleVisibility
  | HiddenVisibility
  | InheritVisibility
  | InitialVisibility
  | OtherVisibility ValueElement

type alias VisibilityFactory =
  {
    visibility: String -> Visibility
  , visible: Visibility
  , hidden: Visibility
  , inherit: Visibility
  , initial: Visibility
  , other: ValueElement -> Visibility
  }

visibilityFactory : VisibilityFactory
visibilityFactory =
  {
    visibility str = Visibility str
  , visible = VisibleVisibility
  , hidden = HiddenVisibility
  , inherit = InheritVisibility
  , initial = InitialVisibility
  , other valElement = OtherVisibility valElement
  }

visibilityValue : Visibility -> Value 
visibilityValue visibilityValue =
  case visibilityValue of
    Visibility str -> stringValue str
    VisibleVisibility -> visibleValue
    HiddenVisibility -> hiddenValue
    InheritVisibility -> inheritValue
    InitialVisibility -> initialValue
    OtherVisibility str -> otherValue str

-------------------------------------------------------------------------------

type alias ClipDescriptor a b c d = ClipFactory a b c d -> Clip a b c d

type Clip a b c d
  = Rect (Size a) (Size b) (Size c) (Size d)
  | AutoClip
  | InheritClip
  | InitialClip
  | OtherClip ValueElement

type alias ClipFactory a b c d =
  {
    rect: (Size a) -> (Size b) -> (Size c) -> (Size d) -> Clip a b c d
  , auto: Clip a b c d
  , inherit: Clip a b c d
  , initial: Clip a b c d
  , other: ValueElement -> Clip a b c d
  }

clipFactory : ClipFactory a b c d
clipFactory =
  {
    rect top right bottom left = Rect top right bottom left
  , auto = AutoClip
  , inherit = InheritClip
  , initial = InitialClip
  , other valElement = OtherClip valElement
  }

clipValue : Clip a b c d -> Value
clipValue clipValue =
  case clipValue of
    Rect top right bottom left ->
      let szv = sizeValue
          quadrupleValue = commaQuadrupleValue szv szv szv szv (top, right, bottom, left)
          prefixValue = stringValue "rect("
          suffixValue = stringValue ")"
      in concatenateValues [ prefixValue, quadrupleValue, suffixValue ]
    AutoClip -> autoValue
    InheritClip -> inheritValue
    InitialClip -> initialValue
    OtherClip str -> otherValue str

-------------------------------------------------------------------------------

type alias OpacityDescriptor = OpacityFactory -> Opacity

type Opacity
  = Opacity Float
  | InheritOpacity
  | InitialOpacity
  | OtherOpacity ValueElement

type alias OpacityFactory =
  {
    opacity : Float -> Opacity
  , inherit : Opacity
  , initial : Opacity
  , other : ValueElement -> Opacity
  }

opacityFactory : OpacityFactory
opacityFactory =
  {
    opacity num = Opacity num
  , inherit = InheritOpacity
  , initial = InitialOpacity
  , other valElement = OtherOpacity valElement
  }

opacityValue : Opacity -> Value 
opacityValue opacityLevel =
  case opacityLevel of
    Opacity num -> floatValue num
    InheritOpacity -> inheritValue
    InitialOpacity -> inheritValue
    OtherOpacity str -> otherValue str

-------------------------------------------------------------------------------

type alias ZIndexDescriptor = ZIndexFactory -> ZIndex

type ZIndex
  = ZIndex Int
  | AutoZIndex
  | InheritZIndex
  | InitialZIndex
  | OtherZIndex ValueElement

type alias ZIndexFactory =
  {
    zIndex: Int -> ZIndex
  , auto: ZIndex
  , inherit: ZIndex
  , initial: ZIndex
  , other: ValueElement -> ZIndex
  }

zIndexFactory : ZIndexFactory
zIndexFactory =
  {
    zIndex num = ZIndex num
  , auto = AutoZIndex
  , inherit = InheritZIndex
  , initial = InitialZIndex
  , other valElement = OtherZIndex valElement
  }

zIndexValue : ZIndex -> Value 
zIndexValue zIdx =
  case zIdx of
    ZIndex num -> intValue num
    AutoZIndex -> autoValue
    InheritZIndex -> inheritValue
    InitialZIndex -> initialValue
    OtherZIndex str -> otherValue str

-------------------------------------------------------------------------------

type alias PointerEventsDescriptor = PointerEventsFactory -> PointerEvents

type PointerEvents
  = PointerEvents String
  | VisiblePointerEvents
  | InheritPointerEvents
  | InitialPointerEvents
  | AutoPointerEvents
  | NoPointerEvents
  | OtherPointerEvents ValueElement

type alias PointerEventsFactory =
  {
    pointerEvents: String -> PointerEvents
  , auto: PointerEvents
  , inherit: PointerEvents
  , initial: PointerEvents
  , other: ValueElement -> PointerEvents
  }

pointerEventsFactory : PointerEventsFactory
pointerEventsFactory =
  {
    pointerEvents str = PointerEvents str
  , auto = AutoPointerEvents
  , inherit = InheritPointerEvents
  , initial = InitialPointerEvents
  , other valElement = OtherPointerEvents valElement
  }

pointerEventsValue : PointerEvents -> Value 
pointerEventsValue pointerEvts =
  case pointerEvts of
    PointerEvents str -> stringValue str
    AutoPointerEvents -> autoValue
    InheritPointerEvents -> inheritValue
    InitialPointerEvents -> initialValue
    OtherPointerEvents str -> otherValue str

-------------------------------------------------------------------------------

type alias VerticalAlignDescriptor = VerticalAlignFactory -> VerticalAlign

type VerticalAlign
  = VerticalAlign Value
  | BaselineVerticalAlign
  | InitialVerticalAlign
  | InheritVerticalAlign
  | OtherVerticalAlign ValueElement

-- Since SizeDescriptor is parameterized by a generic type `a` rather than
-- simply by `Size a`, that means that for dimensioned sizes it just calls
-- whatever `size` function is passed to it in the record -- that function
-- doesn't need to return a `Size`. So we can pass this factory to a SizeDescriptor
-- and get a `VerticalAlignValue` out instead of a `Size`.
type alias VerticalAlignFactory =
  { size: Value -> VerticalAlign
  , vAlign: String -> VerticalAlign
  , baseline: VerticalAlign
  , initial: VerticalAlign
  , inherit: VerticalAlign
  , other: ValueElement -> VerticalAlign
  }

verticalAlignFactory : VerticalAlignFactory
verticalAlignFactory =
  { size value = VerticalAlign value
  , vAlign str = stringValue str |> VerticalAlign
  , baseline = BaselineVerticalAlign
  , initial = InitialVerticalAlign
  , inherit = InheritVerticalAlign
  , other valElement = OtherVerticalAlign valElement
  }

verticalAlignValue : VerticalAlign -> Value 
verticalAlignValue valign =
  case valign of
    VerticalAlign value -> value
    BaselineVerticalAlign -> baselineValue
    InitialVerticalAlign -> initialValue
    InheritVerticalAlign -> inheritValue
    OtherVerticalAlign str -> otherValue str

-------------------------------------------------------------------------------

type alias CursorDescriptor = CursorFactory -> Cursor

type Cursor
  = Cursor String
  | AutoCursor
  | NoCursor
  | InheritCursor
  | InitialCursor
  | OtherCursor ValueElement

type alias CursorFactory =
  {
    cursor: String -> Cursor
  , auto: Cursor
  , none : Cursor
  , inherit: Cursor
  , initial: Cursor
  , other: ValueElement -> Cursor
  }

cursorFactory : CursorFactory
cursorFactory =
  { cursor str = Cursor str
  , auto = AutoCursor
  , none = NoCursor
  , inherit = InheritCursor
  , initial = InitialCursor
  , other valElement = OtherCursor valElement
  }

cursorValue : Cursor -> Value 
cursorValue cursorValue =
  case cursorValue of
    Cursor str -> stringValue str
    AutoCursor -> autoValue
    NoCursor -> noneValue
    InheritCursor -> inheritValue
    InitialCursor -> initialValue
    OtherCursor str -> otherValue str
