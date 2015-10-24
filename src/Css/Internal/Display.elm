module Css.Internal.Display
  ( FloatStyleDescriptor, floatStyleFactory, floatStyleValueFactory
  , ClearDescriptor, clearFactory, clearValueFactory
  , PositionDescriptor, positionFactory, positionValueFactory
  , DisplayDescriptor, displayFactory, displayValueFactory
  , OverflowDescriptor, overflowFactory, overflowValueFactory
  , VisibilityDescriptor, visibilityFactory, visibilityValueFactory
  , ClipDescriptor, clipFactory, clipValueFactory
  , OpacityDescriptor, opacityFactory, opacityValueFactory
  , ZIndexDescriptor, zIndexFactory, zIndexValueFactory
  , PointerEventsDescriptor, pointerEventsFactory, pointerEventsValueFactory
  , VerticalAlignDescriptor, verticalAlignFactory, verticalAlignValueFactory
  , CursorDescriptor, cursorFactory, cursorValueFactory 
  ) where

import Css.Internal.Property exposing 
  ( Value, ValueFactory, concatenateValues, stringValueFactory
  , intValueFactory, floatValueFactory, commaQuadrupleValueFactory
  )

import Css.Internal.Common exposing 
  ( autoValue, baselineValue, inheritValue, initialValue
  , noneValue, visibleValue, hiddenValue, otherValue
  )
 
import Css.Size exposing (Size, sizeValueFactory)

-------------------------------------------------------------------------------

type alias FloatStyleDescriptor = FloatStyleFactory -> FloatStyle

type FloatStyle
  = FloatStyle String
  | NoFloat
  | InheritFloat
  | InitialFloat
  | OtherFloat String

type alias FloatStyleFactory =
  {
    floatStyle: String -> FloatStyle
  , none: FloatStyle
  , inherit: FloatStyle
  , initial: FloatStyle
  , other: String -> FloatStyle
  }

floatStyleFactory : FloatStyleFactory
floatStyleFactory =
  {
    floatStyle str = FloatStyle str
  , none = NoFloat
  , inherit = InheritFloat
  , initial = InitialFloat
  , other str = OtherFloat str
  }

floatStyleValueFactory : ValueFactory FloatStyle
floatStyleValueFactory =
  { value floatStyle =
      case floatStyle of
        FloatStyle str -> stringValueFactory.value str
        NoFloat -> noneValue
        InheritFloat -> inheritValue
        InitialFloat -> initialValue
        OtherFloat str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias ClearDescriptor = ClearFactory -> Clear

type Clear
  = Clear String
  | NoClear
  | InheritClear
  | InitialClear
  | OtherClear String

type alias ClearFactory =
  {
    clear: String -> Clear
  , none: Clear
  , inherit: Clear
  , initial: Clear
  , other: String -> Clear
  }

clearFactory : ClearFactory
clearFactory =
  {
    clear str = Clear str
  , none = NoClear
  , inherit = InheritClear
  , initial = InitialClear
  , other str = OtherClear str
  }

clearValueFactory : ValueFactory Clear
clearValueFactory =
  { value clearValue =
      case clearValue of
        Clear str -> stringValueFactory.value str
        NoClear -> noneValue
        InheritClear -> inheritValue
        InitialClear -> initialValue
        OtherClear str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias PositionDescriptor = PositionFactory -> Position

type Position
  = Position String
  | InheritPosition
  | InitialPosition
  | OtherPosition String

type alias PositionFactory =
  {
    position: String -> Position
  , inherit: Position
  , initial: Position
  , other: String -> Position
  }

positionFactory : PositionFactory
positionFactory =
  {
    position str = Position str
  , inherit = InheritPosition
  , initial = InitialPosition
  , other str = OtherPosition str
  }

positionValueFactory : ValueFactory Position
positionValueFactory =
  { value positionValue =
      case positionValue of
        Position str -> stringValueFactory.value str
        InheritPosition -> inheritValue
        InitialPosition -> initialValue
        OtherPosition str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias DisplayDescriptor = DisplayFactory -> Display

type Display
  = Display String
  | NoDisplay
  | InheritDisplay
  | InitialDisplay
  | OtherDisplay String

type alias DisplayFactory =
  {
    display: String -> Display
  , none: Display
  , inherit: Display
  , initial: Display
  , other: String -> Display
  }

displayFactory : DisplayFactory
displayFactory =
  {
    display str = Display str
  , none = NoDisplay
  , inherit = InheritDisplay
  , initial = InitialDisplay
  , other str = OtherDisplay str
  }

displayValueFactory : ValueFactory Display
displayValueFactory =
  { value displayValue =
      case displayValue of
        Display str -> stringValueFactory.value str
        NoDisplay -> noneValue
        InheritDisplay -> inheritValue
        InitialDisplay -> inheritValue
        OtherDisplay str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias OverflowDescriptor = OverflowFactory -> Overflow

type Overflow
  = Overflow String
  | VisibleOverflow
  | HiddenOverflow
  | InheritOverflow
  | InitialOverflow
  | AutoOverflow
  | OtherOverflow String

type alias OverflowFactory =
  {
    overflow: String -> Overflow
  , visible: Overflow
  , hidden: Overflow
  , inherit: Overflow
  , initial: Overflow
  , auto: Overflow
  , other: String -> Overflow
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
  , other str = OtherOverflow str
  }

overflowValueFactory : ValueFactory Overflow
overflowValueFactory =
  { value overflowValue =
      case overflowValue of
        Overflow str -> stringValueFactory.value str
        VisibleOverflow -> visibleValue
        HiddenOverflow -> hiddenValue
        InheritOverflow -> inheritValue
        InitialOverflow -> initialValue
        AutoOverflow -> autoValue
        OtherOverflow str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias VisibilityDescriptor = VisibilityFactory -> Visibility

type Visibility
  = Visibility String
  | VisibleVisibility
  | HiddenVisibility
  | InheritVisibility
  | InitialVisibility
  | OtherVisibility String

type alias VisibilityFactory =
  {
    visibility: String -> Visibility
  , visible: Visibility
  , hidden: Visibility
  , inherit: Visibility
  , initial: Visibility
  , other: String -> Visibility
  }

visibilityFactory : VisibilityFactory
visibilityFactory =
  {
    visibility str = Visibility str
  , visible = VisibleVisibility
  , hidden = HiddenVisibility
  , inherit = InheritVisibility
  , initial = InitialVisibility
  , other str = OtherVisibility str
  }

visibilityValueFactory : ValueFactory Visibility
visibilityValueFactory =
  { value visibilityValue =
      case visibilityValue of
        Visibility str -> stringValueFactory.value str
        VisibleVisibility -> visibleValue
        HiddenVisibility -> hiddenValue
        InheritVisibility -> inheritValue
        InitialVisibility -> initialValue
        OtherVisibility str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias ClipDescriptor a b c d = ClipFactory a b c d -> Clip a b c d

type Clip a b c d
  = Rect (Size a) (Size b) (Size c) (Size d)
  | AutoClip
  | InheritClip
  | InitialClip
  | OtherClip String

type alias ClipFactory a b c d =
  {
    rect: (Size a) -> (Size b) -> (Size c) -> (Size d) -> Clip a b c d
  , auto: Clip a b c d
  , inherit: Clip a b c d
  , initial: Clip a b c d
  , other: String -> Clip a b c d
  }

clipFactory : ClipFactory a b c d
clipFactory =
  {
    rect top right bottom left = Rect top right bottom left
  , auto = AutoClip
  , inherit = InheritClip
  , initial = InitialClip
  , other str = OtherClip str
  }

clipValueFactory : ValueFactory (Clip a b c d)
clipValueFactory =
  { value clipValue =
      case clipValue of
        Rect top right bottom left ->
          let szv = sizeValueFactory
              quadrupleValueFactory = commaQuadrupleValueFactory szv szv szv szv
              quadrupleValue = quadrupleValueFactory.value (top, right, bottom, left)
              prefixValue = stringValueFactory.value "rect("
              suffixValue = stringValueFactory.value ")"
          in concatenateValues [ prefixValue, quadrupleValue, suffixValue ]
        AutoClip -> autoValue
        InheritClip -> inheritValue
        InitialClip -> initialValue
        OtherClip str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias OpacityDescriptor = OpacityFactory -> Opacity

type Opacity
  = Opacity Float
  | InheritOpacity
  | InitialOpacity
  | OtherOpacity String

type alias OpacityFactory =
  {
    opacity : Float -> Opacity
  , inherit : Opacity
  , initial : Opacity
  , other : String -> Opacity
  }

opacityFactory : OpacityFactory
opacityFactory =
  {
    opacity num = Opacity num
  , inherit = InheritOpacity
  , initial = InitialOpacity
  , other str = OtherOpacity str
  }

opacityValueFactory : ValueFactory Opacity
opacityValueFactory =
  { value opacityLevel =
      case opacityLevel of
        Opacity num -> floatValueFactory.value num
        InheritOpacity -> inheritValue
        InitialOpacity -> inheritValue
        OtherOpacity str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias ZIndexDescriptor = ZIndexFactory -> ZIndex

type ZIndex
  = ZIndex Int
  | AutoZIndex
  | InheritZIndex
  | InitialZIndex
  | OtherZIndex String

type alias ZIndexFactory =
  {
    zIndex: Int -> ZIndex
  , auto: ZIndex
  , inherit: ZIndex
  , initial: ZIndex
  , other: String -> ZIndex
  }

zIndexFactory : ZIndexFactory
zIndexFactory =
  {
    zIndex num = ZIndex num
  , auto = AutoZIndex
  , inherit = InheritZIndex
  , initial = InitialZIndex
  , other str = OtherZIndex str
  }

zIndexValueFactory : ValueFactory ZIndex
zIndexValueFactory =
  { value zIdx =
      case zIdx of
        ZIndex num -> intValueFactory.value num
        AutoZIndex -> autoValue
        InheritZIndex -> inheritValue
        InitialZIndex -> initialValue
        OtherZIndex str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias PointerEventsDescriptor = PointerEventsFactory -> PointerEvents

type PointerEvents
  = PointerEvents String
  | VisiblePointerEvents
  | InheritPointerEvents
  | InitialPointerEvents
  | AutoPointerEvents
  | NoPointerEvents
  | OtherPointerEvents String

type alias PointerEventsFactory =
  {
    pointerEvents: String -> PointerEvents
  , auto: PointerEvents
  , inherit: PointerEvents
  , initial: PointerEvents
  , other: String -> PointerEvents
  }

pointerEventsFactory : PointerEventsFactory
pointerEventsFactory =
  {
    pointerEvents str = PointerEvents str
  , auto = AutoPointerEvents
  , inherit = InheritPointerEvents
  , initial = InitialPointerEvents
  , other str = OtherPointerEvents str
  }

pointerEventsValueFactory : ValueFactory PointerEvents
pointerEventsValueFactory =
  { value pointerEvts =
      case pointerEvts of
        PointerEvents str -> stringValueFactory.value str
        AutoPointerEvents -> autoValue
        InheritPointerEvents -> inheritValue
        InitialPointerEvents -> initialValue
        OtherPointerEvents str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias VerticalAlignDescriptor = VerticalAlignFactory -> VerticalAlign

type VerticalAlign
  = VerticalAlign Value
  | BaselineVerticalAlign
  | InitialVerticalAlign
  | InheritVerticalAlign
  | OtherVerticalAlign String

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
  , other: String -> VerticalAlign
  }

verticalAlignFactory : VerticalAlignFactory
verticalAlignFactory =
  { size value = VerticalAlign value
  , vAlign str = stringValueFactory.value str |> VerticalAlign
  , baseline = BaselineVerticalAlign
  , initial = InitialVerticalAlign
  , inherit = InheritVerticalAlign
  , other str = OtherVerticalAlign str
  }

verticalAlignValueFactory : ValueFactory VerticalAlign
verticalAlignValueFactory =
  { value valign =
      case valign of
        VerticalAlign value -> value
        BaselineVerticalAlign -> baselineValue
        InitialVerticalAlign -> initialValue
        InheritVerticalAlign -> inheritValue
        OtherVerticalAlign str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias CursorDescriptor = CursorFactory -> Cursor

type Cursor
  = Cursor String
  | AutoCursor
  | NoCursor
  | InheritCursor
  | InitialCursor
  | OtherCursor String

type alias CursorFactory =
  {
    cursor: String -> Cursor
  , auto: Cursor
  , none : Cursor
  , inherit: Cursor
  , initial: Cursor
  , other: String -> Cursor
  }

cursorFactory : CursorFactory
cursorFactory =
  {
    cursor str = Cursor str
  , auto = AutoCursor
  , none = NoCursor
  , inherit = InheritCursor
  , initial = InitialCursor
  , other str = OtherCursor str
  }

cursorValueFactory : ValueFactory Cursor
cursorValueFactory =
  { value cursorValue =
      case cursorValue of
        Cursor str -> stringValueFactory.value str
        AutoCursor -> autoValue
        NoCursor -> noneValue
        InheritCursor -> inheritValue
        InitialCursor -> initialValue
        OtherCursor str -> otherValue str
  }
