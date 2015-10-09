module Css.Display (
  -- * Float

    FloatStyle
  , float
  , floatLeft, floatRight

  , Clear
  , clear
  , both , clearLeft, clearRight

  -- * Position

  , Position
  , position
  , static, absolute, fixed, relative

  -- * Display

  , Display
  , display
  , inline, block, listItem, runIn, inlineBlock, table, inlineTable, tableRowGroup
  , tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn
  , tableCell, tableCaption, displayNone, displayInherit, flex
  , inlineFlex, grid, inlineGrid

  -- * Overlow

  , Overflow
  , scroll
  , overflow, overflowX, overflowY

  -- * Visibility

  , Visibility
  , collapse, separate

  , visibility

  -- Clipping

  , Clip
  , clip
  , rect

  -- * Opacity

  , opacity

  -- * Z-index

  , zIndex

  -- * Pointer-events

  , PointerEvents
  , pointerEvents
  , visiblePainted, visibleFill, visibleStroke, painted
  , fillEvents, strokeEvents, allEvents

  -- * Vertical align
{-
  , VerticalAlign(..)
  , middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom

  -- * Cursor

  , Cursor(..)
  , crosshair, cursorDefault, pointer, move, eResize, neResize, nwResize, nResize
  , seResize, swResize, sResize, wResize, cursorText, wait, cursorProgress, help, cursorUrl

-}
  -- Used by other modules
  , VisibilityDescriptor, visibilityFactory, visibilityValueFactory

  ) where

import Css.Internal.Property exposing (Value, ValueFactory, concatenateValues, stringKey
  , stringValueFactory, intValueFactory, floatValueFactory, commaQuadrupleValueFactory
  )
import Css.Internal.Stylesheet exposing (CssGenerator, key)

import Css.Common exposing (
    Auto, Inherit, None, Visible, Hidden, Other
  , autoValueFactory, inheritValueFactory, initialValueFactory, noneValueFactory
  , visibleValueFactory, hiddenValueFactory, otherValueFactory
  )
import Css.Size exposing (Size, SizeDescriptor, sizeFactory, sizeValueFactory)

-------------------------------------------------------------------------------

type FloatStyle
  = FloatStyle String
  | NoFloat
  | InheritFloat
  | InitialFloat

type alias FloatStyleFactory =
  {
    floatStyle: String -> FloatStyle
  , none: FloatStyle
  , inherit: FloatStyle
  , initial: FloatStyle
  }

type alias FloatStyleDescriptor = FloatStyleFactory -> FloatStyle

floatStyleFactory : FloatStyleFactory
floatStyleFactory =
  {
    floatStyle str = FloatStyle str
  , none = NoFloat
  , inherit = InheritFloat
  , initial = InitialFloat
  }

floatStyleValueFactory : ValueFactory FloatStyle
floatStyleValueFactory =
  { value floatStyle =
      case floatStyle of
        FloatStyle str -> stringValueFactory.value str
        NoFloat -> noneValueFactory.none
        InheritFloat -> inheritValueFactory.inherit
        InitialFloat -> initialValueFactory.initial
  }

float : FloatStyleDescriptor -> CssGenerator FloatStyle
float descriptor =
  let floatStyle = descriptor floatStyleFactory
  in key (stringKey "float") floatStyle floatStyleValueFactory

floatLeft : FloatStyleDescriptor
floatLeft factory = factory.floatStyle "left"

floatRight : FloatStyleDescriptor
floatRight factory = factory.floatStyle "right"

-------------------------------------------------------------------------------

type Clear
  = Clear String
  | NoClear
  | InheritClear
  | InitialClear
  | OtherClear Value

type alias ClearFactory =
  {
    clear: String -> Clear
  , none: Clear
  , inherit: Clear
  , initial: Clear
  , other: Value -> Clear
  }

type alias ClearDescriptor = ClearFactory -> Clear

clearFactory : ClearFactory
clearFactory =
  {
    clear str = Clear str
  , none = NoClear
  , inherit = InheritClear
  , initial = InitialClear
  , other val = OtherClear val
  }

clearValueFactory : ValueFactory Clear
clearValueFactory =
  { value clearValue =
      case clearValue of
        Clear str -> stringValueFactory.value str
        NoClear -> noneValueFactory.none
        InheritClear -> inheritValueFactory.inherit
        InitialClear -> initialValueFactory.initial
        OtherClear value -> otherValueFactory.other value
  }

clear : ClearDescriptor -> CssGenerator Clear
clear descriptor =
  let clearValue = descriptor clearFactory
  in key (stringKey "clear") clearValue clearValueFactory

both : ClearDescriptor
both factory = factory.clear "both"

clearLeft : ClearDescriptor
clearLeft factory = factory.clear "left"

clearRight : ClearDescriptor
clearRight factory = factory.clear "right"

-------------------------------------------------------------------------------

type Position
  = Position String
  | InheritPosition
  | InitialPosition
  | OtherPosition Value

type alias PositionFactory =
  {
    position: String -> Position
  , inherit: Position
  , initial: Position
  , other: Value -> Position
  }

type alias PositionDescriptor = PositionFactory -> Position

positionFactory : PositionFactory
positionFactory =
  {
    position str = Position str
  , inherit = InheritPosition
  , initial = InitialPosition
  , other val = OtherPosition val
  }

positionValueFactory : ValueFactory Position
positionValueFactory =
  { value positionValue =
      case positionValue of
        Position str -> stringValueFactory.value str
        InheritPosition -> inheritValueFactory.inherit
        InitialPosition -> initialValueFactory.initial
        OtherPosition value -> otherValueFactory.other value
  }

position : PositionDescriptor -> CssGenerator Position
position descriptor =
  let positionValue = descriptor positionFactory
  in key (stringKey "position") positionValue positionValueFactory

static : PositionDescriptor
static factory = factory.position "static"

absolute : PositionDescriptor
absolute factory = factory.position "absolute"

fixed : PositionDescriptor
fixed factory = factory.position "fixed"

relative : PositionDescriptor
relative factory = factory.position "relative"

-------------------------------------------------------------------------------

type Display
  = Display String
  | NoDisplay
  | InheritDisplay
  | InitialDisplay
  | OtherDisplay Value

type alias DisplayFactory =
  {
    display: String -> Display
  , none: Display
  , inherit: Display
  , initial: Display
  , other: Value -> Display
  }

type alias DisplayDescriptor = DisplayFactory -> Display

displayFactory : DisplayFactory
displayFactory =
  {
    display str = Display str
  , none = NoDisplay
  , inherit = InheritDisplay
  , initial = InitialDisplay
  , other val = OtherDisplay val
  }

displayValueFactory : ValueFactory Display
displayValueFactory =
  { value displayValue =
      case displayValue of
        Display str -> stringValueFactory.value str
        NoDisplay -> noneValueFactory.none
        InheritDisplay -> inheritValueFactory.inherit
        InitialDisplay -> inheritValueFactory.inherit
        OtherDisplay value -> otherValueFactory.other value
  }

display : DisplayDescriptor -> CssGenerator Display
display descriptor =
  let displayValue = descriptor displayFactory
  in key (stringKey "display") displayValue displayValueFactory

inline : DisplayDescriptor
inline factory = factory.display "inline"

block : DisplayDescriptor
block factory = factory.display "block"

listItem : DisplayDescriptor
listItem factory = factory.display "list-item"

runIn : DisplayDescriptor
runIn factory = factory.display "runIn"

inlineBlock : DisplayDescriptor
inlineBlock factory = factory.display "inline-block"

table : DisplayDescriptor
table factory = factory.display "table"

inlineTable : DisplayDescriptor
inlineTable factory = factory.display "inline-table"

tableRowGroup : DisplayDescriptor
tableRowGroup factory = factory.display "table-row-Group"

tableHeaderGroup : DisplayDescriptor
tableHeaderGroup factory = factory.display "table-header-group"

tableFooterGroup : DisplayDescriptor
tableFooterGroup factory = factory.display "table-footer-group"

tableRow : DisplayDescriptor
tableRow factory = factory.display "table-row"

tableColumnGroup : DisplayDescriptor
tableColumnGroup factory = factory.display "table-column-group"

tableColumn : DisplayDescriptor
tableColumn factory = factory.display "table-column"

tableCell : DisplayDescriptor
tableCell factory = factory.display "table-cell"

tableCaption : DisplayDescriptor
tableCaption factory = factory.display "table-caption"

displayNone : DisplayDescriptor
displayNone factory = factory.display "none"

displayInherit : DisplayDescriptor
displayInherit factory = factory.display "inherit"

flex : DisplayDescriptor
flex factory = factory.display "flex"

inlineFlex : DisplayDescriptor
inlineFlex factory = factory.display "inline-flex"

grid : DisplayDescriptor
grid factory = factory.display "grid"

inlineGrid : DisplayDescriptor
inlineGrid factory = factory.display "inline-grid"

-------------------------------------------------------------------------------

type Overflow
  = Overflow String
  | VisibleOverflow
  | HiddenOverflow
  | InheritOverflow
  | InitialOverflow
  | AutoOverflow
  | OtherOverflow Value

type alias OverflowFactory =
  {
    overflow: String -> Overflow
  , visible: Overflow
  , hidden: Overflow
  , inherit: Overflow
  , initial: Overflow
  , auto: Overflow
  , other: Value -> Overflow
  }

type alias OverflowDescriptor = OverflowFactory -> Overflow

overflowFactory : OverflowFactory
overflowFactory =
  {
    overflow str = Overflow str
  , visible = VisibleOverflow
  , hidden = HiddenOverflow
  , inherit = InheritOverflow
  , initial = InitialOverflow
  , auto = AutoOverflow
  , other val = OtherOverflow val
  }

overflowValueFactory : ValueFactory Overflow
overflowValueFactory =
  { value overflowValue =
      case overflowValue of
        Overflow str -> stringValueFactory.value str
        VisibleOverflow -> visibleValueFactory.visible
        HiddenOverflow -> hiddenValueFactory.hidden
        InheritOverflow -> inheritValueFactory.inherit
        InitialOverflow -> initialValueFactory.initial
        AutoOverflow -> autoValueFactory.auto
        OtherOverflow value -> otherValueFactory.other value
  }

overflow : OverflowDescriptor -> CssGenerator Overflow
overflow descriptor =
  let overflowValue = descriptor overflowFactory
  in key (stringKey "overflow") overflowValue overflowValueFactory

overflowX : OverflowDescriptor -> CssGenerator Overflow
overflowX descriptor =
  let overflowValue = descriptor overflowFactory
  in key (stringKey "overflow-x") overflowValue overflowValueFactory

overflowY : OverflowDescriptor -> CssGenerator Overflow
overflowY descriptor =
  let overflowValue = descriptor overflowFactory
  in key (stringKey "overflow-y") overflowValue overflowValueFactory

scroll : OverflowDescriptor
scroll factory = factory.overflow "scroll"

-------------------------------------------------------------------------------

type Visibility
  = Visibility String
  | VisibleVisibility
  | HiddenVisibility
  | InheritVisibility
  | InitialVisibility
  | OtherVisibility Value

type alias VisibilityFactory =
  {
    visibility: String -> Visibility
  , visible: Visibility
  , hidden: Visibility
  , inherit: Visibility
  , initial: Visibility
  , other: Value -> Visibility
  }

type alias VisibilityDescriptor = VisibilityFactory -> Visibility

visibilityFactory : VisibilityFactory
visibilityFactory =
  {
    visibility str = Visibility str
  , visible = VisibleVisibility
  , hidden = HiddenVisibility
  , inherit = InheritVisibility
  , initial = InitialVisibility
  , other val = OtherVisibility val
  }

visibilityValueFactory : ValueFactory Visibility
visibilityValueFactory =
  { value visibilityValue =
      case visibilityValue of
        Visibility str -> stringValueFactory.value str
        VisibleVisibility -> visibleValueFactory.visible
        HiddenVisibility -> hiddenValueFactory.hidden
        InheritVisibility -> inheritValueFactory.inherit
        InitialVisibility -> initialValueFactory.initial
        OtherVisibility value -> otherValueFactory.other value
  }

visibility : VisibilityDescriptor -> CssGenerator Visibility
visibility descriptor =
  let visibilityValue = descriptor visibilityFactory
  in key (stringKey "visibility") visibilityValue visibilityValueFactory

separate : VisibilityDescriptor
separate factory = factory.visibility "separate"

collapse : VisibilityDescriptor
collapse factory = factory.visibility "collapse"

-------------------------------------------------------------------------------

type Clip a b c d
  = Rect (Size a) (Size b) (Size c) (Size d)
  | AutoClip
  | InheritClip
  | InitialClip
  | OtherClip Value

type alias ClipFactory a b c d =
  {
    rect: (Size a) -> (Size b) -> (Size c) -> (Size d) -> Clip a b c d
  , auto: Clip a b c d
  , inherit: Clip a b c d
  , initial: Clip a b c d
  , other: Value -> Clip a b c d
  }

type alias ClipDescriptor a b c d = ClipFactory a b c d -> Clip a b c d

clipFactory : ClipFactory a b c d
clipFactory =
  {
    rect top right bottom left = Rect top right bottom left
  , auto = AutoClip
  , inherit = InheritClip
  , initial = InitialClip
  , other val = OtherClip val
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
        AutoClip -> autoValueFactory.auto
        InheritClip -> inheritValueFactory.inherit
        InitialClip -> initialValueFactory.initial
        OtherClip value -> otherValueFactory.other value
  }

clip : ClipDescriptor a b c d -> CssGenerator (Clip a b c d)
clip descriptor =
  let clipValue = descriptor clipFactory
  in key (stringKey "clip") clipValue clipValueFactory

rect : SizeDescriptor a -> SizeDescriptor b -> SizeDescriptor c -> SizeDescriptor d ->
        ClipDescriptor a b c d
rect top right bottom left factory =
  let t = top sizeFactory
      r = right sizeFactory
      b = bottom sizeFactory
      l = left sizeFactory
  in factory.rect t r b l

-------------------------------------------------------------------------------

type Opacity
  = Opacity Float
  | InheritOpacity
  | InitialOpacity

type alias OpacityFactory =
  {
    opacity: Float -> Opacity
  , inherit: Opacity
  , initial: Opacity
  }

type alias OpacityDescriptor = OpacityFactory -> Opacity

opacityFactory : OpacityFactory
opacityFactory =
  {
    opacity num = Opacity num
  , inherit = InheritOpacity
  , initial = InitialOpacity
  }

opacityValueFactory : ValueFactory Opacity
opacityValueFactory =
  { value opacityLevel =
      case opacityLevel of
        Opacity num -> floatValueFactory.value num
        InheritOpacity -> inheritValueFactory.inherit
        InitialOpacity -> inheritValueFactory.inherit
  }

opacity : OpacityDescriptor -> CssGenerator Opacity
opacity descriptor =
  let opacityLevel = descriptor opacityFactory
  in key (stringKey "opacity") opacityLevel opacityValueFactory

pctOpacity : Float -> OpacityDescriptor
pctOpacity level factory = factory.opacity level


-------------------------------------------------------------------------------

type ZIndex
  = ZIndex Int
  | AutoZIndex
  | InheritZIndex
  | InitialZIndex
  | OtherZIndex Value

type alias ZIndexFactory =
  {
    zIndex: Int -> ZIndex
  , auto: ZIndex
  , inherit: ZIndex
  , initial: ZIndex
  , other: Value -> ZIndex
  }

type alias ZIndexDescriptor = ZIndexFactory -> ZIndex

zIndexFactory : ZIndexFactory
zIndexFactory =
  {
    zIndex num = ZIndex num
  , auto = AutoZIndex
  , inherit = InheritZIndex
  , initial = InitialZIndex
  , other val = OtherZIndex val
  }

zIndexValueFactory : ValueFactory ZIndex
zIndexValueFactory =
  { value zIdx =
      case zIdx of
        ZIndex num -> intValueFactory.value num
        AutoZIndex -> autoValueFactory.auto
        InheritZIndex -> inheritValueFactory.inherit
        InitialZIndex -> initialValueFactory.initial
        OtherZIndex value -> otherValueFactory.other value
  }

zIndex : ZIndexDescriptor -> CssGenerator ZIndex
zIndex descriptor =
  let zIdx = descriptor zIndexFactory
  in key (stringKey "z-index") zIdx zIndexValueFactory

zLevel : Int -> ZIndexDescriptor
zLevel num factory = factory.zIndex num

-------------------------------------------------------------------------------

type PointerEvents
  = PointerEvents String
  | VisiblePointerEvents
  | InheritPointerEvents
  | InitialPointerEvents
  | AutoPointerEvents
  | NoPointerEvents
  | OtherPointerEvents Value

type alias PointerEventsFactory =
  {
    pointerEvents: String -> PointerEvents
  , auto: PointerEvents
  , inherit: PointerEvents
  , initial: PointerEvents
  , other: Value -> PointerEvents
  }

type alias PointerEventsDescriptor = PointerEventsFactory -> PointerEvents

pointerEventsFactory : PointerEventsFactory
pointerEventsFactory =
  {
    pointerEvents str = PointerEvents str
  , auto = AutoPointerEvents
  , inherit = InheritPointerEvents
  , initial = InitialPointerEvents
  , other val = OtherPointerEvents val
  }

pointerEventsValueFactory : ValueFactory PointerEvents
pointerEventsValueFactory =
  { value pointerEvts =
      case pointerEvts of
        PointerEvents num -> stringValueFactory.value num
        AutoPointerEvents -> autoValueFactory.auto
        InheritPointerEvents -> inheritValueFactory.inherit
        InitialPointerEvents -> initialValueFactory.initial
        OtherPointerEvents value -> otherValueFactory.other value
  }

pointerEvents : PointerEventsDescriptor -> CssGenerator PointerEvents
pointerEvents descriptor =
  let pointerEvts = descriptor pointerEventsFactory
  in key (stringKey "pointer-events") pointerEvts pointerEventsValueFactory

visiblePainted : PointerEventsDescriptor
visiblePainted factory = factory.pointerEvents "visiblePainted"

visibleFill : PointerEventsDescriptor
visibleFill factory = factory.pointerEvents "visibleFill"

visibleStroke : PointerEventsDescriptor
visibleStroke factory = factory.pointerEvents "visibleStroke"

painted : PointerEventsDescriptor
painted factory = factory.pointerEvents "painted"

fillEvents : PointerEventsDescriptor
fillEvents factory = factory.pointerEvents "fill"

strokeEvents : PointerEventsDescriptor
strokeEvents factory = factory.pointerEvents "stroke"

allEvents : PointerEventsDescriptor
allEvents factory = factory.pointerEvents "all"

-------------------------------------------------------------------------------
{- TODO (with notes)
-- baseline|length|   sub|super|top|text-top|middle|bottom|text-bottom|  initial|inherit
-- length	Raises or lower an element by the specified length. Negative values are allowed
-- %	Raises or lower an element in a percent of the "line-height" property. Negative values are allowed

type VerticalAlignValue a
  = VerticalAlignValue Value
  | BaselineVerticalAlign
  | InitialVerticalAlign
  | InheritVerticalAlign
  | OtherVerticalAlign Value

instance VerticalAlign (VerticalAlignValue a)
instance VerticalAlign (Size a)

-- you want either a SizeDescriptor or a VerticalAlignDescriptor
-- SizeDescriptor = SizeFactory a -> Size a
-- SizeFactory a =
--   { size: Value -> Size a
--   , auto: Size a
--   , normal: Size a
--   , inherit: Size a
--   , none: Size a
--   , other: Value -> Size a
--   }
VerticalAlignDescriptor = VerticalAlignFactory a -> VerticalAlignValue a
-- You want to take a SizeDescriptor a but only want a SizeFactory a like this:
-- SizeFactory a =
--   { size: Value -> Size a }

{-
  We want VerticalAlign to take a SizeDescriptor or a VerticalAlignDescriptor
  So we make it generic in a - we have verticalAlign a : a -> CssGenerator VerticalAlignValue a
  Now how do we get a VerticalAlignValue from an a?

  from a SizeDescriptor we need to call it with a SizeFactory
    this SizeFactory can have only one function: { size: Value -> Size a }
    then the result we have to wrap in a VerticalAlignValue Size
  from a VerticalAlignDescriptor we need to call it with a VerticalAlignFactory
    this will give us the VerticalAlignValue
  so verticalAlign a has to take something that will take an a and give a VerticalAlignValue a
    record of functions will have a size function and a valign function?

we need size to be a function size: Value -> a
  so it doesn't always produce a Size

VerticalAlignFactory a
  valignSize : SizeDescriptor a -> VerticalAlignValue a
  valign: VerticalAlignDescriptor -> VerticalAlignValue

suppose Sized is generic, like None
    so type alias Sized a = { size: a }

-}

verticalAlign : a -> CssGenerator VerticalAlignValue a
verticalAlign = key "vertical-align"

middle : VerticalAlignValue Value
middle = VerticalAlignValue "middle"

vAlignSub : VerticalAlignValue Value
vAlignSub = VerticalAlignValue "sub"

vAlignSuper : VerticalAlignValue Value
vAlignSuper = VerticalAlignValue "super"

textTop : VerticalAlignValue Value
textTop = VerticalAlignValue "text-top"

textBottom : VerticalAlignValue Value
textBottom = VerticalAlignValue "text-bottom"

vAlignTop : VerticalAlignValue Value
vAlignTop = VerticalAlignValue "top"

vAlignBottom : VerticalAlignValue Value
vAlignBottom = VerticalAlignValue "bottom"

-------------------------------------------------------------------------------

class (Val a) => Cursor a where
    cursor : a -> CssGenerator
    cursor = key "cursor"

type CursorValue a
  = CursorValue Value
  | InheritCursorValue
  | AutoCursorValue

instance Cursor (CursorValue a)

crosshair : CursorValue Value
crosshair = CursorValue "crosshair"

cursorDefault : CursorValue Value
cursorDefault = CursorValue "cursorDefault"

pointer : CursorValue Value
pointer = CursorValue "pointer"

move : CursorValue Value
move = CursorValue "move"

eResize : CursorValue Value
eResize = CursorValue "e-resize"

neResize : CursorValue Value
neResize = CursorValue "ne-resize"

nwResize : CursorValue Value
nwResize = CursorValue "nw-resize"

nResize : CursorValue Value
nResize = CursorValue "n-resize"

seResize : CursorValue Value
seResize = CursorValue "se-resize"

swResize : CursorValue Value
swResize = CursorValue "sw-resize"

sResize : CursorValue Value
sResize = CursorValue "sResize"

wResize : CursorValue Value
wResize = CursorValue "sResize"

cursorText : CursorValue Value
cursorText = CursorValue "text"

wait : CursorValue Value
wait = CursorValue "wait"

cursorProgress : CursorValue Value
cursorProgress = CursorValue "progress"

help : CursorValue Value
help = CursorValue "help"

cursorUrl : Text -> CursorValue Value
cursorUrl u = CursorValue $ value ("url(\"" <> u <> "\")")

-}
