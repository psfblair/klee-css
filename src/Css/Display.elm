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

  , VerticalAlign(..)
  , middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom

  -- * Cursor

  , Cursor(..)
  , aliasCursor, allScroll, cell, contextMenu, colResize, copy, crosshair
  , defaultCursor, eResize, ewResize, grab, grabbing, help, move, nResize, neResize
  , neswResize, nsResize, nwResize, nwseResize, noDrop, notAllowed, pointer, progress
  , rowResize, sResize, seResize, swResize, textCursor, cursorUrl, vTextCursor
  , wResize, wait, zoomIn, zoomOut

  -- Used by other modules
  , VisibilityDescriptor, visibilityFactory, visibilityValueFactory

  ) where

import Css.Internal.Property exposing (Value, ValueFactory, concatenateValues, stringKey
  , stringValueFactory, intValueFactory, floatValueFactory, commaQuadrupleValueFactory
  )
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, key)

import Css.Common exposing (
    Auto, Inherit, None, Visible, Hidden, Other
  , autoValue, baselineValue, inheritValue, initialValue
  , noneValue, visibleValue, hiddenValue, otherValue
  )
import Css.Size exposing (Size, SizeDescriptor, sizeFactory, sizeValueFactory)

-------------------------------------------------------------------------------

type FloatStyle
  = FloatStyle String
  | NoFloat
  | InheritFloat
  | InitialFloat
  | OtherFloat String

type alias FloatStyleDescriptor = FloatStyleFactory -> FloatStyle

float : FloatStyleDescriptor -> PropertyRuleAppender
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
  | OtherClear String

type alias ClearDescriptor = ClearFactory -> Clear

clear : ClearDescriptor -> PropertyRuleAppender
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
  | OtherPosition String

type alias PositionDescriptor = PositionFactory -> Position

position : PositionDescriptor -> PropertyRuleAppender
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
  | OtherDisplay String

type alias DisplayDescriptor = DisplayFactory -> Display

display : DisplayDescriptor -> PropertyRuleAppender
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
  | OtherOverflow String

type alias OverflowDescriptor = OverflowFactory -> Overflow

overflow : OverflowDescriptor -> PropertyRuleAppender
overflow descriptor =
  let overflowValue = descriptor overflowFactory
  in key (stringKey "overflow") overflowValue overflowValueFactory

overflowX : OverflowDescriptor -> PropertyRuleAppender
overflowX descriptor =
  let overflowValue = descriptor overflowFactory
  in key (stringKey "overflow-x") overflowValue overflowValueFactory

overflowY : OverflowDescriptor -> PropertyRuleAppender
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
  | OtherVisibility String

type alias VisibilityDescriptor = VisibilityFactory -> Visibility

visibility : VisibilityDescriptor -> PropertyRuleAppender
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
  | OtherClip String

type alias ClipDescriptor a b c d = ClipFactory a b c d -> Clip a b c d

clip : ClipDescriptor a b c d -> PropertyRuleAppender
clip descriptor =
  let clipValue = descriptor clipFactory
  in key (stringKey "clip") clipValue clipValueFactory

rect : SizeDescriptor (Size a) a ->
       SizeDescriptor (Size b) b ->
       SizeDescriptor (Size c) c ->
       SizeDescriptor (Size d) d ->
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
  | OtherOpacity String

type alias OpacityDescriptor = OpacityFactory -> Opacity

opacity : OpacityDescriptor -> PropertyRuleAppender
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
  | OtherZIndex String

type alias ZIndexDescriptor = ZIndexFactory -> ZIndex

zIndex : ZIndexDescriptor -> PropertyRuleAppender
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
  | OtherPointerEvents String

type alias PointerEventsDescriptor = PointerEventsFactory -> PointerEvents

pointerEvents : PointerEventsDescriptor -> PropertyRuleAppender
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

type VerticalAlign
  = VerticalAlign Value
  | BaselineVerticalAlign
  | InitialVerticalAlign
  | InheritVerticalAlign
  | OtherVerticalAlign String

type alias VerticalAlignDescriptor = VerticalAlignFactory -> VerticalAlign

verticalAlign : VerticalAlignDescriptor -> PropertyRuleAppender
verticalAlign descriptor =
  let valign = descriptor verticalAlignFactory
  in key (stringKey "vertical-align") valign verticalAlignValueFactory

middle : VerticalAlignDescriptor
middle factory = factory.vAlign "middle"

vAlignSub : VerticalAlignDescriptor
vAlignSub factory = factory.vAlign  "sub"

vAlignSuper : VerticalAlignDescriptor
vAlignSuper factory = factory.vAlign  "super"

textTop : VerticalAlignDescriptor
textTop factory = factory.vAlign  "text-top"

textBottom : VerticalAlignDescriptor
textBottom factory = factory.vAlign  "text-bottom"

vAlignTop : VerticalAlignDescriptor
vAlignTop factory = factory.vAlign  "top"

vAlignBottom : VerticalAlignDescriptor
vAlignBottom factory = factory.vAlign  "bottom"

-------------------------------------------------------------------------------

type Cursor
  = Cursor String
  | AutoCursor
  | NoCursor
  | InheritCursor
  | InitialCursor
  | OtherCursor String

type alias CursorDescriptor = CursorFactory -> Cursor

cursor : CursorDescriptor -> PropertyRuleAppender
cursor descriptor =
  let cursorValue = descriptor cursorFactory
  in key (stringKey "cursor") cursorValue cursorValueFactory

aliasCursor : CursorDescriptor
aliasCursor factory = factory.cursor "alias"

allScroll : CursorDescriptor
allScroll factory = factory.cursor "all-scroll"

cell : CursorDescriptor
cell factory = factory.cursor "cell"

contextMenu : CursorDescriptor
contextMenu factory = factory.cursor "context-menu"

colResize : CursorDescriptor
colResize factory = factory.cursor "col-resize"

copy : CursorDescriptor
copy factory = factory.cursor "copy"

crosshair : CursorDescriptor
crosshair factory = factory.cursor "crosshair"

defaultCursor : CursorDescriptor
defaultCursor factory = factory.cursor "default"

eResize : CursorDescriptor
eResize factory = factory.cursor "e-resize"

ewResize : CursorDescriptor
ewResize factory = factory.cursor "ew-resize"

grab : CursorDescriptor
grab factory = factory.cursor "grab"

grabbing : CursorDescriptor
grabbing factory = factory.cursor "grabbing"

help : CursorDescriptor
help factory = factory.cursor "help"

move : CursorDescriptor
move factory = factory.cursor "move"

nResize : CursorDescriptor
nResize factory = factory.cursor "n-resize"

neResize : CursorDescriptor
neResize factory = factory.cursor "ne-resize"

neswResize : CursorDescriptor
neswResize factory = factory.cursor "nesw-resize"

nsResize : CursorDescriptor
nsResize factory = factory.cursor "ns-resize"

nwResize : CursorDescriptor
nwResize factory = factory.cursor "nw-resize"

nwseResize : CursorDescriptor
nwseResize factory = factory.cursor "nwse-resize"

noDrop : CursorDescriptor
noDrop factory = factory.cursor "no-drop"

notAllowed : CursorDescriptor
notAllowed factory = factory.cursor "not-allowed"

pointer : CursorDescriptor
pointer factory = factory.cursor "pointer"

progress : CursorDescriptor
progress factory = factory.cursor "progress"

rowResize : CursorDescriptor
rowResize factory = factory.cursor "row-resize"

sResize : CursorDescriptor
sResize factory = factory.cursor "sResize"

seResize : CursorDescriptor
seResize factory = factory.cursor "se-resize"

swResize : CursorDescriptor
swResize factory = factory.cursor "sw-resize"

textCursor : CursorDescriptor
textCursor factory = factory.cursor "text"

cursorUrl : String -> CursorDescriptor
cursorUrl url factory =
  let urlexpr =  "url(\"" ++ url  ++ "\")"
  in factory.cursor urlexpr

vTextCursor : CursorDescriptor
vTextCursor factory = factory.cursor "vertical-text"

wResize : CursorDescriptor
wResize factory = factory.cursor "sResize"

wait : CursorDescriptor
wait factory = factory.cursor "wait"

zoomIn : CursorDescriptor
zoomIn factory = factory.cursor "zoom-in"

zoomOut : CursorDescriptor
zoomOut factory = factory.cursor "zoom-out"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Ancillary types used for implementation. These substitute for Clay's typeclasses.

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
-------------------------------------------------------------------------------

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
