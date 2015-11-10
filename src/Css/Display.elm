module Css.Display (
  -- * Float
    float
  , floatLeft, floatRight

  , clear
  , both , clearLeft, clearRight

  -- * Position

  , position
  , static, absolute, fixed, relative

  -- * Display

  , display
  , inline, block, listItem, runIn, inlineBlock, table, inlineTable, tableRowGroup
  , tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn
  , tableCell, tableCaption, displayNone, displayInherit, flex
  , inlineFlex, grid, inlineGrid

  -- * Overlow

  , scroll
  , overflow, overflowX, overflowY

  -- * Visibility

  , collapse, separate
  , visibility

  -- Clipping

  , clip
  , rect

  -- * Opacity

  , opacity

  -- * Z-index

  , zIndex

  -- * Pointer-events

  , pointerEvents
  , visiblePainted, visibleFill, visibleStroke, painted
  , fillEvents, strokeEvents, allEvents

  -- * Vertical align

  , middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom

  -- * Cursor

  , aliasCursor, allScroll, cell, contextMenu, colResize, copy, crosshair
  , defaultCursor, eResize, ewResize, grab, grabbing, help, move, nResize, neResize
  , neswResize, nsResize, nwResize, nwseResize, noDrop, notAllowed, pointer, progress
  , rowResize, sResize, seResize, swResize, textCursor, cursorUrl, vTextCursor
  , wResize, wait, zoomIn, zoomOut
  ) where

import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)
import Css.Internal.Size exposing (Size, SizeDescriptor, sizeFactory)
import Css.Internal.Display exposing (..) 

-------------------------------------------------------------------------------

float : FloatStyleDescriptor -> PropertyRuleAppender
float descriptor =
  let floatStyle = descriptor floatStyleFactory
  in simpleProperty "float" floatStyle

floatLeft : FloatStyleDescriptor
floatLeft factory = factory.floatStyle "left"

floatRight : FloatStyleDescriptor
floatRight factory = factory.floatStyle "right"

-------------------------------------------------------------------------------

clear : ClearDescriptor -> PropertyRuleAppender
clear descriptor =
  let clearValue = descriptor clearFactory
  in simpleProperty "clear" clearValue

both : ClearDescriptor
both factory = factory.clear "both"

clearLeft : ClearDescriptor
clearLeft factory = factory.clear "left"

clearRight : ClearDescriptor
clearRight factory = factory.clear "right"

-------------------------------------------------------------------------------

position : PositionDescriptor -> PropertyRuleAppender
position descriptor =
  let positionValue = descriptor positionFactory
  in simpleProperty "position" positionValue

static : PositionDescriptor
static factory = factory.position "static"

absolute : PositionDescriptor
absolute factory = factory.position "absolute"

fixed : PositionDescriptor
fixed factory = factory.position "fixed"

relative : PositionDescriptor
relative factory = factory.position "relative"

-------------------------------------------------------------------------------

display : DisplayDescriptor -> PropertyRuleAppender
display descriptor =
  let displayValue = descriptor displayFactory
  in simpleProperty "display" displayValue

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

overflow : OverflowDescriptor -> PropertyRuleAppender
overflow descriptor =
  let overflowValue = descriptor overflowFactory
  in simpleProperty "overflow" overflowValue

overflowX : OverflowDescriptor -> PropertyRuleAppender
overflowX descriptor =
  let overflowValue = descriptor overflowFactory
  in simpleProperty "overflow-x" overflowValue

overflowY : OverflowDescriptor -> PropertyRuleAppender
overflowY descriptor =
  let overflowValue = descriptor overflowFactory
  in simpleProperty "overflow-y" overflowValue

scroll : OverflowDescriptor
scroll factory = factory.overflow "scroll"

-------------------------------------------------------------------------------

visibility : VisibilityDescriptor -> PropertyRuleAppender
visibility descriptor =
  let visibilityValue = descriptor visibilityFactory
  in simpleProperty "visibility" visibilityValue

separate : VisibilityDescriptor
separate factory = factory.visibility "separate"

collapse : VisibilityDescriptor
collapse factory = factory.visibility "collapse"

-------------------------------------------------------------------------------

clip : ClipDescriptor a b c d -> PropertyRuleAppender
clip descriptor =
  let clipValue = descriptor clipFactory
  in simpleProperty "clip" clipValue

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

opacity : OpacityDescriptor -> PropertyRuleAppender
opacity descriptor =
  let opacityValue = descriptor opacityFactory
  in simpleProperty "opacity" opacityValue

pctOpacity : Float -> OpacityDescriptor
pctOpacity level factory = factory.opacity level

-------------------------------------------------------------------------------

zIndex : ZIndexDescriptor -> PropertyRuleAppender
zIndex descriptor =
  let zIndexValue = descriptor zIndexFactory
  in simpleProperty "z-index" zIndexValue

zLevel : Int -> ZIndexDescriptor
zLevel num factory = factory.zIndex num

-------------------------------------------------------------------------------

pointerEvents : PointerEventsDescriptor -> PropertyRuleAppender
pointerEvents descriptor =
  let pointerEventsValue = descriptor pointerEventsFactory
  in simpleProperty "pointer-events" pointerEventsValue

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

verticalAlign : VerticalAlignDescriptor -> PropertyRuleAppender
verticalAlign descriptor =
  let verticalAlignValue = descriptor verticalAlignFactory
  in simpleProperty "vertical-align" verticalAlignValue

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

cursor : CursorDescriptor -> PropertyRuleAppender
cursor descriptor =
  let cursorValue = descriptor cursorFactory
  in simpleProperty "cursor" cursorValue

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
