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
  , defaultCursor, eResize, ewResize, grab, grabbing, help, move, nResize
  , neResize, neswResize, nsResize, nwResize, nwseResize, noDrop, notAllowed
  , pointer, progress, rowResize, sResize, seResize, swResize, textCursor
  , cursorUrl, vTextCursor, wResize, wait, zoomIn, zoomOut
  ) where

import Css.Internal.Display as Display
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

float : Display.FloatStyleDescriptor -> Stylesheet.PropertyRuleAppender
float descriptor =
  let floatStyle = descriptor Display.floatStyleFactory
  in Stylesheet.simpleProperty "float" floatStyle

floatLeft : Display.FloatStyleDescriptor
floatLeft factory = factory.floatStyle "left"

floatRight : Display.FloatStyleDescriptor
floatRight factory = factory.floatStyle "right"

-------------------------------------------------------------------------------

clear : Display.ClearDescriptor -> Stylesheet.PropertyRuleAppender
clear descriptor =
  let clearValue = descriptor Display.clearFactory
  in Stylesheet.simpleProperty "clear" clearValue

both : Display.ClearDescriptor
both factory = factory.clear "both"

clearLeft : Display.ClearDescriptor
clearLeft factory = factory.clear "left"

clearRight : Display.ClearDescriptor
clearRight factory = factory.clear "right"

-------------------------------------------------------------------------------

position : Display.PositionDescriptor -> Stylesheet.PropertyRuleAppender
position descriptor =
  let positionValue = descriptor Display.positionFactory
  in Stylesheet.simpleProperty "position" positionValue

static : Display.PositionDescriptor
static factory = factory.position "static"

absolute : Display.PositionDescriptor
absolute factory = factory.position "absolute"

fixed : Display.PositionDescriptor
fixed factory = factory.position "fixed"

relative : Display.PositionDescriptor
relative factory = factory.position "relative"

-------------------------------------------------------------------------------

display : Display.DisplayDescriptor -> Stylesheet.PropertyRuleAppender
display descriptor =
  let displayValue = descriptor Display.displayFactory
  in Stylesheet.simpleProperty "display" displayValue

inline : Display.DisplayDescriptor
inline factory = factory.display "inline"

block : Display.DisplayDescriptor
block factory = factory.display "block"

listItem : Display.DisplayDescriptor
listItem factory = factory.display "list-item"

runIn : Display.DisplayDescriptor
runIn factory = factory.display "runIn"

inlineBlock : Display.DisplayDescriptor
inlineBlock factory = factory.display "inline-block"

table : Display.DisplayDescriptor
table factory = factory.display "table"

inlineTable : Display.DisplayDescriptor
inlineTable factory = factory.display "inline-table"

tableRowGroup : Display.DisplayDescriptor
tableRowGroup factory = factory.display "table-row-Group"

tableHeaderGroup : Display.DisplayDescriptor
tableHeaderGroup factory = factory.display "table-header-group"

tableFooterGroup : Display.DisplayDescriptor
tableFooterGroup factory = factory.display "table-footer-group"

tableRow : Display.DisplayDescriptor
tableRow factory = factory.display "table-row"

tableColumnGroup : Display.DisplayDescriptor
tableColumnGroup factory = factory.display "table-column-group"

tableColumn : Display.DisplayDescriptor
tableColumn factory = factory.display "table-column"

tableCell : Display.DisplayDescriptor
tableCell factory = factory.display "table-cell"

tableCaption : Display.DisplayDescriptor
tableCaption factory = factory.display "table-caption"

displayNone : Display.DisplayDescriptor
displayNone factory = factory.display "none"

displayInherit : Display.DisplayDescriptor
displayInherit factory = factory.display "inherit"

flex : Display.DisplayDescriptor
flex factory = factory.display "flex"

inlineFlex : Display.DisplayDescriptor
inlineFlex factory = factory.display "inline-flex"

grid : Display.DisplayDescriptor
grid factory = factory.display "grid"

inlineGrid : Display.DisplayDescriptor
inlineGrid factory = factory.display "inline-grid"

-------------------------------------------------------------------------------

overflow : Display.OverflowDescriptor -> Stylesheet.PropertyRuleAppender
overflow descriptor =
  let overflowValue = descriptor Display.overflowFactory
  in Stylesheet.simpleProperty "overflow" overflowValue

overflowX : Display.OverflowDescriptor -> Stylesheet.PropertyRuleAppender
overflowX descriptor =
  let overflowValue = descriptor Display.overflowFactory
  in Stylesheet.simpleProperty "overflow-x" overflowValue

overflowY : Display.OverflowDescriptor -> Stylesheet.PropertyRuleAppender
overflowY descriptor =
  let overflowValue = descriptor Display.overflowFactory
  in Stylesheet.simpleProperty "overflow-y" overflowValue

scroll : Display.OverflowDescriptor
scroll factory = factory.overflow "scroll"

-------------------------------------------------------------------------------

visibility : Display.ExtendedVisibilityDescriptor {} -> 
             Stylesheet.PropertyRuleAppender
visibility descriptor =
  let visibilityValue = descriptor Display.extendedVisibilityFactory
  in Stylesheet.simpleProperty "visibility" visibilityValue

separate : Display.VisibilityDescriptor rec
separate = \factory -> factory.visibility "separate"

collapse : Display.VisibilityDescriptor rec
collapse = \factory -> factory.visibility "collapse"

-------------------------------------------------------------------------------
--TODO Remove; use clip-path instead
clip : Display.ClipDescriptor a b c d -> Stylesheet.PropertyRuleAppender
clip descriptor =
  let clipValue = descriptor Display.clipFactory
  in Stylesheet.simpleProperty "clip" clipValue

-------------------------------------------------------------------------------

opacity : Display.OpacityDescriptor -> Stylesheet.PropertyRuleAppender
opacity descriptor =
  let opacityValue = descriptor Display.opacityFactory
  in Stylesheet.simpleProperty "opacity" opacityValue

pctOpacity : Float -> Display.OpacityDescriptor
pctOpacity level factory = factory.opacity level

-------------------------------------------------------------------------------

zIndex : Display.ZIndexDescriptor -> Stylesheet.PropertyRuleAppender
zIndex descriptor =
  let zIndexValue = descriptor Display.zIndexFactory
  in Stylesheet.simpleProperty "z-index" zIndexValue

zLevel : Int -> Display.ZIndexDescriptor
zLevel num factory = factory.zIndex num

-------------------------------------------------------------------------------

pointerEvents : Display.PointerEventsDescriptor -> 
                Stylesheet.PropertyRuleAppender
pointerEvents descriptor =
  let pointerEventsValue = descriptor Display.pointerEventsFactory
  in Stylesheet.simpleProperty "pointer-events" pointerEventsValue

visiblePainted : Display.PointerEventsDescriptor
visiblePainted factory = factory.pointerEvents "visiblePainted"

visibleFill : Display.PointerEventsDescriptor
visibleFill factory = factory.pointerEvents "visibleFill"

visibleStroke : Display.PointerEventsDescriptor
visibleStroke factory = factory.pointerEvents "visibleStroke"

painted : Display.PointerEventsDescriptor
painted factory = factory.pointerEvents "painted"

fillEvents : Display.PointerEventsDescriptor
fillEvents factory = factory.pointerEvents "fill"

strokeEvents : Display.PointerEventsDescriptor
strokeEvents factory = factory.pointerEvents "stroke"

allEvents : Display.PointerEventsDescriptor
allEvents factory = factory.pointerEvents "all"

-------------------------------------------------------------------------------

verticalAlign : Display.VerticalAlignDescriptor {} -> 
                Stylesheet.PropertyRuleAppender
verticalAlign descriptor =
  let verticalAlignValue = descriptor Display.verticalAlignFactory
  in Stylesheet.simpleProperty "vertical-align" verticalAlignValue

middle : Display.VerticalAlignDescriptor sz
middle factory = factory.vAlign "middle"

vAlignSub : Display.VerticalAlignDescriptor sz
vAlignSub factory = factory.vAlign  "sub"

vAlignSuper : Display.VerticalAlignDescriptor sz
vAlignSuper factory = factory.vAlign  "super"

textTop : Display.VerticalAlignDescriptor sz
textTop factory = factory.vAlign  "text-top"

textBottom : Display.VerticalAlignDescriptor sz
textBottom factory = factory.vAlign  "text-bottom"

vAlignTop : Display.VerticalAlignDescriptor sz
vAlignTop factory = factory.vAlign  "top"

vAlignBottom : Display.VerticalAlignDescriptor sz
vAlignBottom factory = factory.vAlign  "bottom"

-------------------------------------------------------------------------------

cursor : Display.CursorDescriptor -> Stylesheet.PropertyRuleAppender
cursor descriptor =
  let cursorValue = descriptor Display.cursorFactory
  in Stylesheet.simpleProperty "cursor" cursorValue

aliasCursor : Display.CursorDescriptor
aliasCursor factory = factory.cursor "alias"

allScroll : Display.CursorDescriptor
allScroll factory = factory.cursor "all-scroll"

cell : Display.CursorDescriptor
cell factory = factory.cursor "cell"

contextMenu : Display.CursorDescriptor
contextMenu factory = factory.cursor "context-menu"

colResize : Display.CursorDescriptor
colResize factory = factory.cursor "col-resize"

copy : Display.CursorDescriptor
copy factory = factory.cursor "copy"

crosshair : Display.CursorDescriptor
crosshair factory = factory.cursor "crosshair"

defaultCursor : Display.CursorDescriptor
defaultCursor factory = factory.cursor "default"

eResize : Display.CursorDescriptor
eResize factory = factory.cursor "e-resize"

ewResize : Display.CursorDescriptor
ewResize factory = factory.cursor "ew-resize"

grab : Display.CursorDescriptor
grab factory = factory.cursor "grab"

grabbing : Display.CursorDescriptor
grabbing factory = factory.cursor "grabbing"

help : Display.CursorDescriptor
help factory = factory.cursor "help"

move : Display.CursorDescriptor
move factory = factory.cursor "move"

nResize : Display.CursorDescriptor
nResize factory = factory.cursor "n-resize"

neResize : Display.CursorDescriptor
neResize factory = factory.cursor "ne-resize"

neswResize : Display.CursorDescriptor
neswResize factory = factory.cursor "nesw-resize"

nsResize : Display.CursorDescriptor
nsResize factory = factory.cursor "ns-resize"

nwResize : Display.CursorDescriptor
nwResize factory = factory.cursor "nw-resize"

nwseResize : Display.CursorDescriptor
nwseResize factory = factory.cursor "nwse-resize"

noDrop : Display.CursorDescriptor
noDrop factory = factory.cursor "no-drop"

notAllowed : Display.CursorDescriptor
notAllowed factory = factory.cursor "not-allowed"

pointer : Display.CursorDescriptor
pointer factory = factory.cursor "pointer"

progress : Display.CursorDescriptor
progress factory = factory.cursor "progress"

rowResize : Display.CursorDescriptor
rowResize factory = factory.cursor "row-resize"

sResize : Display.CursorDescriptor
sResize factory = factory.cursor "sResize"

seResize : Display.CursorDescriptor
seResize factory = factory.cursor "se-resize"

swResize : Display.CursorDescriptor
swResize factory = factory.cursor "sw-resize"

textCursor : Display.CursorDescriptor
textCursor factory = factory.cursor "text"

cursorUrl : String -> Display.CursorDescriptor
cursorUrl url factory =
  let urlexpr =  "url(\"" ++ url  ++ "\")"
  in factory.cursor urlexpr

vTextCursor : Display.CursorDescriptor
vTextCursor factory = factory.cursor "vertical-text"

wResize : Display.CursorDescriptor
wResize factory = factory.cursor "sResize"

wait : Display.CursorDescriptor
wait factory = factory.cursor "wait"

zoomIn : Display.CursorDescriptor
zoomIn factory = factory.cursor "zoom-in"

zoomOut : Display.CursorDescriptor
zoomOut factory = factory.cursor "zoom-out"
