module Css.Pointer (

  -- * Cursor

  , aliasCursor, allScroll, cell, contextMenu, colResize, copy, crosshair
  , defaultCursor, eResize, ewResize, grab, grabbing, help, move, nResize
  , neResize, neswResize, nsResize, nwResize, nwseResize, noDrop, notAllowed
  , pointer, progress, rowResize, sResize, seResize, swResize, textCursor
  , cursorUrl, vTextCursor, wResize, wait, zoomIn, zoomOut

  -- * Pointer-events

  , pointerEvents
  , visiblePainted, visibleFill, visibleStroke, painted
  , fillEvents, strokeEvents, allEvents
  
) where

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
