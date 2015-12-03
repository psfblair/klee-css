module Css.Pointer (

  -- * Cursor

    cursor
    
  , cursorUrl
    
  , aliasCursor, allScroll, cell, contextMenu, colResize, copy, crosshair
  , defaultCursor, eResize, ewResize, grab, grabbing, help, move, nResize
  , neResize, neswResize, nsResize, nwResize, nwseResize, noDrop, notAllowed
  , pointer, progress, rowResize, sResize, seResize, swResize, textCursor
  , vTextCursor, wResize, wait, zoomIn, zoomOut

  -- * Pointer-events

  , pointerEvents
  
  , visiblePainted, visibleFill, visibleStroke, painted
  , fillEvents, strokeEvents, allEvents
  
  ) where

import Css.Internal.Pointer.Cursor as Cursor
import Css.Internal.Pointer.Events as Events
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

cursor : Cursor.CursorDescriptor -> Stylesheet.PropertyRuleAppender
cursor descriptor =
  let cursorValue = descriptor Cursor.cursorFactory
  in Stylesheet.simpleProperty "cursor" cursorValue

cursorUrl : String -> Cursor.CursorDescriptor
cursorUrl url factory =
  let urlexpr =  "url(\"" ++ url  ++ "\")"
  in factory.cursor urlexpr

aliasCursor : Cursor.CursorDescriptor
aliasCursor = \factory -> factory.cursor "alias"

allScroll : Cursor.CursorDescriptor
allScroll = \factory -> factory.cursor "all-scroll"

cell : Cursor.CursorDescriptor
cell = \factory -> factory.cursor "cell"

contextMenu : Cursor.CursorDescriptor
contextMenu = \factory -> factory.cursor "context-menu"

colResize : Cursor.CursorDescriptor
colResize = \factory -> factory.cursor "col-resize"

copy : Cursor.CursorDescriptor
copy = \factory -> factory.cursor "copy"

crosshair : Cursor.CursorDescriptor
crosshair = \factory -> factory.cursor "crosshair"

defaultCursor : Cursor.CursorDescriptor
defaultCursor = \factory -> factory.cursor "default"

eResize : Cursor.CursorDescriptor
eResize = \factory -> factory.cursor "e-resize"

ewResize : Cursor.CursorDescriptor
ewResize = \factory -> factory.cursor "ew-resize"

grab : Cursor.CursorDescriptor
grab = \factory -> factory.cursor "grab"

grabbing : Cursor.CursorDescriptor
grabbing = \factory -> factory.cursor "grabbing"

help : Cursor.CursorDescriptor
help = \factory -> factory.cursor "help"

move : Cursor.CursorDescriptor
move = \factory -> factory.cursor "move"

nResize : Cursor.CursorDescriptor
nResize = \factory -> factory.cursor "n-resize"

neResize : Cursor.CursorDescriptor
neResize = \factory -> factory.cursor "ne-resize"

neswResize : Cursor.CursorDescriptor
neswResize = \factory -> factory.cursor "nesw-resize"

nsResize : Cursor.CursorDescriptor
nsResize = \factory -> factory.cursor "ns-resize"

nwResize : Cursor.CursorDescriptor
nwResize = \factory -> factory.cursor "nw-resize"

nwseResize : Cursor.CursorDescriptor
nwseResize = \factory -> factory.cursor "nwse-resize"

noDrop : Cursor.CursorDescriptor
noDrop = \factory -> factory.cursor "no-drop"

notAllowed : Cursor.CursorDescriptor
notAllowed = \factory -> factory.cursor "not-allowed"

pointer : Cursor.CursorDescriptor
pointer = \factory -> factory.cursor "pointer"

progress : Cursor.CursorDescriptor
progress = \factory -> factory.cursor "progress"

rowResize : Cursor.CursorDescriptor
rowResize = \factory -> factory.cursor "row-resize"

sResize : Cursor.CursorDescriptor
sResize = \factory -> factory.cursor "s-resize"

seResize : Cursor.CursorDescriptor
seResize = \factory -> factory.cursor "se-resize"

swResize : Cursor.CursorDescriptor
swResize = \factory -> factory.cursor "sw-resize"

textCursor : Cursor.CursorDescriptor
textCursor = \factory -> factory.cursor "text"

vTextCursor : Cursor.CursorDescriptor
vTextCursor = \factory -> factory.cursor "vertical-text"

wResize : Cursor.CursorDescriptor
wResize = \factory -> factory.cursor "w-resize"

wait : Cursor.CursorDescriptor
wait = \factory -> factory.cursor "wait"

zoomIn : Cursor.CursorDescriptor
zoomIn = \factory -> factory.cursor "zoom-in"

zoomOut : Cursor.CursorDescriptor
zoomOut = \factory -> factory.cursor "zoom-out"

-------------------------------------------------------------------------------

pointerEvents : Events.PointerEventsDescriptor -> 
                Stylesheet.PropertyRuleAppender
pointerEvents descriptor =
  let pointerEventsValue = descriptor Events.pointerEventsFactory
  in Stylesheet.simpleProperty "pointer-events" pointerEventsValue

visiblePainted : Events.PointerEventsDescriptor
visiblePainted = \factory -> factory.pointerEvents "visiblePainted"

visibleFill : Events.PointerEventsDescriptor
visibleFill = \factory -> factory.pointerEvents "visibleFill"

visibleStroke : Events.PointerEventsDescriptor
visibleStroke = \factory -> factory.pointerEvents "visibleStroke"

painted : Events.PointerEventsDescriptor
painted = \factory -> factory.pointerEvents "painted"

fillEvents : Events.PointerEventsDescriptor
fillEvents = \factory -> factory.pointerEvents "fill"

strokeEvents : Events.PointerEventsDescriptor
strokeEvents = \factory -> factory.pointerEvents "stroke"

allEvents : Events.PointerEventsDescriptor
allEvents = \factory -> factory.pointerEvents "all"
