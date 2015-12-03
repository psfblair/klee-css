module Css.Layout (

  -- * Box-sizing.
    
    boxSizing
  , paddingBox, borderBox, contentBox

  -- * Clear
  
  , clear
  , both , clearLeft, clearRight

  -- Clipping

  , clip

  -- * Display

  , display
  , inline, block, listItem, runIn, inlineBlock, table, inlineTable, tableRowGroup
  , tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn
  , tableCell, tableCaption, displayNone, displayInherit, flex
  , inlineFlex, grid, inlineGrid

  -- * Float
  
  , float
  , floatLeft, floatRight

  -- * Opacity

  , opacity

  -- * Overflow

  , scroll
  , overflow, overflowX, overflowY

  -- * Position

  , position
  , static, absolute, fixed, relative

  -- * Vertical align

  , middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom

  -- * Visibility

  , collapse, separate
  , visibility

  -- * Z-index

  , zIndex

  ) where

import Css.Internal.Layout.BoxSizing as BoxSizing
import Css.Internal.Layout.Clear as Clear
import Css.Internal.Layout.Clip as Clip
import Css.Internal.Layout.Display as Display
import Css.Internal.Layout.Float as Float
import Css.Internal.Layout.Opacity as Opacity
import Css.Internal.Layout.Overflow as Overflow
import Css.Internal.Layout.Position as Position
import Css.Internal.Layout.VerticalAlign as VerticalAlign
import Css.Internal.Layout.Visibility as Visibility
import Css.Internal.Layout.ZIndex as ZIndex
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

boxSizing : BoxSizing.BoxSizingDescriptor -> Stylesheet.PropertyRuleAppender
boxSizing descriptor =
  let boxType = descriptor BoxSizing.boxTypeFactory
  in Stylesheet.simpleProperty "box-sizing" boxType

paddingBox : BoxSizing.BoxTypeDescriptor rec
paddingBox = \factory -> factory.boxType "padding-box"

borderBox : BoxSizing.BoxTypeDescriptor rec
borderBox = \factory ->  factory.boxType "border-box"

contentBox : BoxSizing.BoxTypeDescriptor rec
contentBox = \factory ->  factory.boxType "content-box"

-------------------------------------------------------------------------------

clear : Clear.ClearDescriptor -> Stylesheet.PropertyRuleAppender
clear descriptor =
  let clearValue = descriptor Clear.clearFactory
  in Stylesheet.simpleProperty "clear" clearValue

both : Clear.ClearDescriptor
both factory = factory.clear "both"

clearLeft : Clear.ClearDescriptor
clearLeft factory = factory.clear "left"

clearRight : Clear.ClearDescriptor
clearRight factory = factory.clear "right"

-------------------------------------------------------------------------------
-- TODO The clip property is obsolete; replace with clip-path.
clip : Clip.ClipDescriptor a b c d -> Stylesheet.PropertyRuleAppender
clip descriptor =
  let clipValue = descriptor Clip.clipFactory
  in Stylesheet.simpleProperty "clip" clipValue

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

float : Float.FloatStyleDescriptor -> Stylesheet.PropertyRuleAppender
float descriptor =
  let floatStyle = descriptor Float.floatStyleFactory
  in Stylesheet.simpleProperty "float" floatStyle

floatLeft : Float.FloatStyleDescriptor
floatLeft factory = factory.floatStyle "left"

floatRight : Float.FloatStyleDescriptor
floatRight factory = factory.floatStyle "right"

-------------------------------------------------------------------------------

opacity : Opacity.OpacityDescriptor -> Stylesheet.PropertyRuleAppender
opacity descriptor =
  let opacityValue = descriptor Opacity.opacityFactory
  in Stylesheet.simpleProperty "opacity" opacityValue

pctOpacity : Float -> Opacity.OpacityDescriptor
pctOpacity level factory = factory.opacity level

-------------------------------------------------------------------------------

overflow : Overflow.OverflowDescriptor -> Stylesheet.PropertyRuleAppender
overflow descriptor =
  let overflowValue = descriptor Overflow.overflowFactory
  in Stylesheet.simpleProperty "overflow" overflowValue

overflowX : Overflow.OverflowDescriptor -> Stylesheet.PropertyRuleAppender
overflowX descriptor =
  let overflowValue = descriptor Overflow.overflowFactory
  in Stylesheet.simpleProperty "overflow-x" overflowValue

overflowY : Overflow.OverflowDescriptor -> Stylesheet.PropertyRuleAppender
overflowY descriptor =
  let overflowValue = descriptor Overflow.overflowFactory
  in Stylesheet.simpleProperty "overflow-y" overflowValue

scroll : Overflow.OverflowDescriptor
scroll factory = factory.overflow "scroll"

-------------------------------------------------------------------------------

position : Position.PositionDescriptor -> Stylesheet.PropertyRuleAppender
position descriptor =
  let positionValue = descriptor Position.positionFactory
  in Stylesheet.simpleProperty "position" positionValue

static : Position.PositionDescriptor
static factory = factory.position "static"

absolute : Position.PositionDescriptor
absolute factory = factory.position "absolute"

fixed : Position.PositionDescriptor
fixed factory = factory.position "fixed"

relative : Position.PositionDescriptor
relative factory = factory.position "relative"

-------------------------------------------------------------------------------

verticalAlign : VerticalAlign.VerticalAlignDescriptor {} -> 
                Stylesheet.PropertyRuleAppender
verticalAlign descriptor =
  let verticalAlignValue = descriptor VerticalAlign.verticalAlignFactory
  in Stylesheet.simpleProperty "vertical-align" verticalAlignValue

middle : VerticalAlign.VerticalAlignDescriptor sz
middle factory = factory.vAlign "middle"

vAlignSub : VerticalAlign.VerticalAlignDescriptor sz
vAlignSub factory = factory.vAlign  "sub"

vAlignSuper : VerticalAlign.VerticalAlignDescriptor sz
vAlignSuper factory = factory.vAlign  "super"

textTop : VerticalAlign.VerticalAlignDescriptor sz
textTop factory = factory.vAlign  "text-top"

textBottom : VerticalAlign.VerticalAlignDescriptor sz
textBottom factory = factory.vAlign  "text-bottom"

vAlignTop : VerticalAlign.VerticalAlignDescriptor sz
vAlignTop factory = factory.vAlign  "top"

vAlignBottom : VerticalAlign.VerticalAlignDescriptor sz
vAlignBottom factory = factory.vAlign  "bottom"

-------------------------------------------------------------------------------

visibility : Visibility.ExtendedVisibilityDescriptor {} -> 
             Stylesheet.PropertyRuleAppender
visibility descriptor =
  let visibilityValue = descriptor Visibility.extendedVisibilityFactory
  in Stylesheet.simpleProperty "visibility" visibilityValue

separate : Visibility.VisibilityDescriptor rec
separate = \factory -> factory.visibility "separate"

collapse : Visibility.VisibilityDescriptor rec
collapse = \factory -> factory.visibility "collapse"

-------------------------------------------------------------------------------

zIndex : ZIndex.ZIndexDescriptor -> Stylesheet.PropertyRuleAppender
zIndex descriptor =
  let zIndexValue = descriptor ZIndex.zIndexFactory
  in Stylesheet.simpleProperty "z-index" zIndexValue

zLevel : Int -> ZIndex.ZIndexDescriptor
zLevel num factory = factory.zIndex num
