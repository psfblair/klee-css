module Css.Border (
  -- * Stroke type, used for border-style and outline-style.
  solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

  -- * Border properties.

  , border, borderTop, borderLeft, borderBottom, borderRight
  , borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor, borderColor4
  , borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle, borderStyle4
  
  , medium, thick, thin
  , borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth, borderWidth4

  -- * Outline properties.

  , outline, outlineStyle, outlineWidth, outlineOffset, outlineColor

  -- * Border radius.

  , borderRadius
  , borderTopLeftRadius, borderTopRightRadius
  , borderBottomLeftRadius, borderBottomRightRadius

  -- * Collapsing borders model for a table
  , borderCollapse
  , borderSpacing, borderSpacing2
  ) where

import Css.Internal.Property exposing
  ( Value, stringValue, spacePairValue
  , spaceTripleValue, spaceQuadrupleValue
  )

import Css.Internal.Display exposing (VisibilityDescriptor, visibilityFactory)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)

import Css.Internal.Border exposing (..)

import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Linear.Absolute as Absolute

-------------------------------------------------------------------------------

solid : StrokeDescriptor
solid factory = factory.stroke "solid"

dotted : StrokeDescriptor
dotted factory = factory.stroke "dotted"

dashed : StrokeDescriptor
dashed factory = factory.stroke "dashed"

double : StrokeDescriptor
double factory = factory.stroke "double"

wavy : StrokeDescriptor
wavy factory = factory.stroke "wavy"

groove : StrokeDescriptor
groove factory = factory.stroke "groove"

ridge : StrokeDescriptor
ridge factory = factory.stroke "ridge"

inset : StrokeDescriptor
inset factory = factory.stroke "inset"

outset : StrokeDescriptor
outset factory = factory.stroke "outset"

-------------------------------------------------------------------------------
-- TODO Fix these shorthand properties - should take generics too
border : StrokeDescriptor -> 
         Linear.SizeDescriptor {} Absolute.Abs -> 
         Color.ColorDescriptor {} -> 
         PropertyRuleAppender
border strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      valueFactory = spaceTripleValue identity sizeDescriptor colorDescriptor
      tuple = (stroke, Linear.nubSizeFactory, Color.nubColorFactory)
  in simpleProperty "border" (valueFactory tuple)

borderTop : StrokeDescriptor -> 
            Linear.SizeDescriptor {} Absolute.Abs -> 
            Color.ColorDescriptor {} -> 
            PropertyRuleAppender
borderTop strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      valueFactory = spaceTripleValue identity widthDescriptor colorDescriptor
      tuple = (stroke, Linear.nubSizeFactory, Color.nubColorFactory)
  in simpleProperty "border-top" (valueFactory tuple)

borderLeft : StrokeDescriptor -> 
             Linear.SizeDescriptor {} Absolute.Abs -> 
             Color.ColorDescriptor {} -> 
             PropertyRuleAppender
borderLeft strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      valueFactory = spaceTripleValue identity widthDescriptor colorDescriptor
      tuple = (stroke, Linear.nubSizeFactory, Color.nubColorFactory)
  in simpleProperty "border-left" (valueFactory tuple)

borderBottom : StrokeDescriptor -> 
               Linear.SizeDescriptor {} Absolute.Abs -> 
               Color.ColorDescriptor {} -> 
               PropertyRuleAppender
borderBottom strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      valueFactory = spaceTripleValue identity widthDescriptor colorDescriptor
      tuple = (stroke, Linear.nubSizeFactory, Color.nubColorFactory)
  in simpleProperty "border-bottom" (valueFactory tuple)

borderRight : StrokeDescriptor -> 
              Linear.SizeDescriptor {} Absolute.Abs -> 
              Color.ColorDescriptor {} -> 
              PropertyRuleAppender
borderRight strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      valueFactory = spaceTripleValue identity widthDescriptor colorDescriptor
      tuple = (stroke, Linear.nubSizeFactory, Color.nubColorFactory)
  in simpleProperty "border-right" (valueFactory tuple)

-------------------------------------------------------------------------------

borderColor : Color.BasicColorDescriptor -> PropertyRuleAppender
borderColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in simpleProperty "border-color" colorValue

borderLeftColor : Color.BasicColorDescriptor -> PropertyRuleAppender
borderLeftColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in simpleProperty "border-left-color" colorValue

borderRightColor : Color.BasicColorDescriptor -> PropertyRuleAppender
borderRightColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in simpleProperty "border-right-color" colorValue

borderTopColor : Color.BasicColorDescriptor -> PropertyRuleAppender
borderTopColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in simpleProperty "border-top-color" colorValue

borderBottomColor : Color.BasicColorDescriptor -> PropertyRuleAppender
borderBottomColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in simpleProperty "border-bottom-color" colorValue

borderColor4 : Color.ColorDescriptor {} -> 
               Color.ColorDescriptor {} -> 
               Color.ColorDescriptor {} -> 
               Color.ColorDescriptor {} -> 
               PropertyRuleAppender
borderColor4 topColor rightColor bottomColor leftColor =
  let colorFactory = Color.nubColorFactory
      -- This looks backwards because the descriptors are the functions that 
      -- construct values and the factories are what those functions are applied to.
      valueFactory = spaceQuadrupleValue topColor rightColor bottomColor leftColor      
      tuple = (colorFactory, colorFactory, colorFactory, colorFactory)
  in simpleProperty "border-color" (valueFactory tuple)

-------------------------------------------------------------------------------

borderStyle : StrokeDescriptor -> PropertyRuleAppender
borderStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-style" style

borderLeftStyle : StrokeDescriptor -> PropertyRuleAppender
borderLeftStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-left-style" style

borderRightStyle : StrokeDescriptor -> PropertyRuleAppender
borderRightStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-right-style" style

borderTopStyle : StrokeDescriptor -> PropertyRuleAppender
borderTopStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-top-style" style

borderBottomStyle : StrokeDescriptor -> PropertyRuleAppender
borderBottomStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-bottom-style" style

borderStyle4 : StrokeDescriptor ->
               StrokeDescriptor ->
               StrokeDescriptor ->
               StrokeDescriptor ->
               PropertyRuleAppender
borderStyle4 strokeDescriptorA strokeDescriptorB strokeDescriptorC strokeDescriptorD =
  let strokeA = strokeDescriptorA strokeFactory
      strokeB = strokeDescriptorB strokeFactory
      strokeC = strokeDescriptorC strokeFactory
      strokeD = strokeDescriptorD strokeFactory
      valueFactory = spaceQuadrupleValue identity identity identity identity
  in simpleProperty "border-style" (valueFactory (strokeA, strokeB, strokeC, strokeD))

-------------------------------------------------------------------------------

medium : BorderWidthDescriptor rec
medium = \factory -> factory.medium
  
thin : BorderWidthDescriptor rec
thin = \factory -> factory.thin

thick : BorderWidthDescriptor rec
thick = \factory -> factory.thick

borderWidth : BasicBorderWidthDescriptor {} -> PropertyRuleAppender
borderWidth sizeDescriptor = 
  simpleProperty "border-width" (sizeDescriptor borderWidthFactory)

borderLeftWidth : BasicBorderWidthDescriptor {} -> PropertyRuleAppender
borderLeftWidth sizeDescriptor =
  simpleProperty "border-left-width" (sizeDescriptor borderWidthFactory)

borderRightWidth : BasicBorderWidthDescriptor {} -> PropertyRuleAppender
borderRightWidth sizeDescriptor =
  simpleProperty "border-right-width" (sizeDescriptor borderWidthFactory)

borderTopWidth : BasicBorderWidthDescriptor {} -> PropertyRuleAppender
borderTopWidth sizeDescriptor =
  simpleProperty "border-top-width" (sizeDescriptor borderWidthFactory)

borderBottomWidth : BasicBorderWidthDescriptor {} -> PropertyRuleAppender
borderBottomWidth sizeDescriptor =
  simpleProperty "border-bottom-width" (sizeDescriptor borderWidthFactory)

borderWidth4 : BorderWidthDescriptor {} ->
               BorderWidthDescriptor {} ->
               BorderWidthDescriptor {} ->
               BorderWidthDescriptor {} ->
               PropertyRuleAppender
borderWidth4 topWidth rightWidth bottomWidth leftWidth =
  let valueFactory = 
        spaceQuadrupleValue topWidth rightWidth bottomWidth leftWidth
      tuple = (nubBorderWidthFactory, nubBorderWidthFactory, nubBorderWidthFactory, nubBorderWidthFactory)
  in simpleProperty "border-width" (valueFactory tuple)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- TODO Fix shorthand property - should not take separate arguments like this
outline : StrokeDescriptor -> 
          Linear.SizeDescriptor {} Absolute.Abs -> 
          Color.ColorDescriptorWithInvert {} -> 
          PropertyRuleAppender
outline strokeDescriptor sizeDescriptor colorDescriptor =
  let valueFactory = spaceTripleValue strokeDescriptor sizeDescriptor colorDescriptor
      tuple = (strokeFactory, Linear.nubSizeFactory, Color.colorFactoryWithInvert)
  in simpleProperty "outline" (valueFactory tuple)

outlineStyle : StrokeDescriptor -> PropertyRuleAppender
outlineStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "outline-style" style

outlineWidth : BasicBorderWidthDescriptor {} -> PropertyRuleAppender
outlineWidth widthDescriptor =
  simpleProperty "outline-width" (widthDescriptor borderWidthFactory)

outlineOffset : Linear.BasicSizeDescriptor Absolute.Abs -> PropertyRuleAppender
outlineOffset offsetDescriptor =
  simpleProperty "outline-offset" (offsetDescriptor Linear.basicSizeFactory)

-------------------------------------------------------------------------------
outlineColor : Color.ColorDescriptorWithInvert {} -> PropertyRuleAppender
outlineColor colorDescriptor = 
  let colorValue = colorDescriptor Color.colorFactoryWithInvert
  in simpleProperty "outline-color" colorValue

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- TODO Should take initial, inherit, other. Do like above, have borderRadius4
borderRadius : Linear.SizeDescriptor {} a ->
               Linear.SizeDescriptor {} b ->
               Linear.SizeDescriptor {} c ->
               Linear.SizeDescriptor {} d ->
               PropertyRuleAppender
borderRadius topLeftRadius topRightRadius bottomRightRadius bottomLeftRadius =
  let valueFactory = 
        spaceQuadrupleValue topLeftRadius topRightRadius bottomRightRadius bottomLeftRadius
      tuple = 
        (Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory)
  in simpleProperty "border-radius" (valueFactory tuple)

-- TODO Should take initial, inherit, other
borderTopLeftRadius : Linear.SizeDescriptor {} a ->
                      Linear.SizeDescriptor {} b ->
                      PropertyRuleAppender
borderTopLeftRadius horizontal vertical =
  let valueFactory = spacePairValue horizontal vertical 
      tuple = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in simpleProperty "border-top-left-radius" (valueFactory tuple)

-- TODO Should take initial, inherit, other
borderTopRightRadius : Linear.SizeDescriptor {} a ->
                       Linear.SizeDescriptor {} b ->
                       PropertyRuleAppender
borderTopRightRadius horizontal vertical =
  let valueFactory = spacePairValue horizontal vertical 
      tuple = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in simpleProperty "border-top-right-radius" (valueFactory tuple)

-- TODO Should take initial, inherit, other
borderBottomLeftRadius : Linear.SizeDescriptor {} a ->
                         Linear.SizeDescriptor {} b ->
                         PropertyRuleAppender
borderBottomLeftRadius horizontal vertical =
  let valueFactory = spacePairValue horizontal vertical 
      tuple = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in simpleProperty "border-bottom-left-radius" (valueFactory tuple)

-- TODO Should take initial, inherit, other
borderBottomRightRadius : Linear.SizeDescriptor {} a ->
                          Linear.SizeDescriptor {} b ->
                          PropertyRuleAppender
borderBottomRightRadius horizontal vertical =
  let valueFactory = spacePairValue horizontal vertical 
      tuple = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in simpleProperty "border-bottom-right-radius" (valueFactory tuple)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

borderCollapse : VisibilityDescriptor -> PropertyRuleAppender
borderCollapse visibilityDescriptor =
  let visibility = visibilityDescriptor visibilityFactory
  in simpleProperty "border-collapse" visibility

-- TODO Should take initial, inherit, other
borderSpacing : Linear.SizeDescriptor {} a -> PropertyRuleAppender
borderSpacing lengthDescriptor =
  simpleProperty "border-spacing" (lengthDescriptor Linear.nubSizeFactory)

-- TODO Should take initial, inherit, other
borderSpacing2 : Linear.SizeDescriptor {} a ->
                 Linear.SizeDescriptor {} b ->
                 PropertyRuleAppender
borderSpacing2 horizontal vertical =
  let valueFactory = spacePairValue horizontal vertical 
      tuple = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in simpleProperty "border-spacing" (valueFactory tuple)
