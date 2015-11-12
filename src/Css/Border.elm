module Css.Border (
  -- * Stroke type, used for border-style and outline-style.
  solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

  -- * Border properties.

  , border, borderTop, borderLeft, borderBottom, borderRight
  , borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor, borderColor4
  , borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle, borderStyle4
  , borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth, borderWidth4

  -- * Outline properties.

  , outline, outlineStyle, outlineWidth, outlineOffset, outlineColor, invert

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
import Css.Internal.Color exposing 
  (CssColor (..), ColorDescriptor, ColorFactory
  , rgbaString, hslaString, colorFactory, colorValue
  )
import Css.Internal.Display exposing (VisibilityDescriptor, visibilityFactory)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)

import Css.Internal.Border exposing (..)

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

border : StrokeDescriptor -> 
         Linear.SizeDescriptor {} Absolute.Abs -> 
         ColorDescriptor {} -> 
         PropertyRuleAppender
border strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue identity Linear.sizeValue colorValue
  in simpleProperty "border" (valueFactory (stroke, sizeDescriptor, color))

borderTop : StrokeDescriptor -> 
            Linear.SizeDescriptor {} Absolute.Abs -> 
            ColorDescriptor {} -> 
            PropertyRuleAppender
borderTop strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue identity Linear.sizeValue colorValue
  in simpleProperty "border-top" (valueFactory (stroke, widthDescriptor, color))

borderLeft : StrokeDescriptor -> 
             Linear.SizeDescriptor {} Absolute.Abs -> 
             ColorDescriptor {} -> 
             PropertyRuleAppender
borderLeft strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue identity Linear.sizeValue colorValue
  in simpleProperty "border-left" (valueFactory (stroke, widthDescriptor, color))

borderBottom : StrokeDescriptor -> 
               Linear.SizeDescriptor {} Absolute.Abs -> 
               ColorDescriptor {} -> 
               PropertyRuleAppender
borderBottom strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue identity Linear.sizeValue colorValue
  in simpleProperty "border-bottom" (valueFactory (stroke, widthDescriptor, color))

borderRight : StrokeDescriptor -> 
              Linear.SizeDescriptor {} Absolute.Abs -> 
              ColorDescriptor {} -> 
              PropertyRuleAppender
borderRight strokeDescriptor widthDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue identity Linear.sizeValue colorValue
  in simpleProperty "border-right" (valueFactory (stroke, widthDescriptor, color))

-------------------------------------------------------------------------------

borderColor : ColorDescriptor {} -> PropertyRuleAppender
borderColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in simpleProperty "border-color" (colorValue color)

borderLeftColor : ColorDescriptor {} -> PropertyRuleAppender
borderLeftColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in simpleProperty "border-left-color" (colorValue color)

borderRightColor : ColorDescriptor {} -> PropertyRuleAppender
borderRightColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in simpleProperty "border-right-color" (colorValue color)

borderTopColor : ColorDescriptor {} -> PropertyRuleAppender
borderTopColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in simpleProperty "border-top-color" (colorValue color)

borderBottomColor : ColorDescriptor {} -> PropertyRuleAppender
borderBottomColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in simpleProperty "border-bottom-color" (colorValue color)

borderColor4 : ColorDescriptor {} -> 
               ColorDescriptor {} -> 
               ColorDescriptor {} -> 
               ColorDescriptor {} -> 
               PropertyRuleAppender
borderColor4 colorDescriptorA colorDescriptorB colorDescriptorC colorDescriptorD =
  let colorA = colorDescriptorA colorFactory
      colorB = colorDescriptorB colorFactory
      colorC = colorDescriptorC colorFactory
      colorD = colorDescriptorD colorFactory
      valueFactory = spaceQuadrupleValue colorValue colorValue colorValue colorValue
  in simpleProperty "border-color" (valueFactory (colorA, colorB, colorC, colorD))

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
-- TODO These need to take generic properties now
borderWidth : Linear.SizeDescriptor {} Absolute.Abs -> PropertyRuleAppender
borderWidth sizeDescriptor = 
  simpleProperty "border-width" (Linear.sizeValue sizeDescriptor)

borderLeftWidth : Linear.SizeDescriptor {} Absolute.Abs -> PropertyRuleAppender
borderLeftWidth sizeDescriptor =
  simpleProperty "border-left-width" (Linear.sizeValue sizeDescriptor)

borderRightWidth : Linear.SizeDescriptor {} Absolute.Abs -> PropertyRuleAppender
borderRightWidth sizeDescriptor =
  simpleProperty "border-right-width" (Linear.sizeValue sizeDescriptor)

borderTopWidth : Linear.SizeDescriptor {} Absolute.Abs -> PropertyRuleAppender
borderTopWidth sizeDescriptor =
  simpleProperty "border-top-width" (Linear.sizeValue sizeDescriptor)

borderBottomWidth : Linear.SizeDescriptor {} Absolute.Abs -> PropertyRuleAppender
borderBottomWidth sizeDescriptor =
  simpleProperty "border-bottom-width" (Linear.sizeValue sizeDescriptor)

borderWidth4 : Linear.SizeDescriptor {} Absolute.Abs ->
               Linear.SizeDescriptor {} Absolute.Abs ->
               Linear.SizeDescriptor {} Absolute.Abs ->
               Linear.SizeDescriptor {} Absolute.Abs ->
               PropertyRuleAppender
borderWidth4 sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD =
  let values = (sizeDescriptorA, sizeDescriptorB, sizeDescriptorC, sizeDescriptorD)
      valueFactory = 
        spaceQuadrupleValue Linear.sizeValue Linear.sizeValue Linear.sizeValue Linear.sizeValue 
  in simpleProperty "border-width" (valueFactory values)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
  
outline : StrokeDescriptor -> 
          Linear.SizeDescriptor {} Absolute.Abs -> 
          OutlineColorDescriptor -> 
          PropertyRuleAppender
outline strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      color = colorDescriptor outlineColorFactory
      szf = Linear.sizeValue 
      cvf = colorValue
      valueFactory = spaceTripleValue identity szf cvf
  in simpleProperty "outline" (valueFactory (stroke, sizeDescriptor, color))

outlineStyle : StrokeDescriptor -> PropertyRuleAppender
outlineStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "outline-style" style

outlineWidth : Linear.SizeDescriptor {} Absolute.Abs -> PropertyRuleAppender
outlineWidth sizeDescriptor =
  simpleProperty "outline-width" (Linear.sizeValue sizeDescriptor)

outlineOffset : Linear.SizeDescriptor {} Absolute.Abs -> PropertyRuleAppender
outlineOffset sizeDescriptor =
  simpleProperty "outline-offset" (Linear.sizeValue sizeDescriptor)

-------------------------------------------------------------------------------
outlineColor : OutlineColorDescriptor -> PropertyRuleAppender
outlineColor colorDescriptor = 
  let color = colorDescriptor outlineColorFactory
  in simpleProperty "outline-color" (colorValue color)

-- Note that OutlineColorDescriptor includes all standard color descriptors.
invert : OutlineColorDescriptor
invert factory = factory.invert

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- TODO Should take initial, inherit, other
borderRadius : Linear.SizeDescriptor {} a ->
               Linear.SizeDescriptor {} b ->
               Linear.SizeDescriptor {} c ->
               Linear.SizeDescriptor {} d ->
               PropertyRuleAppender
borderRadius sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD =
  let values = (sizeDescriptorA, sizeDescriptorB, sizeDescriptorC, sizeDescriptorD)
      valueFactory = spaceQuadrupleValue Linear.sizeValue Linear.sizeValue Linear.sizeValue Linear.sizeValue 
  in simpleProperty "border-radius" (valueFactory values)

-- TODO Should take initial, inherit, other
borderTopLeftRadius : Linear.SizeDescriptor {} a ->
                      Linear.SizeDescriptor {} b ->
                      PropertyRuleAppender
borderTopLeftRadius sizeDescriptorA sizeDescriptorB =
  let values = (sizeDescriptorA, sizeDescriptorB)
      valueFactory = spacePairValue Linear.sizeValue Linear.sizeValue 
  in simpleProperty "border-top-left-radius" (valueFactory values)

-- TODO Should take initial, inherit, other
borderTopRightRadius : Linear.SizeDescriptor {} a ->
                       Linear.SizeDescriptor {} b ->
                       PropertyRuleAppender
borderTopRightRadius sizeDescriptorA sizeDescriptorB =
  let values = (sizeDescriptorA, sizeDescriptorB)
      valueFactory = spacePairValue Linear.sizeValue  Linear.sizeValue 
  in simpleProperty "border-top-right-radius" (valueFactory values)

-- TODO Should take initial, inherit, other
borderBottomLeftRadius : Linear.SizeDescriptor {} a ->
                         Linear.SizeDescriptor {} b ->
                         PropertyRuleAppender
borderBottomLeftRadius sizeDescriptorA sizeDescriptorB =
  let values = (sizeDescriptorA, sizeDescriptorB)
      valueFactory = spacePairValue Linear.sizeValue  Linear.sizeValue 
  in simpleProperty "border-bottom-left-radius" (valueFactory values)

-- TODO Should take initial, inherit, other
borderBottomRightRadius : Linear.SizeDescriptor {} a ->
                          Linear.SizeDescriptor {} b ->
                          PropertyRuleAppender
borderBottomRightRadius sizeDescriptorA sizeDescriptorB =
  let values = (sizeDescriptorA, sizeDescriptorB)
      valueFactory = spacePairValue Linear.sizeValue  Linear.sizeValue 
  in simpleProperty "border-bottom-right-radius" (valueFactory values)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

borderCollapse : VisibilityDescriptor -> PropertyRuleAppender
borderCollapse visibilityDescriptor =
  let visibility = visibilityDescriptor visibilityFactory
  in simpleProperty "border-collapse" visibility

-- TODO Should take initial, inherit, other
borderSpacing : Linear.SizeDescriptor {} a -> PropertyRuleAppender
borderSpacing sizeDescriptor =
  simpleProperty "border-spacing" (Linear.sizeValue sizeDescriptor)

-- TODO Should take initial, inherit, other
borderSpacing2 : Linear.SizeDescriptor {} a ->
                 Linear.SizeDescriptor {} b ->
                 PropertyRuleAppender
borderSpacing2 sizeDescriptorA sizeDescriptorB =
  let values = (sizeDescriptorA, sizeDescriptorB) 
      valueFactory = spacePairValue Linear.sizeValue  Linear.sizeValue 
  in simpleProperty "border-spacing" (valueFactory values)
