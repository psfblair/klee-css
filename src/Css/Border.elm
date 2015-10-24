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
import Css.Internal.Display exposing
  ( VisibilityDescriptor, visibilityFactory, visibilityValue
  )
import Css.Internal.Size exposing 
  (Size, Abs, SizeDescriptor, sizeFactory, sizeValue)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)

import Css.Internal.Border exposing (..)

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
         SizeDescriptor (Size Abs) Abs -> 
         ColorDescriptor {} -> 
         PropertyRuleAppender
border strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue strokeValue sizeValue colorValue
  in simpleProperty "border" (valueFactory (stroke, size, color))

borderTop : StrokeDescriptor -> 
            SizeDescriptor (Size Abs) Abs -> 
            ColorDescriptor {} -> 
            PropertyRuleAppender
borderTop strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue strokeValue sizeValue colorValue
  in simpleProperty "border-top" (valueFactory (stroke, width, color))

borderLeft : StrokeDescriptor -> 
             SizeDescriptor (Size Abs) Abs -> 
             ColorDescriptor {} -> 
             PropertyRuleAppender
borderLeft strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue strokeValue sizeValue colorValue
  in simpleProperty "border-left" (valueFactory (stroke, width, color))

borderBottom : StrokeDescriptor -> 
               SizeDescriptor (Size Abs) Abs -> 
               ColorDescriptor {} -> 
               PropertyRuleAppender
borderBottom strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue strokeValue sizeValue colorValue
  in simpleProperty "border-bottom" (valueFactory (stroke, width, color))

borderRight : StrokeDescriptor -> 
              SizeDescriptor (Size Abs) Abs -> 
              ColorDescriptor {} -> 
              PropertyRuleAppender
borderRight strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory = spaceTripleValue strokeValue sizeValue colorValue
  in simpleProperty "border-right" (valueFactory (stroke, width, color))

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
  in simpleProperty "border-style" (strokeValue style)

borderLeftStyle : StrokeDescriptor -> PropertyRuleAppender
borderLeftStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-left-style" (strokeValue style)

borderRightStyle : StrokeDescriptor -> PropertyRuleAppender
borderRightStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-right-style" (strokeValue style)

borderTopStyle : StrokeDescriptor -> PropertyRuleAppender
borderTopStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-top-style" (strokeValue style)

borderBottomStyle : StrokeDescriptor -> PropertyRuleAppender
borderBottomStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "border-bottom-style" (strokeValue style)

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
      valueFactory = spaceQuadrupleValue strokeValue strokeValue strokeValue strokeValue
  in simpleProperty "border-style" (valueFactory (strokeA, strokeB, strokeC, strokeD))

-------------------------------------------------------------------------------

borderWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "border-width" (sizeValue size)

borderLeftWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderLeftWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "border-left-width" (sizeValue size)

borderRightWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderRightWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "border-right-width" (sizeValue size)

borderTopWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderTopWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "border-top-width" (sizeValue size)

borderBottomWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderBottomWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "border-bottom-width" (sizeValue size)

borderWidth4 : SizeDescriptor (Size Abs) Abs ->
               SizeDescriptor (Size Abs) Abs ->
               SizeDescriptor (Size Abs) Abs ->
               SizeDescriptor (Size Abs) Abs ->
               PropertyRuleAppender
borderWidth4 sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      sizeC = sizeDescriptorC sizeFactory
      sizeD = sizeDescriptorD sizeFactory
      valueFactory = spaceQuadrupleValue sizeValue sizeValue sizeValue sizeValue
  in simpleProperty "border-width" (valueFactory (sizeA, sizeB, sizeC, sizeD))

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
  
outline : StrokeDescriptor -> 
          SizeDescriptor (Size Abs) Abs -> 
          OutlineColorDescriptor -> 
          PropertyRuleAppender
outline strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      color = colorDescriptor outlineColorFactory
      svf = strokeValue
      szf = sizeValue
      cvf = colorValue
      valueFactory = spaceTripleValue svf szf cvf
  in simpleProperty "outline" (valueFactory (stroke, size, color))

outlineStyle : StrokeDescriptor -> PropertyRuleAppender
outlineStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in simpleProperty "outline-style" (strokeValue style)

outlineWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
outlineWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "outline-width" (sizeValue size)

outlineOffset : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
outlineOffset sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "outline-offset" (sizeValue size)

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

borderRadius : SizeDescriptor (Size a) a ->
               SizeDescriptor (Size b) b ->
               SizeDescriptor (Size c) c ->
               SizeDescriptor (Size d) d ->
               PropertyRuleAppender
borderRadius sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      sizeC = sizeDescriptorC sizeFactory
      sizeD = sizeDescriptorD sizeFactory
      valueFactory = spaceQuadrupleValue sizeValue sizeValue sizeValue sizeValue
  in simpleProperty "border-radius" (valueFactory (sizeA, sizeB, sizeC, sizeD))

borderTopLeftRadius : SizeDescriptor (Size a) a ->
                      SizeDescriptor (Size b) b ->
                      PropertyRuleAppender
borderTopLeftRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValue sizeValue sizeValue
  in simpleProperty "border-top-left-radius" (valueFactory (sizeA, sizeB))

borderTopRightRadius : SizeDescriptor (Size a) a ->
                       SizeDescriptor (Size b) b ->
                       PropertyRuleAppender
borderTopRightRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValue sizeValue sizeValue
  in simpleProperty "border-top-right-radius" (valueFactory (sizeA, sizeB))

borderBottomLeftRadius : SizeDescriptor (Size a) a ->
                         SizeDescriptor (Size b) b ->
                         PropertyRuleAppender
borderBottomLeftRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValue sizeValue sizeValue
  in simpleProperty "border-bottom-left-radius" (valueFactory (sizeA, sizeB))

borderBottomRightRadius : SizeDescriptor (Size a) a ->
                          SizeDescriptor (Size b) b ->
                          PropertyRuleAppender
borderBottomRightRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValue sizeValue sizeValue
  in simpleProperty "border-bottom-right-radius" (valueFactory (sizeA, sizeB))

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

borderCollapse : VisibilityDescriptor -> PropertyRuleAppender
borderCollapse visibilityDescriptor =
  let visibility = visibilityDescriptor visibilityFactory
  in simpleProperty "border-collapse" (visibilityValue visibility)

borderSpacing : SizeDescriptor (Size a) a -> PropertyRuleAppender
borderSpacing sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in simpleProperty "border-spacing" (sizeValue size)

borderSpacing2 : SizeDescriptor (Size a) a ->
                 SizeDescriptor (Size b) b ->
                 PropertyRuleAppender
borderSpacing2 sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValue sizeValue sizeValue
  in simpleProperty "border-spacing" (valueFactory (sizeA, sizeB))
