module Css.Border (
  -- * Stroke type, used for border-style and outline-style.
    Stroke
  , solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

  -- * Border properties.

  , border, borderTop, borderLeft, borderBottom, borderRight
  , borderColor4, borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor
  , borderStyle4, borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle
  , borderWidth4, borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth

  -- * Outline properties.

  , outline, outlineTop, outlineLeft, outlineBottom, outlineRight
  , outlineColor4, outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor
  , outlineStyle4, outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle
  , outlineWidth4, outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth
  , outlineOffset

  -- * Border radius.

  , borderRadius
  , borderTopLeftRadius, borderTopRightRadius
  , borderBottomLeftRadius, borderBottomRightRadius

  -- * Collapsing borders model for a table
  , borderCollapse
  , borderSpacing, borderSpacing2

  ) where

import Css.Internal.Property exposing
  ( Value, ValueFactory, stringKey
  , stringValueFactory, spacePairValueFactory
  , spaceTripleValueFactory, spaceQuadrupleValueFactory
  )
import Css.Internal.Stylesheet exposing (CssGenerator, key)

import Css.Common exposing (
  Other, Inherit, Auto, None
  , otherValueFactory, inheritValueFactory, autoValueFactory, noneValueFactory
  )
import Css.Size exposing (Size, Abs, SizeDescriptor, sizeFactory, sizeValueFactory)
import Css.Color exposing (Color, colorValueFactory)
import Css.Display exposing
  ( Visibility, VisibilityDescriptor
  , visibilityFactory, visibilityValueFactory
  )

-------------------------------------------------------------------------------

type Stroke
  = Stroke String
  | NoStroke
  | InheritStroke
  | AutoStroke
  | OtherStroke Value

type alias StrokeFactory =
  {
    stroke: String -> Stroke
  , none: Stroke
  , inherit: Stroke
  , auto: Stroke
  , other: Value -> Stroke
  }

type alias StrokeDescriptor = StrokeFactory -> Stroke

strokeFactory : StrokeFactory
strokeFactory =
  {
    stroke str = Stroke str
  , none = NoStroke
  , inherit = InheritStroke
  , auto = AutoStroke
  , other val = OtherStroke val
  }

strokeValueFactory : ValueFactory Stroke
strokeValueFactory =
  { value stroke =
      case stroke of
        Stroke str -> stringValueFactory.value str
        NoStroke -> noneValueFactory.none
        InheritStroke -> inheritValueFactory.inherit
        AutoStroke -> autoValueFactory.auto
        OtherStroke val -> otherValueFactory.other val
  }

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

border : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
            CssGenerator (Stroke, Size Abs, Color )
border strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      valueFactory = spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border") (stroke, size, color) valueFactory

borderTop : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
              CssGenerator (Stroke, Size Abs, Color )
borderTop strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      valueFactory = spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-top") (stroke, width, color) valueFactory

borderLeft : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
                CssGenerator (Stroke, Size Abs, Color )
borderLeft strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      valueFactory = spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-left") (stroke, width, color) valueFactory

borderBottom : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
                  CssGenerator (Stroke, Size Abs, Color )
borderBottom strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      valueFactory = spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-bottom") (stroke, width, color) valueFactory

borderRight : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
                  CssGenerator (Stroke, Size Abs, Color )
borderRight strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      valueFactory = spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-right") (stroke, width, color) valueFactory

-------------------------------------------------------------------------------

borderColor4 : Color -> Color -> Color -> Color ->
                  CssGenerator (Color, Color, Color, Color )
borderColor4 colorA colorB colorC colorD =
  let valueFactory =
    spaceQuadrupleValueFactory colorValueFactory colorValueFactory colorValueFactory colorValueFactory
  in key (stringKey "border-color") (colorA, colorB, colorC, colorD) valueFactory

borderColor : Color -> CssGenerator Color
borderColor color = key (stringKey "border-color") color colorValueFactory

borderLeftColor : Color -> CssGenerator Color
borderLeftColor color = key (stringKey "border-left-color") color colorValueFactory

borderRightColor : Color -> CssGenerator Color
borderRightColor color = key (stringKey "border-right-color") color colorValueFactory

borderTopColor : Color -> CssGenerator Color
borderTopColor color = key (stringKey "border-top-color") color colorValueFactory

borderBottomColor : Color -> CssGenerator Color
borderBottomColor color = key (stringKey "border-bottom-color") color colorValueFactory

-------------------------------------------------------------------------------

borderStyle4 : StrokeDescriptor -> StrokeDescriptor -> StrokeDescriptor -> StrokeDescriptor ->
                  CssGenerator (Stroke, Stroke, Stroke, Stroke )
borderStyle4 strokeDescriptorA strokeDescriptorB strokeDescriptorC strokeDescriptorD =
  let strokeA = strokeDescriptorA strokeFactory
      strokeB = strokeDescriptorB strokeFactory
      strokeC = strokeDescriptorC strokeFactory
      strokeD = strokeDescriptorD strokeFactory
      valueFactory =
        spaceQuadrupleValueFactory strokeValueFactory strokeValueFactory strokeValueFactory strokeValueFactory
  in key (stringKey "border-style") (strokeA, strokeB, strokeC, strokeD) valueFactory

borderStyle : StrokeDescriptor -> CssGenerator Stroke
borderStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-style") style strokeValueFactory

borderLeftStyle : StrokeDescriptor -> CssGenerator Stroke
borderLeftStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-left-style") style strokeValueFactory

borderRightStyle : StrokeDescriptor -> CssGenerator Stroke
borderRightStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-right-style") style strokeValueFactory

borderTopStyle : StrokeDescriptor -> CssGenerator Stroke
borderTopStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-top-style") style strokeValueFactory

borderBottomStyle : StrokeDescriptor -> CssGenerator Stroke
borderBottomStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-bottom-style") style strokeValueFactory

-------------------------------------------------------------------------------

borderWidth4 : SizeDescriptor Abs ->
                  SizeDescriptor Abs ->
                  SizeDescriptor Abs ->
                  SizeDescriptor Abs ->
                  CssGenerator (Size Abs, Size Abs, Size Abs, Size Abs )
borderWidth4 sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      sizeC = sizeDescriptorC sizeFactory
      sizeD = sizeDescriptorD sizeFactory
      valueFactory =
        spaceQuadrupleValueFactory sizeValueFactory sizeValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-width") (sizeA, sizeB, sizeC, sizeD) valueFactory

borderWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
borderWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-width") size sizeValueFactory

borderLeftWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
borderLeftWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-left-width") size sizeValueFactory

borderRightWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
borderRightWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-right-width") size sizeValueFactory

borderTopWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
borderTopWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-top-width") size sizeValueFactory

borderBottomWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
borderBottomWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-bottom-width") size sizeValueFactory

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

outline : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
            CssGenerator (Stroke, Size Abs, Color)
outline strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "outline") (stroke, size, color) valueFactory

outlineTop : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
              CssGenerator (Stroke, Size Abs, Color)
outlineTop strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "outline-top") (stroke, size, color) valueFactory

outlineLeft : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
                CssGenerator (Stroke, Size Abs, Color)
outlineLeft strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "outline-left") (stroke, size, color) valueFactory

outlineBottom : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
                    CssGenerator (Stroke, Size Abs, Color)
outlineBottom strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "outline-bottom") (stroke, size, color) valueFactory

outlineRight : StrokeDescriptor -> SizeDescriptor Abs -> Color ->
                  CssGenerator (Stroke, Size Abs, Color)
outlineRight strokeDescriptor sizeDescriptor color =
  let stroke = strokeDescriptor strokeFactory
      size = sizeDescriptor sizeFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "outline-right") (stroke, size, color) valueFactory

-------------------------------------------------------------------------------

outlineColor4 : Color -> Color -> Color -> Color -> CssGenerator (Color, Color, Color, Color )
outlineColor4 colorA colorB colorC colorD =
  let valueFactory =
    spaceQuadrupleValueFactory colorValueFactory colorValueFactory colorValueFactory colorValueFactory
  in key (stringKey "outline-color") (colorA, colorB, colorC, colorD) valueFactory

outlineColor : Color -> CssGenerator Color
outlineColor color = key (stringKey "outline-color") color colorValueFactory

outlineLeftColor : Color -> CssGenerator Color
outlineLeftColor color = key (stringKey "outline-left-color") color colorValueFactory

outlineRightColor : Color -> CssGenerator Color
outlineRightColor color = key (stringKey "outline-right-color") color colorValueFactory

outlineTopColor : Color -> CssGenerator Color
outlineTopColor color = key (stringKey "outline-top-color") color colorValueFactory

outlineBottomColor : Color -> CssGenerator Color
outlineBottomColor color = key (stringKey "outline-bottom-color") color colorValueFactory

-------------------------------------------------------------------------------

outlineStyle4 : StrokeDescriptor -> StrokeDescriptor -> StrokeDescriptor -> StrokeDescriptor ->
                  CssGenerator (Stroke, Stroke, Stroke, Stroke )
outlineStyle4 strokeDescriptorA strokeDescriptorB strokeDescriptorC strokeDescriptorD =
  let strokeA = strokeDescriptorA strokeFactory
      strokeB = strokeDescriptorB strokeFactory
      strokeC = strokeDescriptorC strokeFactory
      strokeD = strokeDescriptorD strokeFactory
      valueFactory =
        spaceQuadrupleValueFactory strokeValueFactory strokeValueFactory strokeValueFactory strokeValueFactory
  in key (stringKey "outline-style") (strokeA, strokeB, strokeC, strokeD) valueFactory

outlineStyle : StrokeDescriptor -> CssGenerator Stroke
outlineStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "outline-style") style strokeValueFactory

outlineLeftStyle : StrokeDescriptor -> CssGenerator Stroke
outlineLeftStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "outline-left-style") style strokeValueFactory

outlineRightStyle : StrokeDescriptor -> CssGenerator Stroke
outlineRightStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "outline-right-style") style strokeValueFactory

outlineTopStyle : StrokeDescriptor -> CssGenerator Stroke
outlineTopStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "outline-top-style") style strokeValueFactory

outlineBottomStyle : StrokeDescriptor -> CssGenerator Stroke
outlineBottomStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "outline-bottom-style") style strokeValueFactory

-------------------------------------------------------------------------------

outlineWidth4 : SizeDescriptor Abs -> SizeDescriptor Abs -> SizeDescriptor Abs -> SizeDescriptor Abs ->
                  CssGenerator (Size Abs, Size Abs, Size Abs, Size Abs )
outlineWidth4 sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      sizeC = sizeDescriptorC sizeFactory
      sizeD = sizeDescriptorD sizeFactory
      valueFactory =
        spaceQuadrupleValueFactory sizeValueFactory sizeValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "outline-width") (sizeA, sizeB, sizeC, sizeD) valueFactory

outlineWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
outlineWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-width") size sizeValueFactory

outlineLeftWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
outlineLeftWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-left-width") size sizeValueFactory

outlineRightWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
outlineRightWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-right-width") size sizeValueFactory

outlineTopWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
outlineTopWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-top-width") size sizeValueFactory

outlineBottomWidth : SizeDescriptor Abs -> CssGenerator (Size Abs)
outlineBottomWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-bottom-width") size sizeValueFactory

outlineOffset : SizeDescriptor Abs -> CssGenerator (Size Abs)
outlineOffset sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-offset") size sizeValueFactory

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

borderRadius : SizeDescriptor a ->
               SizeDescriptor b ->
               SizeDescriptor c ->
               SizeDescriptor d ->
               CssGenerator (Size a, Size b, Size c, Size d)
borderRadius sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      sizeC = sizeDescriptorC sizeFactory
      sizeD = sizeDescriptorD sizeFactory
      szf = sizeValueFactory
      valueFactory = spaceQuadrupleValueFactory szf szf szf szf
  in key (stringKey "border-radius") (sizeA, sizeB, sizeC, sizeD) valueFactory

borderTopLeftRadius : SizeDescriptor a ->
                      SizeDescriptor b ->
                      CssGenerator (Size a, Size b)
borderTopLeftRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-top-left-radius") (sizeA, sizeB) valueFactory

borderTopRightRadius : SizeDescriptor a ->
                       SizeDescriptor b ->
                       CssGenerator (Size a, Size b)
borderTopRightRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-top-right-radius") (sizeA, sizeB) valueFactory

borderBottomLeftRadius : SizeDescriptor a ->
                         SizeDescriptor b ->
                         CssGenerator (Size a, Size b)
borderBottomLeftRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-bottom-left-radius") (sizeA, sizeB) valueFactory

borderBottomRightRadius : SizeDescriptor a ->
                          SizeDescriptor b ->
                          CssGenerator (Size a, Size b)
borderBottomRightRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-bottom-right-radius") (sizeA, sizeB) valueFactory

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

borderCollapse : VisibilityDescriptor -> CssGenerator Visibility
borderCollapse visibilityDescriptor =
  let visibility = visibilityDescriptor visibilityFactory
  in key (stringKey "border-collapse") visibility visibilityValueFactory

borderSpacing : SizeDescriptor a -> CssGenerator (Size a)
borderSpacing sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-spacing") size sizeValueFactory

borderSpacing2 : SizeDescriptor a -> SizeDescriptor b -> CssGenerator (Size a, Size b)
borderSpacing2 sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-spacing") (sizeA, sizeB) valueFactory
