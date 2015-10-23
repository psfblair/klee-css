module Css.Border (
  -- * Stroke type, used for border-style and outline-style.
    Stroke
  , solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

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

  -- * Used by other modules
  , StrokeDescriptor

  ) where

import Css.Internal.Property exposing
  ( Value, ValueFactory, stringKey
  , stringValueFactory, spacePairValueFactory
  , spaceTripleValueFactory, spaceQuadrupleValueFactory
  )
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, key)

import Css.Common exposing (
  Other, Inherit, Auto, None
  , otherValue, initialValue, inheritValue, autoValue, noneValue
  )
import Css.Size exposing (Size, Abs, SizeDescriptor, sizeFactory, sizeValueFactory)

import Css.Color exposing 
  (CssColor (..), ColorDescriptor, ColorFactory
  , rgbaString, hslaString, colorFactory, colorValueFactory
  )

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
  | OtherStroke String

-- See bottom of this file for the additional boilerplate types
type alias StrokeDescriptor = StrokeFactory -> Stroke

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
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border") (stroke, size, color) valueFactory

borderTop : StrokeDescriptor -> 
            SizeDescriptor (Size Abs) Abs -> 
            ColorDescriptor {} -> 
            PropertyRuleAppender
borderTop strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-top") (stroke, width, color) valueFactory

borderLeft : StrokeDescriptor -> 
             SizeDescriptor (Size Abs) Abs -> 
             ColorDescriptor {} -> 
             PropertyRuleAppender
borderLeft strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-left") (stroke, width, color) valueFactory

borderBottom : StrokeDescriptor -> 
               SizeDescriptor (Size Abs) Abs -> 
               ColorDescriptor {} -> 
               PropertyRuleAppender
borderBottom strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-bottom") (stroke, width, color) valueFactory

borderRight : StrokeDescriptor -> 
              SizeDescriptor (Size Abs) Abs -> 
              ColorDescriptor {} -> 
              PropertyRuleAppender
borderRight strokeDescriptor sizeDescriptor colorDescriptor =
  let stroke = strokeDescriptor strokeFactory
      width = sizeDescriptor sizeFactory
      color = colorDescriptor colorFactory
      valueFactory =
        spaceTripleValueFactory strokeValueFactory sizeValueFactory colorValueFactory
  in key (stringKey "border-right") (stroke, width, color) valueFactory

-------------------------------------------------------------------------------

borderColor : ColorDescriptor {} -> PropertyRuleAppender
borderColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in key (stringKey "border-color") color colorValueFactory

borderLeftColor : ColorDescriptor {} -> PropertyRuleAppender
borderLeftColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in key (stringKey "border-left-color") color colorValueFactory

borderRightColor : ColorDescriptor {} -> PropertyRuleAppender
borderRightColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in key (stringKey "border-right-color") color colorValueFactory

borderTopColor : ColorDescriptor {} -> PropertyRuleAppender
borderTopColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in key (stringKey "border-top-color") color colorValueFactory

borderBottomColor : ColorDescriptor {} -> PropertyRuleAppender
borderBottomColor colorDescriptor =
  let color = colorDescriptor colorFactory
  in key (stringKey "border-bottom-color") color colorValueFactory

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
      cvf = colorValueFactory
      valueFactory = spaceQuadrupleValueFactory cvf cvf cvf cvf
  in key (stringKey "border-color") (colorA, colorB, colorC, colorD) valueFactory

-------------------------------------------------------------------------------

borderStyle : StrokeDescriptor -> PropertyRuleAppender
borderStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-style") style strokeValueFactory

borderLeftStyle : StrokeDescriptor -> PropertyRuleAppender
borderLeftStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-left-style") style strokeValueFactory

borderRightStyle : StrokeDescriptor -> PropertyRuleAppender
borderRightStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-right-style") style strokeValueFactory

borderTopStyle : StrokeDescriptor -> PropertyRuleAppender
borderTopStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-top-style") style strokeValueFactory

borderBottomStyle : StrokeDescriptor -> PropertyRuleAppender
borderBottomStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "border-bottom-style") style strokeValueFactory

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
      svf = strokeValueFactory
      valueFactory = spaceQuadrupleValueFactory svf svf svf svf
  in key (stringKey "border-style") (strokeA, strokeB, strokeC, strokeD) valueFactory

-------------------------------------------------------------------------------

borderWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-width") size sizeValueFactory

borderLeftWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderLeftWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-left-width") size sizeValueFactory

borderRightWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderRightWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-right-width") size sizeValueFactory

borderTopWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderTopWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-top-width") size sizeValueFactory

borderBottomWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
borderBottomWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-bottom-width") size sizeValueFactory

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
      svf = sizeValueFactory
      valueFactory = spaceQuadrupleValueFactory svf svf svf svf
  in key (stringKey "border-width") (sizeA, sizeB, sizeC, sizeD) valueFactory

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
      svf = strokeValueFactory
      szf = sizeValueFactory
      cvf = colorValueFactory
      valueFactory = spaceTripleValueFactory svf szf cvf
  in key (stringKey "outline") (stroke, size, color) valueFactory

outlineStyle : StrokeDescriptor -> PropertyRuleAppender
outlineStyle strokeDescriptor =
  let style = strokeDescriptor strokeFactory
  in key (stringKey "outline-style") style strokeValueFactory

outlineWidth : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
outlineWidth sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-width") size sizeValueFactory

outlineOffset : SizeDescriptor (Size Abs) Abs -> PropertyRuleAppender
outlineOffset sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "outline-offset") size sizeValueFactory

-------------------------------------------------------------------------------
outlineColor : OutlineColorDescriptor -> PropertyRuleAppender
outlineColor colorDescriptor = 
  let color = colorDescriptor outlineColorFactory
  in key (stringKey "outline-color") color colorValueFactory

invert : OutlineColorDescriptor
invert factory = factory.invert

-------------------------------------------------------------------------------
-- NOTE outline-color takes "invert" as well as the standard color descriptors,
-- which is why we need ColorDescriptor to be parameterized.

type alias OutlineColorDescriptor = OutlineColorFactory -> CssColor

type alias InvertColorFactory = { invert: CssColor }

type alias OutlineColorFactory = ColorFactory InvertColorFactory

outlineColorFactory : OutlineColorFactory
outlineColorFactory = { colorFactory | invert = OtherColor "invert" }

outlineColorValueFactory : ValueFactory CssColor
outlineColorValueFactory = 
    { value cssColor =
        case cssColor of 
          CssRgba color ->  rgbaString color |> stringValueFactory.value
          CssHsla color ->  hslaString color |> stringValueFactory.value
          InitialColor -> initialValue
          InheritColor -> inheritValue
          OtherColor str -> otherValue str
    }

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
      szf = sizeValueFactory
      valueFactory = spaceQuadrupleValueFactory szf szf szf szf
  in key (stringKey "border-radius") (sizeA, sizeB, sizeC, sizeD) valueFactory

borderTopLeftRadius : SizeDescriptor (Size a) a ->
                      SizeDescriptor (Size b) b ->
                      PropertyRuleAppender
borderTopLeftRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-top-left-radius") (sizeA, sizeB) valueFactory

borderTopRightRadius : SizeDescriptor (Size a) a ->
                       SizeDescriptor (Size b) b ->
                       PropertyRuleAppender
borderTopRightRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-top-right-radius") (sizeA, sizeB) valueFactory

borderBottomLeftRadius : SizeDescriptor (Size a) a ->
                         SizeDescriptor (Size b) b ->
                         PropertyRuleAppender
borderBottomLeftRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-bottom-left-radius") (sizeA, sizeB) valueFactory

borderBottomRightRadius : SizeDescriptor (Size a) a ->
                          SizeDescriptor (Size b) b ->
                          PropertyRuleAppender
borderBottomRightRadius sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-bottom-right-radius") (sizeA, sizeB) valueFactory

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

borderCollapse : VisibilityDescriptor -> PropertyRuleAppender
borderCollapse visibilityDescriptor =
  let visibility = visibilityDescriptor visibilityFactory
  in key (stringKey "border-collapse") visibility visibilityValueFactory

borderSpacing : SizeDescriptor (Size a) a -> PropertyRuleAppender
borderSpacing sizeDescriptor =
  let size = sizeDescriptor sizeFactory
  in key (stringKey "border-spacing") size sizeValueFactory

borderSpacing2 : SizeDescriptor (Size a) a ->
                 SizeDescriptor (Size b) b ->
                 PropertyRuleAppender
borderSpacing2 sizeDescriptorA sizeDescriptorB =
  let sizeA = sizeDescriptorA sizeFactory
      sizeB = sizeDescriptorB sizeFactory
      valueFactory = spacePairValueFactory sizeValueFactory sizeValueFactory
  in key (stringKey "border-spacing") (sizeA, sizeB) valueFactory

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Ancillary types used for implementation. These substitute for Clay's typeclasses.

type alias StrokeFactory =
  {
    stroke: String -> Stroke
  , none: Stroke
  , inherit: Stroke
  , auto: Stroke
  , other: String -> Stroke
  }

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
        NoStroke -> noneValue
        InheritStroke -> inheritValue
        AutoStroke -> autoValue
        OtherStroke val -> otherValue val
  }
