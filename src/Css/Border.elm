module Css.Border (

  -- * Stroke. Used for border-style and outline-style.
    
  solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

  -- * Border properties.
    
  , aBorderWith
  , border, borderTop, borderLeft, borderBottom, borderRight
  , borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor, borderColor4
  , borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle, borderStyle4
  
  , medium, thick, thin
  , borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth, borderWidth4

  -- * Outline properties.

  , outline, anOutlineWith
  , outlineStyle, outlineWidth, outlineOffset, outlineColor

  -- * Border radius.

  , borderRadius, borderRadius4
  , borderTopLeftRadius, borderTopLeftRadius2
  , borderTopRightRadius, borderTopRightRadius2
  , borderBottomLeftRadius, borderBottomLeftRadius2
  , borderBottomRightRadius, borderBottomRightRadius2

  -- * Collapsing borders model for a table
  , borderCollapse
  , borderSpacing, borderSpacing2
  ) where

import Css.Internal.Display exposing (VisibilityDescriptor, visibilityFactory)

import Css.Internal.Box.Border as Border
import Css.Internal.Box.Border.Stroke as Stroke
import Css.Internal.Box.Border.Width as BorderWidth
import Css.Internal.Box.Outline as Outline
import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Linear.Absolute as Absolute
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

solid : Stroke.NubStrokeDescriptor rec
solid = \factory -> factory.stroke "solid"

dotted : Stroke.NubStrokeDescriptor rec
dotted = \factory -> factory.stroke "dotted"

dashed : Stroke.NubStrokeDescriptor rec
dashed = \factory -> factory.stroke "dashed"

double : Stroke.NubStrokeDescriptor rec
double = \factory -> factory.stroke "double"

wavy : Stroke.NubStrokeDescriptor rec
wavy = \factory -> factory.stroke "wavy"

groove : Stroke.NubBorderStrokeDescriptor rec
groove = \factory -> factory.stroke "groove"

ridge : Stroke.NubBorderStrokeDescriptor rec
ridge = \factory -> factory.stroke "ridge"

inset : Stroke.NubBorderStrokeDescriptor rec
inset = \factory -> factory.stroke "inset"

outset : Stroke.NubBorderStrokeDescriptor rec
outset = \factory -> factory.stroke "outset"

-------------------------------------------------------------------------------

aBorderWith : Stroke.NubBorderStyleDescriptor {} -> 
              Linear.SizeDescriptor {} Absolute.Abs -> 
              Color.ColorDescriptor {} ->
              Border.BorderDescriptor
aBorderWith strokeDescriptor widthDescriptor colorDescriptor =
  \factory -> factory.border strokeDescriptor widthDescriptor colorDescriptor
  
border : Border.BorderDescriptor -> Stylesheet.PropertyRuleAppender
border borderDescriptor =
  let borderValue = borderDescriptor Border.borderFactory
  in Stylesheet.simpleProperty "border" borderValue

borderTop : Border.BorderDescriptor -> Stylesheet.PropertyRuleAppender
borderTop borderDescriptor =
  let borderValue = borderDescriptor Border.borderFactory
  in Stylesheet.simpleProperty "border-top" borderValue

borderLeft : Border.BorderDescriptor -> Stylesheet.PropertyRuleAppender
borderLeft borderDescriptor =
  let borderValue = borderDescriptor Border.borderFactory
  in Stylesheet.simpleProperty "border-left" borderValue

borderBottom : Border.BorderDescriptor -> Stylesheet.PropertyRuleAppender
borderBottom borderDescriptor =
  let borderValue = borderDescriptor Border.borderFactory
  in Stylesheet.simpleProperty "border-bottom" borderValue

borderRight : Border.BorderDescriptor -> Stylesheet.PropertyRuleAppender
borderRight borderDescriptor =
  let borderValue = borderDescriptor Border.borderFactory
  in Stylesheet.simpleProperty "border-right" borderValue

-------------------------------------------------------------------------------

borderColor : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
borderColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "border-color" colorValue

borderColor4 : Color.ColorDescriptor {} -> 
               Color.ColorDescriptor {} -> 
               Color.ColorDescriptor {} -> 
               Color.ColorDescriptor {} -> 
               Stylesheet.PropertyRuleAppender
borderColor4 topColor rightColor bottomColor leftColor =
  let compositeDescriptor = 
        Property.spaceQuadrupleValue topColor rightColor bottomColor leftColor      
      factory = Utils.quadrupleOf Color.nubColorFactory
  in Stylesheet.simpleProperty "border-color" (compositeDescriptor factory)

borderLeftColor : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
borderLeftColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "border-left-color" colorValue

borderRightColor : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
borderRightColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "border-right-color" colorValue

borderTopColor : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
borderTopColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "border-top-color" colorValue

borderBottomColor : Color.BasicColorDescriptor -> Stylesheet.PropertyRuleAppender
borderBottomColor colorDescriptor =
  let colorValue = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "border-bottom-color" colorValue

-------------------------------------------------------------------------------

borderStyle : Stroke.BorderStyleDescriptor {} -> Stylesheet.PropertyRuleAppender
borderStyle strokeDescriptor =
  let style = strokeDescriptor Stroke.borderStyleStrokeFactory
  in Stylesheet.simpleProperty "border-style" style

borderStyle4 : Stroke.NubBorderStyleDescriptor {} ->
               Stroke.NubBorderStyleDescriptor {} ->
               Stroke.NubBorderStyleDescriptor {} ->
               Stroke.NubBorderStyleDescriptor {} ->
               Stylesheet.PropertyRuleAppender
borderStyle4 topStroke rightStroke bottomStroke leftStroke  =
  let compositeDescriptor = 
        Property.spaceQuadrupleValue topStroke rightStroke bottomStroke leftStroke
      factory = Utils.quadrupleOf Stroke.nubBorderStyleStrokeFactory
  in Stylesheet.simpleProperty "border-style" (compositeDescriptor factory)

borderLeftStyle : Stroke.BorderStyleDescriptor {} -> 
                  Stylesheet.PropertyRuleAppender
borderLeftStyle strokeDescriptor =
  let style = strokeDescriptor Stroke.borderStyleStrokeFactory
  in Stylesheet.simpleProperty "border-left-style" style

borderRightStyle : Stroke.BorderStyleDescriptor {} -> 
                   Stylesheet.PropertyRuleAppender
borderRightStyle strokeDescriptor =
  let style = strokeDescriptor Stroke.borderStyleStrokeFactory
  in Stylesheet.simpleProperty "border-right-style" style

borderTopStyle : Stroke.BorderStyleDescriptor {} -> 
                 Stylesheet.PropertyRuleAppender
borderTopStyle strokeDescriptor =
  let style = strokeDescriptor Stroke.borderStyleStrokeFactory
  in Stylesheet.simpleProperty "border-top-style" style

borderBottomStyle : Stroke.BorderStyleDescriptor {} -> 
                    Stylesheet.PropertyRuleAppender
borderBottomStyle strokeDescriptor =
  let style = strokeDescriptor Stroke.borderStyleStrokeFactory
  in Stylesheet.simpleProperty "border-bottom-style" style

-------------------------------------------------------------------------------

medium : BorderWidth.BorderWidthDescriptor rec
medium = \factory -> factory.medium
  
thin : BorderWidth.BorderWidthDescriptor rec
thin = \factory -> factory.thin

thick : BorderWidth.BorderWidthDescriptor rec
thick = \factory -> factory.thick

-- May be a linear absolute value as well as medium, thick, thin, or generics.
borderWidth : BorderWidth.BasicBorderWidthDescriptor {} -> 
              Stylesheet.PropertyRuleAppender
borderWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor BorderWidth.borderWidthFactory
  in Stylesheet.simpleProperty "border-width" borderWidthValue

borderWidth4 : BorderWidth.BorderWidthDescriptor {} ->
               BorderWidth.BorderWidthDescriptor {} ->
               BorderWidth.BorderWidthDescriptor {} ->
               BorderWidth.BorderWidthDescriptor {} ->
               Stylesheet.PropertyRuleAppender
borderWidth4 topWidth rightWidth bottomWidth leftWidth =
  let widthFactory = BorderWidth.nubBorderWidthFactory
      compositeDescriptor = 
        Property.spaceQuadrupleValue topWidth rightWidth bottomWidth leftWidth
      factory = Utils.quadrupleOf widthFactory
  in Stylesheet.simpleProperty "border-width" (compositeDescriptor factory)

borderLeftWidth : BorderWidth.BasicBorderWidthDescriptor {} -> 
                  Stylesheet.PropertyRuleAppender
borderLeftWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor BorderWidth.borderWidthFactory
  in Stylesheet.simpleProperty "border-left-width" borderWidthValue

borderRightWidth : BorderWidth.BasicBorderWidthDescriptor {} -> 
                   Stylesheet.PropertyRuleAppender
borderRightWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor BorderWidth.borderWidthFactory
  in Stylesheet.simpleProperty "border-right-width" borderWidthValue

borderTopWidth : BorderWidth.BasicBorderWidthDescriptor {} -> 
                 Stylesheet.PropertyRuleAppender
borderTopWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor BorderWidth.borderWidthFactory
  in Stylesheet.simpleProperty "border-top-width" borderWidthValue

borderBottomWidth : BorderWidth.BasicBorderWidthDescriptor {} -> 
                    Stylesheet.PropertyRuleAppender
borderBottomWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor BorderWidth.borderWidthFactory
  in Stylesheet.simpleProperty "border-bottom-width" borderWidthValue

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- TODO radius syntax with slashes
borderRadius : Linear.BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
borderRadius radiusDescriptor = 
  let radiusValue = radiusDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "border-radius" radiusValue

borderRadius4 : Linear.SizeDescriptor {} szTopL ->
                Linear.SizeDescriptor {} szTopR ->
                Linear.SizeDescriptor {} szBotL ->
                Linear.SizeDescriptor {} szBotR ->
                Stylesheet.PropertyRuleAppender
borderRadius4 topLRadius topRRadius bottomRRadius bottomLRadius =
  let topLValue = topLRadius Linear.nubSizeFactory
      topRValue = topRRadius Linear.nubSizeFactory
      bottomRValue = bottomRRadius Linear.nubSizeFactory
      bottomLValue = bottomLRadius Linear.nubSizeFactory
      valueFactory = 
        Property.spaceQuadrupleValue identity identity identity identity
      values = (topLValue, topRValue, bottomRValue, bottomLValue)
  in Stylesheet.simpleProperty "border-radius" (valueFactory values)

borderTopLeftRadius : Linear.BasicSizeDescriptor sz ->
                      Stylesheet.PropertyRuleAppender
borderTopLeftRadius radiusDescriptor =
  let radiusValue = radiusDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "border-top-left-radius" radiusValue

borderTopLeftRadius2 : Linear.SizeDescriptor {} szH ->
                       Linear.SizeDescriptor {} szV ->
                       Stylesheet.PropertyRuleAppender
borderTopLeftRadius2 horizontal vertical =
  let compositeDescriptor = Property.spacePairValue horizontal vertical 
      factory = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in Stylesheet.simpleProperty "border-top-left-radius" (compositeDescriptor factory)

borderTopRightRadius : Linear.BasicSizeDescriptor sz ->
                       Stylesheet.PropertyRuleAppender
borderTopRightRadius radiusDescriptor =
  let radiusValue = radiusDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "border-top-right-radius" radiusValue

borderTopRightRadius2 : Linear.SizeDescriptor {} szH ->
                        Linear.SizeDescriptor {} szV ->
                        Stylesheet.PropertyRuleAppender
borderTopRightRadius2 horizontal vertical =
  let compositeDescriptor = Property.spacePairValue horizontal vertical 
      factory = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in Stylesheet.simpleProperty "border-top-right-radius" (compositeDescriptor factory)

borderBottomLeftRadius : Linear.BasicSizeDescriptor sz ->
                         Stylesheet.PropertyRuleAppender
borderBottomLeftRadius radiusDescriptor =
  let radiusValue = radiusDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "border-bottom-left-radius" radiusValue

borderBottomLeftRadius2 : Linear.SizeDescriptor {} a ->
                          Linear.SizeDescriptor {} b ->
                          Stylesheet.PropertyRuleAppender
borderBottomLeftRadius2 horizontal vertical =
  let compositeDescriptor = Property.spacePairValue horizontal vertical 
      factory = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in Stylesheet.simpleProperty "border-bottom-left-radius" (compositeDescriptor factory)

borderBottomRightRadius : Linear.BasicSizeDescriptor sz ->
                          Stylesheet.PropertyRuleAppender
borderBottomRightRadius radiusDescriptor =
  let radiusValue = radiusDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "border-bottom-right-radius" radiusValue

borderBottomRightRadius2 : Linear.SizeDescriptor {} a ->
                           Linear.SizeDescriptor {} b ->
                           Stylesheet.PropertyRuleAppender
borderBottomRightRadius2 horizontal vertical =
  let compositeDescriptor = Property.spacePairValue horizontal vertical 
      factory = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in Stylesheet.simpleProperty "border-bottom-right-radius" (compositeDescriptor factory)
  
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

borderSpacing : Linear.BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
borderSpacing lengthDescriptor =
  let spacingValue = lengthDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "border-spacing" spacingValue

borderSpacing2 : Linear.SizeDescriptor {} a ->
                 Linear.SizeDescriptor {} b ->
                 Stylesheet.PropertyRuleAppender
borderSpacing2 horizontal vertical =
  let compositeDescriptor = Property.spacePairValue horizontal vertical 
      factory = (Linear.nubSizeFactory, Linear.nubSizeFactory)
  in Stylesheet.simpleProperty "border-spacing" (compositeDescriptor factory)

borderCollapse : VisibilityDescriptor {} -> Stylesheet.PropertyRuleAppender
borderCollapse visibilityDescriptor =
  let visibility = visibilityDescriptor visibilityFactory
  in Stylesheet.simpleProperty "border-collapse" visibility

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

anOutlineWith : Stroke.NubOutlineStrokeDescriptor {} -> 
                Linear.SizeDescriptor {} Absolute.Abs -> 
                Color.NubColorDescriptorWithInvert {} ->
                Outline.OutlineDescriptor
anOutlineWith strokeDescriptor widthDescriptor colorDescriptor =
  \factory -> factory.outline strokeDescriptor widthDescriptor colorDescriptor

outline : Outline.OutlineDescriptor -> 
          Stylesheet.PropertyRuleAppender
outline outlineDescriptor =
  let outlineValue = outlineDescriptor Outline.outlineFactory
  in Stylesheet.simpleProperty "outline" outlineValue

outlineColor : Color.ColorDescriptorWithInvert {} -> 
               Stylesheet.PropertyRuleAppender
outlineColor colorDescriptor = 
  let colorValue = colorDescriptor Color.colorFactoryWithInvert
  in Stylesheet.simpleProperty "outline-color" colorValue

outlineStyle : Stroke.OutlineStrokeDescriptor {} -> 
               Stylesheet.PropertyRuleAppender
outlineStyle strokeDescriptor =
  let style = strokeDescriptor Stroke.outlineStrokeFactory
  in Stylesheet.simpleProperty "outline-style" style

outlineWidth : BorderWidth.BasicBorderWidthDescriptor {} -> 
               Stylesheet.PropertyRuleAppender
outlineWidth widthDescriptor =
  let widthValue = widthDescriptor BorderWidth.borderWidthFactory
  in Stylesheet.simpleProperty "outline-width" widthValue

outlineOffset : Linear.BasicSizeDescriptor rec -> 
                Stylesheet.PropertyRuleAppender
outlineOffset offsetDescriptor =
  let offsetValue = offsetDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "outline-offset" offsetValue
