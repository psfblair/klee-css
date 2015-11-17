module Css.Box
  ( 
  -- * Box-sizing.
    boxSizing
  , paddingBox, borderBox, contentBox

  -- * Box-shadow.

  , boxShadow
  , shadow, boxInset, boxColor, boxBlur

  -- * Border properties.
    
  , aBorderWith
  , border, borderTop, borderLeft, borderBottom, borderRight
  , borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor, borderColor4
  , borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle, borderStyle4
  
  , medium, thick, thin
  , borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth, borderWidth4

  -- * Border radius.

  , borderRadius, borderRadius4
  , borderTopLeftRadius, borderTopLeftRadius2
  , borderTopRightRadius, borderTopRightRadius2
  , borderBottomLeftRadius, borderBottomLeftRadius2
  , borderBottomRightRadius, borderBottomRightRadius2

  -- * Collapsing borders model for a table
  , borderCollapse
  , borderSpacing, borderSpacing2

  -- * Outline properties.

  , outline, anOutlineWith
  , outlineStyle, outlineWidth, outlineOffset, outlineColor
  
  ) where

import Css.Internal.Box.Border as Border
import Css.Internal.Box.Shadow as BoxShadow
import Css.Internal.Box.Sizing as BoxSizing
import Css.Internal.Box.Outline as Outline
import Css.Internal.Color as Color
import Css.Internal.Display as Display
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Stroke as Stroke
import Css.Internal.Stylesheet as Stylesheet
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

-- These descriptors must be usable in cases where we don't want to accept 
-- generic properties; i.e., where we are constructing more complex descriptors. 
-- So we don't require that the common properties be present.
paddingBox : BoxSizing.BoxTypeDescriptor rec
paddingBox = \factory -> factory.boxType "padding-box"

borderBox : BoxSizing.BoxTypeDescriptor rec
borderBox = \factory ->  factory.boxType "border-box"

contentBox : BoxSizing.BoxTypeDescriptor rec
contentBox = \factory ->  factory.boxType "content-box"

boxSizing : BoxSizing.BoxSizingDescriptor -> Stylesheet.PropertyRuleAppender
boxSizing descriptor =
  let boxType = descriptor BoxSizing.boxTypeFactory
  in Stylesheet.simpleProperty "box-sizing" boxType

-------------------------------------------------------------------------------
{- boxShadow can be:
      none | initial | inherit
or:
      h-shadow v-shadow blur spread color inset
where the last four (blur, spread, color, inset) are optional
spread is optional but if you have it you have to have blur
-}
boxShadow : BoxShadow.BoxShadowDescriptor rec xSzTyp ySzTyp blurSzTyp spreadSzTyp -> 
            Stylesheet.PropertyRuleAppender
boxShadow shadowDescriptor =
  let boxShadow = shadowDescriptor BoxShadow.boxShadowFactory
  in Stylesheet.simpleProperty "box-shadow" (BoxShadow.boxShadowValue boxShadow)

-- * Composable shadow descriptors.

shadow : Linear.SizeDescriptor {} xSzTyp ->
         Linear.SizeDescriptor {} ySzTyp ->
         BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
shadow xOffsetDescriptor yOffsetDescriptor shadowFactory =
  shadowFactory.sizedShadow xOffsetDescriptor yOffsetDescriptor


boxInset : BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp -> 
           BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxInset innerDescriptor shadowFactory =
  let innerShadow = innerDescriptor shadowFactory
  in shadowFactory.withInset innerShadow


boxColor : Color.NubColorDescriptor {} ->
           BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp ->
           BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxColor colorDescriptor innerDescriptor shadowFactory =
  let innerShadow = innerDescriptor shadowFactory
  in shadowFactory.withColor colorDescriptor innerShadow


boxBlur : Linear.SizeDescriptor {} blurSzTyp ->
          Linear.SizeDescriptor {} spreadSzTyp ->
          BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp ->
          BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxBlur blurDescriptor spreadDescriptor innerDescriptor shadowFactory =
  let innerShadow = innerDescriptor shadowFactory
  in shadowFactory.withBlur blurDescriptor spreadDescriptor innerShadow

-------------------------------------------------------------------------------

aBorderWith : Stroke.NubBorderStyleDescriptor {} -> 
              Linear.SizeDescriptor {} Linear.Abs -> 
              Color.NubColorDescriptor {} ->
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

borderColor4 : Color.NubColorDescriptor {} -> 
               Color.NubColorDescriptor {} -> 
               Color.NubColorDescriptor {} -> 
               Color.NubColorDescriptor {} -> 
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

medium : Border.BorderWidthDescriptor rec
medium = \factory -> factory.medium
  
thin : Border.BorderWidthDescriptor rec
thin = \factory -> factory.thin

thick : Border.BorderWidthDescriptor rec
thick = \factory -> factory.thick

-- May be a linear absolute value as well as medium, thick, thin, or generics.
borderWidth : Border.BasicBorderWidthDescriptor {} -> 
              Stylesheet.PropertyRuleAppender
borderWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor Border.borderWidthFactory
  in Stylesheet.simpleProperty "border-width" borderWidthValue

borderWidth4 : Border.BorderWidthDescriptor {} ->
               Border.BorderWidthDescriptor {} ->
               Border.BorderWidthDescriptor {} ->
               Border.BorderWidthDescriptor {} ->
               Stylesheet.PropertyRuleAppender
borderWidth4 topWidth rightWidth bottomWidth leftWidth =
  let widthFactory = Border.nubBorderWidthFactory
      compositeDescriptor = 
        Property.spaceQuadrupleValue topWidth rightWidth bottomWidth leftWidth
      factory = Utils.quadrupleOf widthFactory
  in Stylesheet.simpleProperty "border-width" (compositeDescriptor factory)

borderLeftWidth : Border.BasicBorderWidthDescriptor {} -> 
                  Stylesheet.PropertyRuleAppender
borderLeftWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor Border.borderWidthFactory
  in Stylesheet.simpleProperty "border-left-width" borderWidthValue

borderRightWidth : Border.BasicBorderWidthDescriptor {} -> 
                   Stylesheet.PropertyRuleAppender
borderRightWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor Border.borderWidthFactory
  in Stylesheet.simpleProperty "border-right-width" borderWidthValue

borderTopWidth : Border.BasicBorderWidthDescriptor {} -> 
                 Stylesheet.PropertyRuleAppender
borderTopWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor Border.borderWidthFactory
  in Stylesheet.simpleProperty "border-top-width" borderWidthValue

borderBottomWidth : Border.BasicBorderWidthDescriptor {} -> 
                    Stylesheet.PropertyRuleAppender
borderBottomWidth widthDescriptor = 
  let borderWidthValue = widthDescriptor Border.borderWidthFactory
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

borderCollapse : Display.VisibilityDescriptor {} -> Stylesheet.PropertyRuleAppender
borderCollapse visibilityDescriptor =
  let visibility = visibilityDescriptor Display.visibilityFactory
  in Stylesheet.simpleProperty "border-collapse" visibility

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

anOutlineWith : Stroke.NubOutlineStrokeDescriptor {} -> 
                Linear.SizeDescriptor {} Linear.Abs -> 
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

outlineWidth : Border.BasicBorderWidthDescriptor {} -> 
               Stylesheet.PropertyRuleAppender
outlineWidth widthDescriptor =
  let widthValue = widthDescriptor Border.borderWidthFactory
  in Stylesheet.simpleProperty "outline-width" widthValue

outlineOffset : Linear.BasicSizeDescriptor rec -> 
                Stylesheet.PropertyRuleAppender
outlineOffset offsetDescriptor =
  let offsetValue = offsetDescriptor Linear.basicSizeFactory
  in Stylesheet.simpleProperty "outline-offset" offsetValue
