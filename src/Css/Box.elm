module Css.Box
  ( boxSizing
  , paddingBox, borderBox, contentBox
  , boxShadow
  , shadow, inset, boxColor, boxBlur
  ) where

import Css.Internal.Box.Shadow as BoxShadow
import Css.Internal.Box.Sizing as BoxSizing
import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

boxSizing : BoxSizing.BoxSizingDescriptor -> Stylesheet.PropertyRuleAppender
boxSizing = BoxSizing.boxSizing

paddingBox : BoxSizing.BoxTypeDescriptor rec
paddingBox = BoxSizing.paddingBox

borderBox : BoxSizing.BoxTypeDescriptor rec
borderBox = BoxSizing.borderBox

contentBox : BoxSizing.BoxTypeDescriptor rec
contentBox = BoxSizing.contentBox

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
boxShadow = BoxShadow.boxShadow

-- * Composable shadow descriptors.

shadow : Linear.SizeDescriptor {} xSzTyp ->
         Linear.SizeDescriptor {} ySzTyp ->
         BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
shadow = BoxShadow.shadow


inset : BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp -> 
        BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
inset = BoxShadow.inset


boxColor : Color.ColorDescriptor {} ->
           BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp ->
           BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxColor = BoxShadow.boxColor


boxBlur : Linear.SizeDescriptor {} blurSzTyp ->
          Linear.SizeDescriptor {} spreadSzTyp ->
          BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp ->
          BoxShadow.CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxBlur = BoxShadow.boxBlur
