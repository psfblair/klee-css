module Css.Box
  ( paddingBox, borderBox, contentBox
  , boxSizing
  , shadow, inset, boxColor, boxBlur
  , boxShadow
  ) where

import Css.Internal.Property exposing (Value, appendToPrefixedRoot)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, prefixed)
import Css.Internal.Color exposing (ColorDescriptor, colorFactory)
import Css.Internal.Size exposing (Size, SizeDescriptor, sizeFactory)

import Css.Internal.Box exposing (..)

import Css.Common exposing (browsers)

-------------------------------------------------------------------------------

boxSizing : BoxTypeDescriptor -> PropertyRuleAppender
boxSizing descriptor =
  let boxType = descriptor boxTypeFactory
      prefixedKeys = appendToPrefixedRoot browsers "box-sizing"
  in prefixed prefixedKeys boxType

paddingBox : BoxTypeDescriptor
paddingBox factory = factory.boxType "padding-box"

borderBox : BoxTypeDescriptor
borderBox factory = factory.boxType "border-box"

contentBox : BoxTypeDescriptor
contentBox factory = factory.boxType "content-box"

-------------------------------------------------------------------------------
{- boxShadow can be:
      none | initial | inherit
or:
      h-shadow v-shadow blur spread color inset
where the last four (blur, spread, color, inset) are optional
spread is optional but if you have it you have to have blur
-}
boxShadow : BoxShadowDescriptor t x y b s -> PropertyRuleAppender
boxShadow shadowDescriptor =
  let boxShadow = shadowDescriptor boxShadowFactory
      prefixedKeys = appendToPrefixedRoot browsers "box-shadow"
  in prefixed prefixedKeys (boxShadowValue boxShadow)

-- * Composable shadow descriptors.

shadow : SizeDescriptor (Size a) a ->
         SizeDescriptor (Size b) b ->
         BoxShadowDescriptor Sized a b blurSzTyp spreadSzTyp
shadow xSizeDescriptor ySizeDescriptor shadowFactory =
  let xSize = xSizeDescriptor sizeFactory
      ySize = ySizeDescriptor sizeFactory
  in shadowFactory.sizedShadow xSize ySize


inset : BoxShadowDescriptor Sized x y b s -> BoxShadowDescriptor Sized x y b s
inset shadowDescriptor shadowFactory =
  case shadowDescriptor shadowFactory of
    BoxShadow size color blur _ -> BoxShadow size color blur Inset
    somethingElse -> somethingElse -- Sized constraint keeps us from getting here


boxColor : ColorDescriptor {} ->
           BoxShadowDescriptor Sized x y b s ->
           BoxShadowDescriptor Sized x y b s
boxColor colorDescriptor shadowDescriptor shadowFactory =
  let color = colorDescriptor colorFactory
  in case shadowDescriptor shadowFactory of
    BoxShadow size _ blur inset -> BoxShadow size (ShadowColor color) blur inset
    somethingElse -> somethingElse -- Sized constraint keeps us from getting here


boxBlur : SizeDescriptor (Size b) b ->
          SizeDescriptor (Size s) s ->
          BoxShadowDescriptor Sized x y b s ->
          BoxShadowDescriptor Sized x y b s
boxBlur blurDescriptor spreadDescriptor shadowDescriptor shadowFactory =
  let blur = Blur (blurDescriptor sizeFactory) (spreadDescriptor sizeFactory)
  in case shadowDescriptor shadowFactory of
      BoxShadow size color _ inset -> BoxShadow size color blur inset
      somethingElse -> somethingElse -- Sized constraint keeps us from getting here
