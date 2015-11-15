module Css.Internal.Box.Shadow
  ( BoxShadowDescriptor, CompositeShadowDescriptor
  , boxShadow, shadow, inset, boxColor, boxBlur
  ) where

import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

type BoxShadow 
  = BoxShadow ShadowComponents
  | NoBoxShadow
  | InitialBoxShadow
  | InheritBoxShadow
  | UnsetBoxShadow
  | OtherBoxShadow Property.Value
    
type ShadowComponents 
  = ShadowComponents (Property.Value, Property.Value) (Maybe Property.Value) Blur Inset

type Blur
  = Blur Property.Value Property.Value
  | NoBlur

type Inset
  = Inset
  | NoInset

type alias BoxShadowDescriptor rec xSzTyp ySzTyp blurTyp spreadTyp
  = BoxShadowFactory xSzTyp ySzTyp blurTyp spreadTyp -> Shadow rec

type alias Shadow rec = { rec | shadow : BoxShadow }

type alias CompositeShadowDescriptor xSzTyp ySzTyp blurTyp spreadTyp
  = BoxShadowFactory xSzTyp ySzTyp blurTyp spreadTyp -> CompositeShadow

type alias CompositeShadow = Shadow WithComponents

type alias WithComponents = { withComponents : ShadowComponents }

-------------------------------------------------------------------------------
{- boxShadow can be:
      none | initial | inherit
or:
      h-shadow v-shadow blur spread color inset

where the last four (blur, spread, color, inset) are optional
spread is optional but if you have it you have to have blur
-}
boxShadow : BoxShadowDescriptor rec xSzTyp ySzTyp blurSzTyp spreadSzTyp -> 
            Stylesheet.PropertyRuleAppender
boxShadow shadowDescriptor =
  let boxShadow = shadowDescriptor boxShadowFactory
  in Stylesheet.simpleProperty "box-shadow" (boxShadowValue boxShadow)

-- * Composable shadow descriptors.

shadow : Linear.SizeDescriptor {} xSzTyp ->
         Linear.SizeDescriptor {} ySzTyp ->
         CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
shadow xOffsetDescriptor yOffsetDescriptor shadowFactory =
  shadowFactory.sizedShadow xOffsetDescriptor yOffsetDescriptor


inset : CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp -> 
        CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
inset innerDescriptor shadowFactory =
  let innerShadow = innerDescriptor shadowFactory
  in shadowFactory.withInset innerShadow


boxColor : Color.ColorDescriptor {} ->
           CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp ->
           CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxColor colorDescriptor innerDescriptor shadowFactory =
  let innerShadow = innerDescriptor shadowFactory
  in shadowFactory.withColor colorDescriptor innerShadow


boxBlur : Linear.SizeDescriptor {} blurSzTyp ->
          Linear.SizeDescriptor {} spreadSzTyp ->
          CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp ->
          CompositeShadowDescriptor xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxBlur blurDescriptor spreadDescriptor innerDescriptor shadowFactory =
  let innerShadow = innerDescriptor shadowFactory
  in shadowFactory.withBlur blurDescriptor spreadDescriptor innerShadow

-------------------------------------------------------------------------------

type alias BoxShadowFactory xSzTyp ySzTyp blurSzTyp spreadSzTyp =
  { sizedShadow : Linear.SizeDescriptor {} xSzTyp -> 
                  Linear.SizeDescriptor {} ySzTyp ->
                  CompositeShadow
  , withBlur : Linear.SizeDescriptor {} blurSzTyp -> 
               Linear.SizeDescriptor {} spreadSzTyp ->
               CompositeShadow ->
               CompositeShadow 
  , withColor : Color.ColorDescriptor {} -> 
                CompositeShadow ->
                CompositeShadow 
  , withInset : CompositeShadow ->
                CompositeShadow 
  , none_ : Shadow {}
  , initial_ : Shadow {}
  , inherit_ : Shadow {}
  , unset_ : Shadow {}
  , other_ : Property.Value -> Shadow {}
  }

boxShadowFactory : BoxShadowFactory xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxShadowFactory =
  { sizedShadow xOffsetDescriptor yOffsetDescriptor = 
      let xSize = xOffsetDescriptor Linear.nubSizeFactory 
          ySize = yOffsetDescriptor Linear.nubSizeFactory
          components = ShadowComponents (xSize, ySize) Nothing NoBlur NoInset
      in { shadow = BoxShadow components, withComponents = components } 
  , withBlur blurDescriptor spreadDescriptor innerShadow = 
      let newComponents = addBlur blurDescriptor spreadDescriptor innerShadow
      in { shadow = BoxShadow newComponents, withComponents = newComponents }
  , withColor colorDescriptor innerShadow = 
      let newComponents = addColor colorDescriptor innerShadow
      in { shadow = BoxShadow newComponents, withComponents = newComponents }
  , withInset innerShadow =
      let newComponents = addInset innerShadow
      in { shadow = BoxShadow newComponents, withComponents = newComponents } 
  , none_ = { shadow = NoBoxShadow }
  , initial_ = { shadow = InitialBoxShadow }
  , inherit_ = { shadow = InheritBoxShadow }
  , unset_ = { shadow = UnsetBoxShadow }
  , other_ val = { shadow = OtherBoxShadow val }
  }

addBlur : Linear.SizeDescriptor {} blurSzTyp -> 
          Linear.SizeDescriptor {} spreadSzTyp ->
          CompositeShadow ->
          ShadowComponents
addBlur blurDescriptor spreadDescriptor innerShadow = 
  let blur = blurDescriptor Linear.nubSizeFactory
      spread = spreadDescriptor Linear.nubSizeFactory
      blurComponent = Blur blur spread
  in case innerShadow.withComponents of
      ShadowComponents (xSize, ySize) shadowColor _ inset ->
        ShadowComponents (xSize, ySize) shadowColor blurComponent inset

addColor : Color.ColorDescriptor {} -> CompositeShadow -> ShadowComponents
addColor colorDescriptor innerShadow = 
  let shadowColor = colorDescriptor Color.nubColorFactory
  in case innerShadow.withComponents of
      ShadowComponents (xSize, ySize) _ blur inset ->
        ShadowComponents (xSize, ySize) (Just shadowColor) blur inset

addInset : CompositeShadow -> ShadowComponents
addInset innerShadow = 
  case innerShadow.withComponents of
    ShadowComponents (xSize, ySize) shadowColor blur _ ->
      ShadowComponents (xSize, ySize) shadowColor blur Inset

boxShadowValue : Shadow rec -> Property.Value 
boxShadowValue boxShadow=
  case boxShadow.shadow of
    BoxShadow (ShadowComponents (xSize, ySize) shadowColor blur inset) ->
      let xyValues = [ xSize, ySize ]
          blurValues = extractBlurValues blur
          colorValue = extractColorValue shadowColor
          insetValue = extractInsetValue inset
          valueListFactory = Property.spaceListValue identity
      in valueListFactory (xyValues ++ blurValues ++ colorValue ++ insetValue)
    NoBoxShadow -> Common.noneValue
    InitialBoxShadow -> Common.initialValue
    InheritBoxShadow -> Common.inheritValue
    UnsetBoxShadow -> Common.unsetValue
    OtherBoxShadow val -> Common.otherValue val

extractColorValue : (Maybe Property.Value) -> List Property.Value
extractColorValue maybeColor =
  case maybeColor of
    Just color -> [ color ]
    Nothing -> []

extractBlurValues : Blur -> List Property.Value
extractBlurValues maybeBlur =
  case maybeBlur of
    Blur blurSize spreadSize ->[ blurSize, spreadSize]
    NoBlur -> []

extractInsetValue : Inset -> List Property.Value
extractInsetValue maybeInset =
  case maybeInset of
    Inset ->  [ Property.stringValue "inset" ]
    NoInset -> []
