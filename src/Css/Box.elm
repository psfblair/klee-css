module Css.Box
  ( BoxType
  , paddingBox, borderBox, contentBox
  , boxSizing
  , shadow, inset, withColor, withBlur
  , boxShadow
  ) where

import Css.Internal.Property exposing
  (Value, ValueFactory, prefixedKeys, concatenateValues, stringValueFactory)
import Css.Internal.Stylesheet exposing (CssGenerator, prefixed)

import Css.Color exposing (Color, colorValueFactory)
import Css.Common exposing
  ( browsers, noneValueFactory, inheritValueFactory, initialValueFactory, otherValueFactory
  )
import Css.Size exposing (Size, SizeDescriptor, sizeFactory, sizeValueFactory)

-------------------------------------------------------------------------------

type BoxType
  = BoxType String
  | InheritBoxType
  | InitialBoxType
  | OtherBoxType Value

type alias BoxTypeDescriptor = BoxTypeFactory -> BoxType

paddingBox : BoxTypeDescriptor
paddingBox factory = factory.boxType "padding-box"

borderBox : BoxTypeDescriptor
borderBox factory = factory.boxType "border-box"

contentBox : BoxTypeDescriptor
contentBox factory = factory.boxType "content-box"

-------------------------------------------------------------------------------

boxSizing : BoxTypeDescriptor -> CssGenerator BoxType
boxSizing descriptor =
  let boxType = descriptor boxTypeFactory
      browserPrefixes = prefixedKeys browsers "box-sizing"
  in prefixed browserPrefixes boxType boxTypeValueFactory

-------------------------------------------------------------------------------
{- boxShadow can be:
      none | initial | inherit
or:
      h-shadow v-shadow blur spread color inset
where the last four (blur, spread, color, inset) are optional
spread is optional but if you have it you have to have blur
-}

type Sized = Sized -- used in sizedConstraint phantom type in BoxShadow.

type BoxShadow sizedConstraint xSzTyp ySzTyp blurTyp spreadTyp
  = BoxShadow (Size xSzTyp, Size ySzTyp)
              ShadowColor
              (Blur blurTyp spreadTyp)
              Inset
  | NoBoxShadow
  | InitialBoxShadow
  | InheritBoxShadow
  | OtherBoxShadow Value

type alias BoxShadowDescriptor t x y b s
  = BoxShadowFactory x y b s -> BoxShadow t x y b s


boxShadow : BoxShadowDescriptor t x y b s -> CssGenerator (BoxShadow t x y b s)
boxShadow shadowDescriptor =
  let boxShadow = shadowDescriptor boxShadowFactory
      browserPrefixes = prefixedKeys browsers "box-sizing"
  in prefixed browserPrefixes boxShadow boxShadowValueFactory


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


withColor : Color ->
            BoxShadowDescriptor Sized x y b s ->
            BoxShadowDescriptor Sized x y b s
withColor color shadowDescriptor shadowFactory =
  case shadowDescriptor shadowFactory of
    BoxShadow size _ blur inset -> BoxShadow size (ShadowColor color) blur inset
    somethingElse -> somethingElse -- Sized constraint keeps us from getting here


withBlur : SizeDescriptor (Size b) b ->
           SizeDescriptor (Size s) s ->
           BoxShadowDescriptor Sized x y b s ->
           BoxShadowDescriptor Sized x y b s
withBlur blurDescriptor spreadDescriptor shadowDescriptor shadowFactory =
  let blur = Blur (blurDescriptor sizeFactory) (spreadDescriptor sizeFactory)
  in case shadowDescriptor shadowFactory of
      BoxShadow size color _ inset -> BoxShadow size color blur inset
      somethingElse -> somethingElse -- Sized constraint keeps us from getting here

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type alias BoxTypeFactory =
  {
    boxType: String -> BoxType
  , inherit: BoxType
  , initial: BoxType
  , other: Value -> BoxType
  }

boxTypeFactory : BoxTypeFactory
boxTypeFactory =
  {
    boxType str = BoxType str
  , inherit = InheritBoxType
  , initial = InitialBoxType
  , other val = OtherBoxType val
  }

boxTypeValueFactory : ValueFactory BoxType
boxTypeValueFactory =
  { value boxTypeValue =
      case boxTypeValue of
        BoxType str -> stringValueFactory.value str
        InheritBoxType -> inheritValueFactory.inherit
        InitialBoxType -> initialValueFactory.initial
        OtherBoxType value -> otherValueFactory.other value
  }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type Blur blurSizeType spreadSizeType
  = Blur (Size blurSizeType) (Size spreadSizeType)
  | NoBlur

type ShadowColor
  = ShadowColor Color
  | NoShadowColor

type Inset
  = Inset
  | NoInset

type alias BoxShadowFactory xSzTyp ySzTyp blurSzTyp spreadSzTyp =
  { sizedShadow: (Size xSzTyp) -> (Size ySzTyp) ->
                    BoxShadow Sized xSzTyp ySzTyp blurSzTyp spreadSzTyp
  , none: BoxShadow () xSzTyp ySzTyp blurSzTyp spreadSzTyp
  , initial: BoxShadow () xSzTyp ySzTyp blurSzTyp spreadSzTyp
  , inherit: BoxShadow () xSzTyp ySzTyp blurSzTyp spreadSzTyp
  , other: Value -> BoxShadow () xSzTyp ySzTyp blurSzTyp spreadSzTyp
  }

boxShadowFactory : BoxShadowFactory x y b s
boxShadowFactory =
  { sizedShadow xSize ySize = BoxShadow (xSize, ySize) NoShadowColor NoBlur NoInset
  , none = NoBoxShadow
  , initial = InitialBoxShadow
  , inherit = InheritBoxShadow
  , other value = OtherBoxShadow value
  }

boxShadowValueFactory: ValueFactory (BoxShadow t x y b s)
boxShadowValueFactory =
  { value boxShadow = case boxShadow of
      BoxShadow (xSize, ySize) shadowColor blur inset ->
        let colorValue = extractColorValue shadowColor
            blurValues = extractBlurValues blur
            insetValue = extractInsetValue inset
            xyValues = [ sizeValueFactory.value xSize, sizeValueFactory.value ySize ]
        in concatenateValues (xyValues ++ blurValues ++ colorValue ++ insetValue)
      NoBoxShadow -> noneValueFactory.none
      InitialBoxShadow -> initialValueFactory.initial
      InheritBoxShadow -> inheritValueFactory.inherit
      OtherBoxShadow value -> otherValueFactory.other value
  }

extractColorValue : ShadowColor -> List Value
extractColorValue maybeColor =
  case maybeColor of
    NoShadowColor -> []
    ShadowColor color -> [ colorValueFactory.value color ]

extractBlurValues : Blur b s -> List Value
extractBlurValues maybeBlur =
  case maybeBlur of
    NoBlur -> []
    Blur blurSize spreadSize ->
      [ sizeValueFactory.value blurSize
      , sizeValueFactory.value spreadSize
      ]

extractInsetValue : Inset -> List Value
extractInsetValue maybeInset =
  case maybeInset of
    NoInset -> []
    Inset ->  [ stringValueFactory.value "inset" ]
