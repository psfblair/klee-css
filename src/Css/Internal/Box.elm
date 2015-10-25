module Css.Internal.Box
  ( BoxType, BoxTypeDescriptor, boxTypeFactory, boxTypeValue
  , BoxShadow (..), BoxShadowDescriptor
  , Inset (..), ShadowColor (..), Blur (..), Sized
  , boxShadowFactory, boxShadowValue
  ) where
  
import Css.Internal.Property exposing 
  ( Value, Element
  , stringValue, spaceListValue
  )
import Css.Internal.Common exposing
  ( noneValue, inheritValue, initialValue, otherValue)
  
import Css.Internal.Color exposing (CssColor, colorValue)
import Css.Internal.Size exposing (Size, sizeValue)

-------------------------------------------------------------------------------

type alias BoxTypeDescriptor = BoxTypeFactory -> BoxType

type BoxType
  = BoxType String
  | InheritBoxType
  | InitialBoxType
  | OtherBoxType Element

type alias BoxTypeFactory =
  {
    boxType: String -> BoxType
  , inherit: BoxType
  , initial: BoxType
  , other: Element -> BoxType
  }

boxTypeFactory : BoxTypeFactory
boxTypeFactory =
  {
    boxType str = BoxType str
  , inherit = InheritBoxType
  , initial = InitialBoxType
  , other valElement = OtherBoxType valElement
  }

boxTypeValue : BoxType -> Value 
boxTypeValue boxTypeValue =
  case boxTypeValue of
    BoxType str -> stringValue str
    InheritBoxType -> inheritValue
    InitialBoxType -> initialValue
    OtherBoxType valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias BoxShadowDescriptor sizedConstraint xSzTyp ySzTyp blurTyp spreadTyp
  = BoxShadowFactory xSzTyp ySzTyp blurTyp spreadTyp -> 
    BoxShadow sizedConstraint xSzTyp ySzTyp blurTyp spreadTyp

type BoxShadow sizedConstraint xSzTyp ySzTyp blurTyp spreadTyp
  = BoxShadow (Size xSzTyp, Size ySzTyp)
              ShadowColor
              (Blur blurTyp spreadTyp)
              Inset
  | NoBoxShadow
  | InitialBoxShadow
  | InheritBoxShadow
  | OtherBoxShadow Element

type Sized = Sized -- used in sizedConstraint constraint type in BoxShadow.

type Blur blurSizeType spreadSizeType
  = Blur (Size blurSizeType) (Size spreadSizeType)
  | NoBlur

type ShadowColor
  = ShadowColor CssColor
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
  , other: Element -> BoxShadow () xSzTyp ySzTyp blurSzTyp spreadSzTyp
  }

boxShadowFactory : BoxShadowFactory xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxShadowFactory =
  { sizedShadow xSize ySize = BoxShadow (xSize, ySize) NoShadowColor NoBlur NoInset
  , none = NoBoxShadow
  , initial = InitialBoxShadow
  , inherit = InheritBoxShadow
  , other valElement = OtherBoxShadow valElement
  }

boxShadowValue : BoxShadow sizedTyp xSzTyp ySzTyp blurSzTyp spreadSzTyp -> Value 
boxShadowValue boxShadow=
  case boxShadow of
    BoxShadow (xSize, ySize) shadowColor blur inset ->
      let xyValues = [ sizeValue xSize, sizeValue ySize ]
          blurValues = extractBlurValues blur
          colorValue = extractColorValue shadowColor
          insetValue = extractInsetValue inset
          valueListFactory = spaceListValue identity
      in valueListFactory (xyValues ++ blurValues ++ colorValue ++ insetValue)
    NoBoxShadow -> noneValue
    InitialBoxShadow -> initialValue
    InheritBoxShadow -> inheritValue
    OtherBoxShadow valElement -> otherValue valElement

extractColorValue : ShadowColor -> List Value
extractColorValue maybeColor =
  case maybeColor of
    NoShadowColor -> []
    ShadowColor color -> [ colorValue color ]

extractBlurValues : Blur b s -> List Value
extractBlurValues maybeBlur =
  case maybeBlur of
    NoBlur -> []
    Blur blurSize spreadSize ->
      [ sizeValue blurSize
      , sizeValue spreadSize
      ]

extractInsetValue : Inset -> List Value
extractInsetValue maybeInset =
  case maybeInset of
    NoInset -> []
    Inset ->  [ stringValue "inset" ]
