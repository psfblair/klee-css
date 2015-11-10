module Css.Internal.Box
  ( BoxTypeDescriptor, boxTypeFactory
  , BoxShadow (..), BoxShadowDescriptor
  , Inset (..), ShadowColor (..), Blur (..), Sized
  , boxShadowFactory, boxShadowValue
  ) where
  
import Css.Internal.Property exposing (Value, stringValue, spaceListValue)
import Css.Internal.Common exposing
  ( noneValue, inheritValue, initialValue, otherValue)
  
import Css.Internal.Color exposing (CssColor, colorValue)
import Css.Internal.Size exposing (Size, sizeValue)

-------------------------------------------------------------------------------

type alias BoxTypeDescriptor = BoxTypeFactory -> Value

type alias BoxTypeFactory =
  {
    boxType: String -> Value
  , inherit_ : Value
  , initial_ : Value
  , other_ : Value -> Value
  }

boxTypeFactory : BoxTypeFactory
boxTypeFactory =
  {
    boxType str = stringValue str
  , inherit_ = inheritValue
  , initial_ = initialValue
  , other_ val = otherValue val
  }

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
  | OtherBoxShadow Value

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
  , other: Value -> BoxShadow () xSzTyp ySzTyp blurSzTyp spreadSzTyp
  }

boxShadowFactory : BoxShadowFactory xSzTyp ySzTyp blurSzTyp spreadSzTyp
boxShadowFactory =
  { sizedShadow xSize ySize = BoxShadow (xSize, ySize) NoShadowColor NoBlur NoInset
  , none = NoBoxShadow
  , initial = InitialBoxShadow
  , inherit = InheritBoxShadow
  , other val = OtherBoxShadow val
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
    OtherBoxShadow val -> otherValue val

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
