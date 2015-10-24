module Css.Internal.Box
  ( BoxType, BoxTypeDescriptor, boxTypeFactory, boxTypeValueFactory
  , BoxShadow (..), BoxShadowDescriptor
  , Inset (..), ShadowColor (..), Blur (..), Sized
  , boxShadowFactory, boxShadowValueFactory
  ) where
  
import Css.Size exposing (Size)

import Css.Internal.Property exposing
  ( Value, ValueFactory
  , stringValueFactory, valueValueFactory, spaceListValueFactory
  )

import Css.Common exposing
  ( noneValue, inheritValue, initialValue, otherValue
  )
import Css.Color exposing (CssColor, colorValueFactory)
import Css.Size exposing (sizeValueFactory)

-------------------------------------------------------------------------------

type alias BoxTypeDescriptor = BoxTypeFactory -> BoxType

type BoxType
  = BoxType String
  | InheritBoxType
  | InitialBoxType
  | OtherBoxType String

type alias BoxTypeFactory =
  {
    boxType: String -> BoxType
  , inherit: BoxType
  , initial: BoxType
  , other: String -> BoxType
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
        InheritBoxType -> inheritValue
        InitialBoxType -> initialValue
        OtherBoxType value -> otherValue value
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
  | OtherBoxShadow String

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
  , other: String -> BoxShadow () xSzTyp ySzTyp blurSzTyp spreadSzTyp
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
        let xyValues = [ sizeValueFactory.value xSize, sizeValueFactory.value ySize ]
            blurValues = extractBlurValues blur
            colorValue = extractColorValue shadowColor
            insetValue = extractInsetValue inset
            valueListFactory = spaceListValueFactory valueValueFactory
        in valueListFactory.value (xyValues ++ blurValues ++ colorValue ++ insetValue)
      NoBoxShadow -> noneValue
      InitialBoxShadow -> initialValue
      InheritBoxShadow -> inheritValue
      OtherBoxShadow value -> otherValue value
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

-------------------------------------------------------------------------------
