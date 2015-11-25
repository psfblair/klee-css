module Css.Internal.Typography.Text.Shadow
  ( TextShadowDescriptor, CompositeTextShadowDescriptor
  , textShadowFactory, textShadowValue
  ) where

import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias TextShadow rec = { rec | textShadow : TextShadowComponent }

type alias TextShadowDescriptor rec hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> TextShadow rec

type alias CompositeTextShadowDescriptor hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> CompositeTextShadow

type alias CompositeTextShadow = TextShadow WithComponents

type alias WithComponents = { withComponents : () }

type TextShadowComponent
  = BaseShadow Property.Value Property.Value
  | WithBlurRadius Property.Value TextShadowComponent
  | WithColor Property.Value TextShadowComponent
  | InitialTextShadow
  | InheritTextShadow
  | NoTextShadow
  | UnsetTextShadow
  | OtherTextShadow Property.Value

type alias NubTextShadowFactory hSz vSz blrSz rec =
  { rec | baseShadow : Linear.NubSizeDescriptor {} hSz -> 
                       Linear.NubSizeDescriptor {} vSz -> 
                       CompositeTextShadow
        , withBlurRadius : Linear.NubSizeDescriptor {} blrSz -> 
                           TextShadowComponent -> 
                           CompositeTextShadow
        , withColor : Color.NubColorDescriptor {} -> 
                      TextShadowComponent -> 
                      CompositeTextShadow
        , other_ : Property.Value -> CompositeTextShadow
  }

type alias TextShadowFactory hSz vSz blrSz =
  NubTextShadowFactory hSz vSz blrSz
    (Common.Initial (TextShadow {})
      (Common.Inherit (TextShadow {})
        (Common.Unset (TextShadow {})
          (Common.None (TextShadow {}) {}))))

textShadowFactory : TextShadowFactory hSz vSz blrSz
textShadowFactory =
  { baseShadow horizontalDescriptor verticalDescriptor = 
      let horizontal = horizontalDescriptor Linear.nubSizeFactory 
          vertical = verticalDescriptor Linear.nubSizeFactory
      in BaseShadow horizontal vertical |> toCompositeShadow
  , withBlurRadius radiusDescriptor inner = 
      let radius = radiusDescriptor Linear.nubSizeFactory
      in WithBlurRadius radius inner |> toCompositeShadow
  , withColor colorDescriptor inner = 
      let colorValue = colorDescriptor Color.nubColorFactory
      in WithColor colorValue inner |> toCompositeShadow
  , initial_ = InitialTextShadow |> toSimpleShadow
  , inherit_ = InheritTextShadow |> toSimpleShadow
  , none_ = NoTextShadow |> toSimpleShadow
  , unset_ = UnsetTextShadow |> toSimpleShadow
  , other_ val = OtherTextShadow val |> toCompositeShadow
  }

toSimpleShadow : TextShadowComponent -> TextShadow {}
toSimpleShadow component = { textShadow = component }

toCompositeShadow : TextShadowComponent -> CompositeTextShadow
toCompositeShadow component =
  { textShadow = component
  , withComponents = ()
  }

textShadowValue : TextShadow rec -> Property.Value 
textShadowValue textShadow =
  case textShadow.textShadow of
    InitialTextShadow -> Common.initialValue
    InheritTextShadow -> Common.inheritValue
    NoTextShadow -> Common.noneValue
    UnsetTextShadow -> Common.unsetValue
    OtherTextShadow val -> Common.otherValue val
    somethingElse -> textShadowValueRecursive somethingElse Nothing Nothing

textShadowValueRecursive : TextShadowComponent -> 
                           Maybe Property.Value -> 
                           Maybe Property.Value -> 
                           Property.Value
textShadowValueRecursive component maybeRadius maybeColor =
  -- If the TextShadowComponent combinators are called more than once,
  -- the last (outer) one wins. So if it's already set we don't reset it.
  case component of
    WithBlurRadius radius inner -> 
      case maybeRadius of
        Just _ -> 
          textShadowValueRecursive inner maybeRadius maybeColor
        Nothing -> 
          textShadowValueRecursive inner (Just radius) maybeColor
    WithColor colour inner -> 
      case maybeColor of
        Just _ -> 
          textShadowValueRecursive inner maybeRadius maybeColor
        Nothing -> 
          textShadowValueRecursive inner maybeRadius (Just colour)
    BaseShadow horizontal vertical -> 
      let allValues = 
            [ (Just horizontal)
            , (Just vertical)
            , maybeRadius
            , maybeColor
            ] |> List.filterMap identity
            
      in Property.spaceListValue identity allValues
