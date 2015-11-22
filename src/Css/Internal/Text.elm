module Css.Internal.Text
  ( TextRenderingDescriptor, textRenderingFactory
  , TextShadowDescriptor, CompositeTextShadowDescriptor
  , textShadowFactory, textShadowValue
  , TextIndentDescriptor, textIndentFactory
  , TextDirectionDescriptor, textDirectionFactory
  , TextAlignDescriptor, textAlignFactory
  , WhiteSpaceDescriptor, whiteSpaceFactory
  , TextDecorationDescriptor, textDecorationFactory
  , TextTransformDescriptor, textTransformFactory
  ) where

import Css.Internal.Color as Color
import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Geometry.Sides as Sides

-------------------------------------------------------------------------------

type alias TextRenderingDescriptor = TextRenderingFactory -> Property.Value
  
type alias NubTextRenderingFactory rec =
  { rec | speedOptimize : Property.Value
        , legibilityOptimize : Property.Value
        , preciseGeometry : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubTextRenderingFactory : NubTextRenderingFactory {}
nubTextRenderingFactory =
  { speedOptimize = Property.stringValue "optimizeSpeed"
  , legibilityOptimize = Property.stringValue "optimizeLegibility"
  , preciseGeometry = Property.stringValue "geometricPrecision"
  , other_ val = Common.otherValue val
  }

type alias TextRenderingFactory =
  NubTextRenderingFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))
  
textRenderingFactory : TextRenderingFactory
textRenderingFactory =
  let withAuto = { nubTextRenderingFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto

-------------------------------------------------------------------------------

type alias TextShadowDescriptor rec hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> TextShadow rec

type alias CompositeTextShadowDescriptor hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> CompositeTextShadow

type alias TextShadow rec = { rec | textShadow : TextShadowComponent }

type alias WithComponents = { withComponents : () }

type alias CompositeTextShadow = TextShadow WithComponents

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
-------------------------------------------------------------------------------

type alias TextIndentDescriptor sz = TextIndentFactory sz -> Property.Value

type alias NubTextIndentFactory sz rec =
  { rec | textIndent : Linear.NubSizeDescriptor {} sz -> Property.Value
        , indentEachLine : Property.Value
        , hangingIndent : Property.Value
        , other_ : Property.Value -> Property.Value
  } 
  
nubTextIndentFactory : NubTextIndentFactory sz {}
nubTextIndentFactory =
  { textIndent sizeDescriptor = sizeDescriptor Linear.nubSizeFactory
  , indentEachLine = Property.stringValue "each-line"
  , hangingIndent = Property.stringValue "hanging"
  , other_ val = Common.otherValue val
  }

type alias TextIndentFactory sz =
  NubTextIndentFactory sz
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

textIndentFactory : TextIndentFactory sz
textIndentFactory =
  Common.addCommonValues nubTextIndentFactory

-------------------------------------------------------------------------------

type alias TextDirectionDescriptor = TextDirectionFactory -> Property.Value

type alias NubTextDirectionFactory rec =
  { rec | rightToLeft : Property.Value
        , leftToRight : Property.Value
        , other_ : Property.Value -> Property.Value
  } 
  
nubTextDirectionFactory : NubTextDirectionFactory {}
nubTextDirectionFactory =
  { rightToLeft = Property.stringValue "rtl"
  , leftToRight = Property.stringValue "ltr"
  , other_ val = Common.otherValue val
  }

type alias TextDirectionFactory =
  NubTextDirectionFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

textDirectionFactory : TextDirectionFactory 
textDirectionFactory =
  Common.addCommonValues nubTextDirectionFactory

-------------------------------------------------------------------------------

type alias TextAlignDescriptor = TextAlignFactory -> Property.Value

type alias NubTextAlignFactory rec =
  { rec | alignWithSide : Sides.HorizontalSide -> Property.Value
        , justify : Property.Value
        , justifyAll : Property.Value
        , matchParent : Property.Value
        , start : Property.Value
        , end : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubTextAlignFactory : NubTextAlignFactory {}
nubTextAlignFactory =
  { alignWithSide side = Sides.horizontalSideValue side
  , justify = Property.stringValue "justify"
  , justifyAll = Property.stringValue "justify-all"
  , matchParent = Property.stringValue "match-parent"
  , start = Property.stringValue "start"
  , end = Property.stringValue "end"
  , other_ val = Common.otherValue val
  }

type alias TextAlignFactory =
  NubTextAlignFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

textAlignFactory : TextAlignFactory
textAlignFactory =
  Common.addCommonValues nubTextAlignFactory
  
-------------------------------------------------------------------------------

type alias WhiteSpaceDescriptor = WhiteSpaceFactory -> Property.Value
  
type alias NubWhiteSpaceFactory rec =
  { rec | noWrap : Property.Value
        , pre : Property.Value
        , preWrap : Property.Value
        , preLine : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubWhiteSpaceFactory : NubWhiteSpaceFactory {}
nubWhiteSpaceFactory =
  { noWrap = Property.stringValue "nowrap"
  , pre = Property.stringValue "pre"
  , preWrap = Property.stringValue "pre-wrap"
  , preLine = Property.stringValue "pre-line"
  , other_ val = Common.otherValue val
  }

type alias WhiteSpaceFactory =
  NubWhiteSpaceFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value {}))))

whiteSpaceFactory : WhiteSpaceFactory
whiteSpaceFactory =
  let withNormal = { nubWhiteSpaceFactory | normal_ = Common.normalValue }
  in Common.addCommonValues withNormal

-------------------------------------------------------------------------------

type alias TextDecorationDescriptor = TextDecorationFactory -> Property.Value

type alias NubTextDecorationFactory rec =
  { rec | underline : Property.Value
        , overline : Property.Value
        , lineThrough : Property.Value
        , blink : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubTextDecorationFactory : NubTextDecorationFactory {}
nubTextDecorationFactory =
  { underline = Property.stringValue "underline"
  , overline = Property.stringValue "overline"
  , lineThrough = Property.stringValue "line-through"
  , blink = Property.stringValue "blink"
  , other_ val = Common.otherValue val
  }

type alias TextDecorationFactory =
  NubTextDecorationFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))
  
textDecorationFactory : TextDecorationFactory
textDecorationFactory =
  let withNone = { nubTextDecorationFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone

-------------------------------------------------------------------------------

type alias TextTransformDescriptor = TextTransformFactory -> Property.Value

type alias NubTextTransformFactory rec =
  { rec | capitalize : Property.Value
        , uppercase : Property.Value
        , lowercase : Property.Value
        , fullWidth : Property.Value
        , other_ : Property.Value -> Property.Value
  }

type alias TextTransformFactory =
  NubTextTransformFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

nubTextTransformFactory : NubTextTransformFactory {}
nubTextTransformFactory =
  { capitalize = Property.stringValue "capitalize"
  , uppercase = Property.stringValue "uppercase"
  , lowercase = Property.stringValue "lowercase"
  , fullWidth = Property.stringValue "full-width"
  , other_ val = Common.otherValue val
  }

textTransformFactory : TextTransformFactory
textTransformFactory =
  let withNone = { nubTextTransformFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone
