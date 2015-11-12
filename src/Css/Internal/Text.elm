module Css.Internal.Text
  ( TextRenderingDescriptor, textRenderingFactory, textRenderingValue
  , TextShadowDescriptor, CompositeTextShadowDescriptor
  , textShadowFactory, textShadowValue
  , TextIndentDescriptor, textIndentFactory, textIndentValue
  , TextDirectionDescriptor, textDirectionFactory, textDirectionValue
  , TextAlignDescriptor, textAlignFactory, textAlignValue
  , WhiteSpaceDescriptor, whiteSpaceFactory, whiteSpaceValue
  , TextDecorationDescriptor, textDecorationFactory, textDecorationValue
  , TextTransformDescriptor, textTransformFactory, textTransformValue
  , ContentDescriptor, ComposableContentDescriptor, contentFactory, contentValue
  , CounterControlFactory
  , CounterIncrementDescriptor, counterIncrementFactory
  , CounterResetDescriptor, counterResetFactory
  ) where

import Css.Internal.Color exposing (ColorDescriptor, CssColor, colorValue)
import Css.Internal.Common exposing 
  ( initialValue, inheritValue, autoValue
  , noneValue, normalValue, unsetValue, otherValue)
import Css.Internal.List exposing (ListStyleTypeDescriptor, listStyleTypeFactory)
import Css.Internal.Property exposing 
  ( Value, Literal, toLiteral
  , concatenateValues, emptyValue, stringValue, literalValue, intValue
  , spacePairValue, spaceListValue
  )

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides

-------------------------------------------------------------------------------

type alias TextRenderingDescriptor =
  TextRenderingFactory -> TextRendering
  
type TextRendering 
  = OptimizeSpeed 
  | OptimizeLegibility
  | GeometricPrecision
  | InitialTextRendering
  | InheritTextRendering
  | AutoTextRendering
  | UnsetTextRendering
  | OtherTextRendering Value

type alias TextRenderingFactory =
  { speedOptimize : TextRendering
  , legibilityOptimize : TextRendering
  , preciseGeometry : TextRendering
  , initial_ : TextRendering
  , inherit_ : TextRendering
  , auto_ : TextRendering
  , unset_ : TextRendering
  , other_ : Value -> TextRendering
  }

textRenderingFactory : TextRenderingFactory
textRenderingFactory =
  { speedOptimize = OptimizeSpeed
  , legibilityOptimize = OptimizeLegibility
  , preciseGeometry = GeometricPrecision
  , initial_ = InitialTextRendering
  , inherit_ = InheritTextRendering
  , auto_ = AutoTextRendering
  , unset_ = UnsetTextRendering
  , other_ val = OtherTextRendering val
  }

textRenderingValue : TextRendering -> Value 
textRenderingValue textRendering =
  case textRendering of
    OptimizeSpeed -> stringValue "optimizeSpeed"
    OptimizeLegibility -> stringValue "optimizeLegibility"
    GeometricPrecision -> stringValue "geometricPrecision"
    InitialTextRendering -> initialValue
    InheritTextRendering -> inheritValue
    AutoTextRendering -> autoValue
    UnsetTextRendering -> unsetValue
    OtherTextRendering val -> otherValue val

-------------------------------------------------------------------------------

type alias TextShadowDescriptor a hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> TextShadow a

type alias CompositeTextShadowDescriptor hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> CompositeTextShadow

type alias TextShadow a = { a | textShadow : TextShadowComponent }

type alias WithComponents = { withComponents : () }

type alias CompositeTextShadow = TextShadow WithComponents

type TextShadowComponent
  = BaseShadow Value Value
  | WithBlurRadius Value TextShadowComponent
  | WithColor CssColor TextShadowComponent
  | InitialTextShadow
  | InheritTextShadow
  | NoTextShadow
  | UnsetTextShadow
  | OtherTextShadow Value

type alias TextShadowFactory hSz vSz blrSz =
  { baseShadow : Linear.SizeDescriptor {} hSz -> 
                 Linear.SizeDescriptor {} vSz -> 
                 CompositeTextShadow
  , withBlurRadius : Linear.SizeDescriptor {} blrSz -> 
                     TextShadowComponent -> 
                     CompositeTextShadow
  , withColor : CssColor -> 
                TextShadowComponent -> 
                CompositeTextShadow
  , initial_ : TextShadow {}
  , inherit_ : TextShadow {}
  , none_ : TextShadow {}
  , unset_ : TextShadow {}
  , other_ : Value -> CompositeTextShadow
  }
  
textShadowFactory : TextShadowFactory hSz vSz blrSz
textShadowFactory =
  { baseShadow horizontalDescriptor verticalDescriptor = 
      let horizontal = Linear.sizeValue horizontalDescriptor
          vertical = Linear.sizeValue verticalDescriptor
      in BaseShadow horizontal vertical |> toCompositeShadow
  , withBlurRadius radiusDescriptor inner = 
      let radius = Linear.sizeValue radiusDescriptor
      in WithBlurRadius radius inner |> toCompositeShadow
  , withColor colour inner = WithColor colour inner |> toCompositeShadow
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

textShadowValue : TextShadow a -> Value 
textShadowValue textShadow =
  case textShadow.textShadow of
    InitialTextShadow -> initialValue
    InheritTextShadow -> inheritValue
    NoTextShadow -> noneValue
    UnsetTextShadow -> unsetValue
    OtherTextShadow val -> otherValue val
    somethingElse -> textShadowValueRecursive somethingElse Nothing Nothing

textShadowValueRecursive : TextShadowComponent -> 
                           Maybe Value -> 
                           Maybe CssColor -> 
                           Value
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
      let maybeColorValue = Maybe.map colorValue maybeColor
          
          allValues = 
            [ (Just horizontal)
            , (Just vertical)
            , maybeRadius
            , maybeColorValue
            ] |> List.filterMap identity
            
      in spaceListValue identity allValues
-------------------------------------------------------------------------------

type alias TextIndentDescriptor sz = TextIndentFactory sz -> TextIndent
  
type TextIndent
  = TextIndent Value
  | IndentEachLine 
  | HangingIndent
  | InitialTextIndent
  | InheritTextIndent
  | UnsetTextIndent
  | OtherTextIndent Value
  
type alias TextIndentFactory sz =
  { textIndent : Linear.SizeDescriptor {} sz -> TextIndent
  , indentEachLine : TextIndent
  , hangingIndent : TextIndent
  , initial_ : TextIndent
  , inherit_ : TextIndent
  , unset_ : TextIndent
  , other_ : Value -> TextIndent
  } 
  
textIndentFactory : TextIndentFactory sz
textIndentFactory =
  { textIndent sizeDescriptor = Linear.sizeValue sizeDescriptor |> TextIndent
  , indentEachLine = IndentEachLine
  , hangingIndent = HangingIndent
  , initial_ = InitialTextIndent
  , inherit_ = InheritTextIndent
  , unset_ = UnsetTextIndent
  , other_ val = OtherTextIndent val
  }

textIndentValue : TextIndent -> Value 
textIndentValue textIndent =
  case textIndent of
    TextIndent sizeValue -> sizeValue
    IndentEachLine -> stringValue "each-line"
    HangingIndent -> stringValue "hanging"
    InitialTextIndent -> initialValue
    InheritTextIndent -> inheritValue
    UnsetTextIndent -> unsetValue
    OtherTextIndent val -> otherValue val

-------------------------------------------------------------------------------

type alias TextDirectionDescriptor =
  TextDirectionFactory -> TextDirection

type TextDirection 
    = RightToLeft
    | LeftToRight
    | InitialTextDirection
    | InheritTextDirection
    | UnsetTextDirection
    | OtherTextDirection Value

type alias TextDirectionFactory =
  { rightToLeft : TextDirection
  , leftToRight : TextDirection
  , initial_ : TextDirection
  , inherit_ : TextDirection
  , unset_ : TextDirection
  , other_ : Value -> TextDirection
  } 

textDirectionFactory : TextDirectionFactory 
textDirectionFactory =
  { rightToLeft = RightToLeft
  , leftToRight = LeftToRight
  , initial_ = InitialTextDirection
  , inherit_ = InheritTextDirection
  , unset_ = UnsetTextDirection
  , other_ val = OtherTextDirection val
  }

textDirectionValue : TextDirection -> Value 
textDirectionValue textDirection =
  case textDirection of
    RightToLeft -> stringValue "rtl"
    LeftToRight -> stringValue "ltr"
    InitialTextDirection -> initialValue
    InheritTextDirection -> inheritValue
    UnsetTextDirection -> unsetValue
    OtherTextDirection val -> otherValue val

-------------------------------------------------------------------------------

type alias TextAlignDescriptor =
  TextAlignFactory -> TextAlign
  
type TextAlign
  = SideTextAlign Sides.HorizontalSide
  | JustifyTextAlign
  | JustifyAllTextAlign
  | MatchParentTextAlign
  | StartTextAlign
  | EndTextAlign
  | InitialTextAlign
  | InheritTextAlign
  | UnsetTextAlign
  | OtherTextAlign Value

type alias TextAlignFactory =
  { alignWithSide : Sides.HorizontalSide -> TextAlign
  , justify : TextAlign
  , justifyAll : TextAlign
  , matchParent : TextAlign
  , start : TextAlign
  , end : TextAlign
  , initial_ : TextAlign
  , inherit_ : TextAlign
  , unset_ : TextAlign
  , other_ : Value -> TextAlign
  }

textAlignFactory : TextAlignFactory
textAlignFactory =
  { alignWithSide side = SideTextAlign side
  , justify = JustifyTextAlign
  , justifyAll = JustifyAllTextAlign
  , matchParent = MatchParentTextAlign
  , start = StartTextAlign
  , end = EndTextAlign
  , initial_ = InitialTextAlign
  , inherit_ = InheritTextAlign
  , unset_ = UnsetTextAlign
  , other_ val = OtherTextAlign val
  }

textAlignValue : TextAlign -> Value 
textAlignValue alignment =
  case alignment of
    SideTextAlign side -> Sides.horizontalSideValue side
    JustifyTextAlign -> stringValue "justify"
    JustifyAllTextAlign -> stringValue "justify-all"
    MatchParentTextAlign -> stringValue "match-parent"
    StartTextAlign -> stringValue "start"
    EndTextAlign -> stringValue "end"
    InitialTextAlign -> initialValue
    InheritTextAlign -> inheritValue
    UnsetTextAlign -> unsetValue
    OtherTextAlign val -> otherValue val
    
-------------------------------------------------------------------------------

type alias WhiteSpaceDescriptor =
  WhiteSpaceFactory -> WhiteSpace
  
type WhiteSpace
  = NoWrapWhiteSpace
  | PreWhiteSpace
  | PreWrapWhiteSpace
  | PreLineWhiteSpace
  | InitialWhiteSpace
  | InheritWhiteSpace
  | NormalWhiteSpace
  | UnsetWhiteSpace
  | OtherWhiteSpace Value

type alias WhiteSpaceFactory =
  { noWrap : WhiteSpace
  , pre : WhiteSpace
  , preWrap : WhiteSpace
  , preLine : WhiteSpace
  , initial_ : WhiteSpace
  , inherit_ : WhiteSpace
  , normal_ : WhiteSpace
  , unset_ : WhiteSpace
  , other_ : Value -> WhiteSpace
  }

whiteSpaceFactory : WhiteSpaceFactory
whiteSpaceFactory =
  { noWrap = NoWrapWhiteSpace
  , pre = PreWhiteSpace
  , preWrap = PreWrapWhiteSpace
  , preLine = PreLineWhiteSpace
  , initial_ = InitialWhiteSpace
  , inherit_ = InheritWhiteSpace
  , normal_ = NormalWhiteSpace
  , unset_ = UnsetWhiteSpace
  , other_ val = OtherWhiteSpace val
  }

whiteSpaceValue : WhiteSpace -> Value 
whiteSpaceValue theWhiteSpace =
  case theWhiteSpace of
    NoWrapWhiteSpace -> stringValue "nowrap"
    PreWhiteSpace -> stringValue "pre"
    PreWrapWhiteSpace -> stringValue "pre-wrap"
    PreLineWhiteSpace -> stringValue "pre-line"
    InitialWhiteSpace -> initialValue
    InheritWhiteSpace -> inheritValue
    NormalWhiteSpace -> normalValue
    UnsetWhiteSpace -> unsetValue
    OtherWhiteSpace val -> otherValue val

-------------------------------------------------------------------------------

type alias TextDecorationDescriptor =
  TextDecorationFactory -> TextDecoration
  
type TextDecoration
  = UnderlineTextDecoration
  | OverlineTextDecoration
  | LineThroughTextDecoration
  | BlinkTextDecoration
  | NoTextDecoration
  | InitialTextDecoration
  | InheritTextDecoration
  | UnsetTextDecoration
  | OtherTextDecoration Value

type alias TextDecorationFactory =
  { underline : TextDecoration
  , overline : TextDecoration
  , lineThrough : TextDecoration
  , blink : TextDecoration
  , none_ : TextDecoration
  , initial_ : TextDecoration
  , inherit_ : TextDecoration
  , unset_ : TextDecoration
  , other_ : Value -> TextDecoration
  }

textDecorationFactory : TextDecorationFactory
textDecorationFactory =
  { underline = UnderlineTextDecoration
  , overline = OverlineTextDecoration
  , lineThrough = LineThroughTextDecoration
  , blink = BlinkTextDecoration
  , none_ = NoTextDecoration
  , initial_ = InitialTextDecoration
  , inherit_ = InheritTextDecoration
  , unset_ = UnsetTextDecoration
  , other_ val = OtherTextDecoration val
  }

textDecorationValue : TextDecoration -> Value 
textDecorationValue theTextDecoration =
  case theTextDecoration of
    UnderlineTextDecoration -> stringValue "underline"
    OverlineTextDecoration -> stringValue "overline"
    LineThroughTextDecoration -> stringValue "line-through"
    BlinkTextDecoration -> stringValue "blink"
    NoTextDecoration -> noneValue
    InitialTextDecoration -> initialValue
    InheritTextDecoration -> inheritValue
    UnsetTextDecoration -> unsetValue
    OtherTextDecoration val -> otherValue val

-------------------------------------------------------------------------------

type alias TextTransformDescriptor =
  TextTransformFactory -> TextTransform
  
type TextTransform
  = CapitalizeTextTransform
  | UppercaseTextTransform
  | LowercaseTextTransform
  | FullWidthTextTransform
  | NoTextTransform
  | InitialTextTransform
  | InheritTextTransform
  | UnsetTextTransform
  | OtherTextTransform Value

type alias TextTransformFactory =
  { capitalize : TextTransform
  , uppercase : TextTransform
  , lowercase : TextTransform
  , fullWidth : TextTransform
  , none_ : TextTransform
  , initial_ : TextTransform
  , inherit_ : TextTransform
  , unset_ : TextTransform
  , other_ : Value -> TextTransform
  }

textTransformFactory : TextTransformFactory
textTransformFactory =
  { capitalize = CapitalizeTextTransform
  , uppercase = UppercaseTextTransform
  , lowercase = LowercaseTextTransform
  , fullWidth = FullWidthTextTransform
  , none_ = NoTextTransform
  , initial_ = InitialTextTransform
  , inherit_ = InheritTextTransform
  , unset_ = UnsetTextTransform
  , other_ val = OtherTextTransform val
  }

textTransformValue : TextTransform -> Value 
textTransformValue theTextTransform =
  case theTextTransform of
    CapitalizeTextTransform -> stringValue "capitalize"
    UppercaseTextTransform -> stringValue "uppercase"
    LowercaseTextTransform -> stringValue "lowercase"
    FullWidthTextTransform -> stringValue "full-width"
    NoTextTransform -> noneValue
    InitialTextTransform -> initialValue
    InheritTextTransform -> inheritValue
    UnsetTextTransform -> unsetValue
    OtherTextTransform val -> otherValue val

-------------------------------------------------------------------------------

type alias ContentDescriptor a =
  ContentFactory -> BareContent a

type alias ComposableContentDescriptor =
  ContentFactory -> ComposableContent
  
type alias BareContent a = { a | content: Content }

type alias ThatIsComposable = { composable: () }
  
type alias ComposableContent = BareContent ThatIsComposable
  
type Content
  = AttributeContent String
  | StringContent Literal
  | UrlContent String
  | CounterContent String (Maybe ListStyleTypeDescriptor)
  | CountersContent String Literal (Maybe ListStyleTypeDescriptor)
  | OpenQuoteContent
  | CloseQuoteContent
  | NoOpenQuoteContent
  | NoCloseQuoteContent
  | NoContent
  | NormalContent
  | InitialContent
  | InheritContent
  | UnsetContent
  | OtherContent Value

type alias ContentFactory =
  { attributeContent : String -> ComposableContent
  , stringContent : String -> ComposableContent
  , urlContent : String -> ComposableContent
  , counter : String -> Maybe ListStyleTypeDescriptor -> ComposableContent
  , counters : String -> String -> Maybe ListStyleTypeDescriptor -> ComposableContent
  , openQuote : ComposableContent
  , closeQuote : ComposableContent
  , noOpenQuote : ComposableContent
  , noCloseQuote : ComposableContent
  , none_ : BareContent {}
  , normal_ : BareContent {}
  , initial_ : BareContent {}
  , inherit_ : BareContent {}
  , unset_ : BareContent {}
  , other_ : Value -> ComposableContent
  }

contentFactory : ContentFactory
contentFactory =
  { attributeContent str = AttributeContent str |> toComposableContent
  , stringContent str = toLiteral str |> StringContent |> toComposableContent
  , urlContent str = UrlContent str |> toComposableContent
  , counter name maybeStyle = CounterContent name maybeStyle |> toComposableContent
  , counters name separator maybeStyle = 
      CountersContent name (toLiteral separator) maybeStyle |> toComposableContent
  , openQuote = OpenQuoteContent |> toComposableContent
  , closeQuote = CloseQuoteContent |> toComposableContent
  , noOpenQuote = NoOpenQuoteContent |> toComposableContent
  , noCloseQuote = NoCloseQuoteContent |> toComposableContent
  , none_ = NoContent |> toSimpleContent
  , normal_ = NormalContent |> toSimpleContent
  , initial_ = InitialContent |> toSimpleContent
  , inherit_ = InheritContent |> toSimpleContent
  , unset_ = UnsetContent |> toSimpleContent
  , other_ val = OtherContent val |> toComposableContent
  }

toSimpleContent : Content -> BareContent {}
toSimpleContent theContent = { content = theContent }

toComposableContent : Content -> ComposableContent
toComposableContent theContent = 
  { content = theContent
  , composable = ()
  }

contentValue : BareContent a -> Value 
contentValue theContent =
  let styleTypeValue maybeDescriptor =
    case maybeDescriptor of
      Nothing -> emptyValue
      Just descriptor -> 
        [ stringValue ",", descriptor listStyleTypeFactory ] |> concatenateValues
  in 
    case theContent.content of
      AttributeContent str -> 
        [ stringValue "attr(", stringValue str, stringValue ")" ] 
        |> concatenateValues
      StringContent literal -> literalValue literal
      UrlContent urlString ->  -- The URL string doesn't get quoted.
        [ "url(", urlString, ")" ] |> List.map stringValue |> concatenateValues
      CounterContent name maybeStyleDescriptor ->
        [ stringValue "counter("
        , stringValue name
        , styleTypeValue maybeStyleDescriptor
        , stringValue ")" 
        ] |> concatenateValues
      CountersContent name separator maybeStyleDescriptor ->
        [ stringValue "counters("
        , stringValue name
        , stringValue ","
        , literalValue separator
        , styleTypeValue maybeStyleDescriptor
        , stringValue ")" 
        ] |> concatenateValues
      OpenQuoteContent -> stringValue "open-quote"
      CloseQuoteContent -> stringValue "close-quote"
      NoOpenQuoteContent -> stringValue "no-open-quote"
      NoCloseQuoteContent -> stringValue "no-close-quote"
      NoContent -> noneValue
      NormalContent -> normalValue
      InitialContent -> initialValue
      InheritContent -> inheritValue
      UnsetContent -> unsetValue
      OtherContent val -> otherValue val

-------------------------------------------------------------------------------

type alias CounterControlFactory a rec =  { rec | id_ : String -> a }

-------------------------------------------------------------------------------
type alias CounterIncrementDescriptor = CounterIncrementFactory -> Value
  
type alias CounterIncrementFactory =
  { withStep : String -> Int -> Value
  , id_ : String -> Value
  , initial_ : Value
  , inherit_ : Value
  , none_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }  

counterIncrementFactory : CounterIncrementFactory
counterIncrementFactory =
  { withStep str step = spacePairValue stringValue intValue (str, step)
  , id_ str = stringValue str
  , initial_ = initialValue
  , inherit_ = inheritValue
  , none_ = noneValue
  , unset_ = unsetValue
  , other_ val = otherValue val
  }  

-------------------------------------------------------------------------------
type alias CounterResetDescriptor = CounterResetFactory -> Value
  
type alias CounterResetFactory =
  { withInitialValue : String -> Int -> Value
  , id_ : String -> Value
  , initial_ : Value
  , inherit_ : Value
  , none_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }  

counterResetFactory : CounterResetFactory
counterResetFactory =
  { withInitialValue str initVal = spacePairValue stringValue intValue (str, initVal)
  , id_ str = stringValue str
  , initial_ = initialValue
  , inherit_ = inheritValue
  , none_ = noneValue
  , unset_ = unsetValue
  , other_ val = otherValue val
  }  
