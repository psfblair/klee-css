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
  , CounterControlDescriptor, counterControlFactory, counterControlValue
  ) where

import Css.Internal.Color exposing (ColorDescriptor, CssColor, colorValue)
import Css.Internal.Common exposing 
  ( initialValue, inheritValue, autoValue
  , noneValue, normalValue, unsetValue, otherValue)
import Css.Internal.List exposing (ListStyleType, listStyleTypeValue)
import Css.Internal.Position exposing (HorizontalSide, horizontalSideValue)
import Css.Internal.Property exposing 
  ( Value, Literal, toLiteral
  , concatenateValues, emptyValue, stringValue, literalValue
  , spaceListValue
  )
import Css.Internal.Size exposing (Size, sizeValue)
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

type alias TextShadowDescriptor a hSz vSz blrSz = TextShadowFactory hSz vSz blrSz -> TextShadow a hSz vSz blrSz

type alias CompositeTextShadowDescriptor hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> CompositeTextShadow hSz vSz blrSz  

type alias TextShadow a hSz vSz blrSz = { a | textShadow : TextShadowComponent hSz vSz blrSz }

type alias WithComponents = { withComponents : () }

type alias CompositeTextShadow hSz vSz blrSz = TextShadow (WithComponents) hSz vSz blrSz

type TextShadowComponent hSz vSz blrSz
  = BaseShadow (Size hSz) (Size vSz) 
  | WithBlurRadius (Size blrSz) (TextShadowComponent hSz vSz blrSz)
  | WithColor CssColor (TextShadowComponent hSz vSz blrSz)
  | InitialTextShadow
  | InheritTextShadow
  | NoTextShadow
  | OtherTextShadow Value

type alias TextShadowFactory hSz vSz blrSz =
  { baseShadow : Size hSz -> Size vSz -> CompositeTextShadow hSz vSz blrSz 
  , withBlurRadius : Size blrSz -> 
                     TextShadowComponent hSz vSz blrSz -> 
                     CompositeTextShadow hSz vSz blrSz 
  , withColor : CssColor -> 
                TextShadowComponent hSz vSz blrSz -> 
                CompositeTextShadow hSz vSz blrSz
  , initial_ : TextShadow {} hSz vSz blrSz 
  , inherit_ : TextShadow {} hSz vSz blrSz 
  , none_ : TextShadow {} hSz vSz blrSz 
  , other_ : Value -> CompositeTextShadow hSz vSz blrSz 
  }

toSimpleShadow : TextShadowComponent hSz vSz blrSz -> TextShadow {} hSz vSz blrSz
toSimpleShadow component = { textShadow = component }

toCompositeShadow : TextShadowComponent hSz vSz blrSz -> 
                    CompositeTextShadow hSz vSz blrSz
toCompositeShadow component =
  { textShadow = component
  , withComponents = ()
  }
  
textShadowFactory : TextShadowFactory hSz vSz blrSz
textShadowFactory =
  { baseShadow horizontal vertical = BaseShadow horizontal vertical |> toCompositeShadow
  , withBlurRadius radius inner = WithBlurRadius radius inner |> toCompositeShadow
  , withColor colour inner = WithColor colour inner |> toCompositeShadow
  , initial_ = InitialTextShadow |> toSimpleShadow
  , inherit_ = InheritTextShadow |> toSimpleShadow
  , none_ = NoTextShadow |> toSimpleShadow
  , other_ val = OtherTextShadow val |> toCompositeShadow
  }

textShadowValue : TextShadow a hSz vSz blrSz -> Value 
textShadowValue textShadow =
  case textShadow.textShadow of
    InitialTextShadow -> initialValue
    InheritTextShadow -> inheritValue
    NoTextShadow -> noneValue
    OtherTextShadow val -> otherValue val
    somethingElse -> textShadowValueRecursive somethingElse Nothing Nothing

textShadowValueRecursive : TextShadowComponent hSz vSz blrSz -> 
                           Maybe (Size blrSz) -> 
                           Maybe CssColor -> 
                           Value
textShadowValueRecursive component maybeSize maybeColor =
  -- If the TextShadowComponent combinators are called more than once,
  -- the last (outer) one wins. So if it's already set we don't reset it.
  case component of
    WithBlurRadius radius inner -> 
      case maybeSize of
        Just _ -> 
          textShadowValueRecursive inner maybeSize maybeColor
        Nothing -> 
          textShadowValueRecursive inner (Just radius) maybeColor
    WithColor colour inner -> 
      case maybeColor of
        Just _ -> 
          textShadowValueRecursive inner maybeSize maybeColor
        Nothing -> 
          textShadowValueRecursive inner maybeSize (Just colour)
    BaseShadow horizontal vertical -> 
      let horizontalValue = sizeValue horizontal
          verticalValue = sizeValue vertical
          maybeRadiusValue = Maybe.map sizeValue maybeSize
          maybeColorValue = Maybe.map colorValue maybeColor
          
          allValues = 
            [ (Just horizontalValue)
            , (Just verticalValue)
            , maybeRadiusValue
            , maybeColorValue
            ] |> List.filterMap identity
            
      in spaceListValue identity allValues
-------------------------------------------------------------------------------

type alias TextIndentDescriptor a =
  TextIndentFactory a -> TextIndent a
  
type TextIndent a
  = TextIndent (Size a)
  | IndentEachLine 
  | HangingIndent
  | InitialTextIndent
  | InheritTextIndent
  | OtherTextIndent Value
  
type alias TextIndentFactory a =
  { textIndent : Size a -> TextIndent a
  , indentEachLine : TextIndent a
  , hangingIndent : TextIndent a
  , initial_ : TextIndent a
  , inherit_ : TextIndent a
  , other_ : Value -> TextIndent a
  } 
  
textIndentFactory : TextIndentFactory a
textIndentFactory =
  { textIndent size = TextIndent size
  , indentEachLine = IndentEachLine
  , hangingIndent = HangingIndent
  , initial_ = InitialTextIndent
  , inherit_ = InheritTextIndent
  , other_ val = OtherTextIndent val
  }

textIndentValue : TextIndent a -> Value 
textIndentValue textIndent =
  case textIndent of
    TextIndent size -> sizeValue size
    IndentEachLine -> stringValue "each-line"
    HangingIndent -> stringValue "hanging"
    InitialTextIndent -> initialValue
    InheritTextIndent -> inheritValue
    OtherTextIndent val -> otherValue val

-------------------------------------------------------------------------------

type alias TextDirectionDescriptor =
  TextDirectionFactory -> TextDirection

type TextDirection 
    = RightToLeft
    | LeftToRight
    | InitialTextDirection
    | InheritTextDirection
    | OtherTextDirection Value

type alias TextDirectionFactory =
  { rightToLeft : TextDirection
  , leftToRight : TextDirection
  , initial_ : TextDirection
  , inherit_ : TextDirection
  , other_ : Value -> TextDirection
  } 

textDirectionFactory : TextDirectionFactory 
textDirectionFactory =
  { rightToLeft = RightToLeft
  , leftToRight = LeftToRight
  , initial_ = InitialTextDirection
  , inherit_ = InheritTextDirection
  , other_ val = OtherTextDirection val
  }

textDirectionValue : TextDirection -> Value 
textDirectionValue textDirection =
  case textDirection of
    RightToLeft -> stringValue "rtl"
    LeftToRight -> stringValue "ltr"
    InitialTextDirection -> initialValue
    InheritTextDirection -> inheritValue
    OtherTextDirection val -> otherValue val

-------------------------------------------------------------------------------

type alias TextAlignDescriptor =
  TextAlignFactory -> TextAlign
  
type TextAlign
  = SideTextAlign HorizontalSide
  | JustifyTextAlign
  | MatchParentTextAlign
  | StartTextAlign
  | EndTextAlign
  | InitialTextAlign
  | InheritTextAlign
  | OtherTextAlign Value

type alias TextAlignFactory =
  { alignWithSide : HorizontalSide -> TextAlign
  , justify : TextAlign
  , matchParent : TextAlign
  , start : TextAlign
  , end : TextAlign
  , initial_ : TextAlign
  , inherit_ : TextAlign
  , other_ : Value -> TextAlign
  }

textAlignFactory : TextAlignFactory
textAlignFactory =
  { alignWithSide side = SideTextAlign side
  , justify = JustifyTextAlign
  , matchParent = MatchParentTextAlign
  , start = StartTextAlign
  , end = EndTextAlign
  , initial_ = InitialTextAlign
  , inherit_ = InheritTextAlign
  , other_ val = OtherTextAlign val
  }

textAlignValue : TextAlign -> Value 
textAlignValue alignment =
  case alignment of
    SideTextAlign side -> horizontalSideValue side
    JustifyTextAlign -> stringValue "justify"
    MatchParentTextAlign -> stringValue "match-parent"
    StartTextAlign -> stringValue "start"
    EndTextAlign -> stringValue "end"
    InitialTextAlign -> initialValue
    InheritTextAlign -> inheritValue
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
  | OtherWhiteSpace Value

type alias WhiteSpaceFactory =
  { noWrap : WhiteSpace
  , pre : WhiteSpace
  , preWrap : WhiteSpace
  , preLine : WhiteSpace
  , initial_ : WhiteSpace
  , inherit_ : WhiteSpace
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
  | OtherTextDecoration Value

type alias TextDecorationFactory =
  { underline : TextDecoration
  , overline : TextDecoration
  , lineThrough : TextDecoration
  , blink : TextDecoration
  , none_ : TextDecoration
  , initial_ : TextDecoration
  , inherit_ : TextDecoration
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
  | OtherTextTransform Value

type alias TextTransformFactory =
  { capitalize : TextTransform
  , uppercase : TextTransform
  , lowercase : TextTransform
  , fullWidth : TextTransform
  , none_ : TextTransform
  , initial_ : TextTransform
  , inherit_ : TextTransform
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
  | UriContent Literal
  | UrlContent Literal
  | CounterContent String (Maybe ListStyleType)
  | CountersContent String String (Maybe ListStyleType)
  | OpenQuoteContent
  | CloseQuoteContent
  | NoOpenQuoteContent
  | NoCloseQuoteContent
  | NoContent
  | NormalContent
  | InitialContent
  | InheritContent
  | OtherContent Value

type alias ContentFactory =
  { attributeContent : String -> ComposableContent
  , stringContent : String -> ComposableContent
  , uriContent : String -> ComposableContent
  , urlContent : String -> ComposableContent
  , counter : String -> Maybe ListStyleType -> ComposableContent
  , counters : String -> String -> Maybe ListStyleType -> ComposableContent
  , openQuote : ComposableContent
  , closeQuote : ComposableContent
  , noOpenQuote : ComposableContent
  , noCloseQuote : ComposableContent
  , none_ : BareContent {}
  , normal_ : BareContent {}
  , initial_ : BareContent {}
  , inherit_ : BareContent {}
  , other_ : Value -> ComposableContent
  }

contentFactory : ContentFactory
contentFactory =
  { attributeContent str = AttributeContent str |> toComposableContent
  , stringContent str = toLiteral str |> StringContent |> toComposableContent
  , uriContent str = toLiteral str |> UriContent |> toComposableContent
  , urlContent str = toLiteral str |> UrlContent |> toComposableContent
  , counter name maybeStyle = CounterContent name maybeStyle |> toComposableContent
  , counters name separator maybeStyle = 
      CountersContent name separator maybeStyle |> toComposableContent
  , openQuote = OpenQuoteContent |> toComposableContent
  , closeQuote = CloseQuoteContent |> toComposableContent
  , noOpenQuote = NoOpenQuoteContent |> toComposableContent
  , noCloseQuote = NoCloseQuoteContent |> toComposableContent
  , none_ = NoContent |> toSimpleContent
  , normal_ = NormalContent |> toSimpleContent
  , initial_ = InitialContent |> toSimpleContent
  , inherit_ = InheritContent |> toSimpleContent
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
  let styleTypeValue styleMaybe =
    case styleMaybe of
      Nothing -> emptyValue
      Just styleType -> 
        [stringValue ",", listStyleTypeValue styleType] |> concatenateValues
  in 
    case theContent.content of
      AttributeContent str -> 
        [ stringValue "attr(", stringValue str, stringValue ")" ] 
        |> concatenateValues
      StringContent literal -> literalValue literal
      UriContent literal -> 
        [ stringValue "uri(", literalValue literal, stringValue ")" ]
        |> concatenateValues
      UrlContent literal -> 
        [ stringValue "url(", literalValue literal, stringValue ")" ]
        |> concatenateValues
      CounterContent name maybeStyle ->
        [ stringValue "counter("
        , stringValue name
        , styleTypeValue maybeStyle
        , stringValue ")" 
        ] |> concatenateValues
      CountersContent name separator maybeStyle ->
        [ stringValue "counter("
        , stringValue name
        , stringValue ","
        , stringValue separator
        , styleTypeValue maybeStyle
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
      OtherContent val -> otherValue val

-------------------------------------------------------------------------------
type alias CounterControlDescriptor =
  CounterControlFactory -> CounterControl

type CounterControl
  = CounterControl String
  | InitialCounterControl
  | InheritCounterControl
  | NoCounterControl
  | OtherCounterControl Value
  
type alias CounterControlFactory =
  { counterId : String -> CounterControl
  , initial_ : CounterControl
  , inherit_ : CounterControl
  , none_ : CounterControl
  , other_ : Value -> CounterControl
  }  

counterControlFactory : CounterControlFactory
counterControlFactory =
  { counterId str = CounterControl str
  , initial_ = InitialCounterControl
  , inherit_ = InheritCounterControl
  , none_ = NoCounterControl
  , other_ val = OtherCounterControl val
  }  

counterControlValue : CounterControl -> Value
counterControlValue counterControl =
  case counterControl of
    CounterControl str -> stringValue str
    InitialCounterControl -> initialValue
    InheritCounterControl -> inheritValue
    NoCounterControl -> noneValue
    OtherCounterControl val -> otherValue val