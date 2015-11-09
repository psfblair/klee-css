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
  , CounterIncrementDescriptor, counterIncrementFactory, counterIncrementValue
  , CounterResetDescriptor, counterResetFactory, counterResetValue
  ) where

import Css.Internal.Color exposing (ColorDescriptor, CssColor, colorValue)
import Css.Internal.Common exposing 
  ( initialValue, inheritValue, autoValue
  , noneValue, normalValue, unsetValue, otherValue)
import Css.Internal.List exposing (ListStyleType, listStyleTypeValue)
import Css.Internal.Position exposing (HorizontalSide, horizontalSideValue)
import Css.Internal.Property exposing 
  ( Value, Literal, toLiteral
  , concatenateValues, emptyValue, stringValue, literalValue, intValue
  , spacePairValue, spaceListValue
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

type alias TextShadowDescriptor a hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> TextShadow a hSz vSz blrSz

type alias CompositeTextShadowDescriptor hSz vSz blrSz = 
  TextShadowFactory hSz vSz blrSz -> CompositeTextShadow hSz vSz blrSz  

type alias TextShadow a hSz vSz blrSz = 
  { a | textShadow : TextShadowComponent hSz vSz blrSz }

type alias WithComponents = { withComponents : () }

type alias CompositeTextShadow hSz vSz blrSz = 
  TextShadow (WithComponents) hSz vSz blrSz

type TextShadowComponent hSz vSz blrSz
  = BaseShadow (Size hSz) (Size vSz) 
  | WithBlurRadius (Size blrSz) (TextShadowComponent hSz vSz blrSz)
  | WithColor CssColor (TextShadowComponent hSz vSz blrSz)
  | InitialTextShadow
  | InheritTextShadow
  | NoTextShadow
  | UnsetTextShadow
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
  , unset_ : TextShadow {} hSz vSz blrSz 
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
  , unset_ = UnsetTextShadow |> toSimpleShadow
  , other_ val = OtherTextShadow val |> toCompositeShadow
  }

textShadowValue : TextShadow a hSz vSz blrSz -> Value 
textShadowValue textShadow =
  case textShadow.textShadow of
    InitialTextShadow -> initialValue
    InheritTextShadow -> inheritValue
    NoTextShadow -> noneValue
    UnsetTextShadow -> unsetValue
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
  | UnsetTextIndent
  | OtherTextIndent Value
  
type alias TextIndentFactory a =
  { textIndent : Size a -> TextIndent a
  , indentEachLine : TextIndent a
  , hangingIndent : TextIndent a
  , initial_ : TextIndent a
  , inherit_ : TextIndent a
  , unset_ : TextIndent a
  , other_ : Value -> TextIndent a
  } 
  
textIndentFactory : TextIndentFactory a
textIndentFactory =
  { textIndent size = TextIndent size
  , indentEachLine = IndentEachLine
  , hangingIndent = HangingIndent
  , initial_ = InitialTextIndent
  , inherit_ = InheritTextIndent
  , unset_ = UnsetTextIndent
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
  = SideTextAlign HorizontalSide
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
  { alignWithSide : HorizontalSide -> TextAlign
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
    SideTextAlign side -> horizontalSideValue side
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
  | CounterContent String (Maybe ListStyleType)
  | CountersContent String Literal (Maybe ListStyleType)
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
      UrlContent urlString ->  -- The URL string doesn't get quoted.
        [ "url(", urlString, ")" ] |> List.map stringValue |> concatenateValues
      CounterContent name maybeStyle ->
        [ stringValue "counter("
        , stringValue name
        , styleTypeValue maybeStyle
        , stringValue ")" 
        ] |> concatenateValues
      CountersContent name separator maybeStyle ->
        [ stringValue "counters("
        , stringValue name
        , stringValue ","
        , literalValue separator
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
      UnsetContent -> unsetValue
      OtherContent val -> otherValue val

-------------------------------------------------------------------------------

type alias CounterControlFactory a rec =  { rec | id_ : String -> a }

-------------------------------------------------------------------------------
type alias CounterIncrementDescriptor =
  CounterIncrementFactory -> CounterIncrement

type CounterIncrement
  = CounterIncrement String
  | WithStep String Int
  | InitialCounterIncrement
  | InheritCounterIncrement
  | NoCounterIncrement
  | UnsetCounterIncrement
  | OtherCounterIncrement Value
  
type alias CounterIncrementFactory =
  { withStep : String -> Int -> CounterIncrement
  , id_ : String -> CounterIncrement
  , initial_ : CounterIncrement
  , inherit_ : CounterIncrement
  , none_ : CounterIncrement
  , unset_ : CounterIncrement
  , other_ : Value -> CounterIncrement
  }  

counterIncrementFactory : CounterIncrementFactory
counterIncrementFactory =
  { withStep str step = WithStep str step
  , id_ str = CounterIncrement str
  , initial_ = InitialCounterIncrement
  , inherit_ = InheritCounterIncrement
  , none_ = NoCounterIncrement
  , unset_ = UnsetCounterIncrement
  , other_ val = OtherCounterIncrement val
  }  

counterIncrementValue : CounterIncrement -> Value
counterIncrementValue counterIncrement =
  case counterIncrement of
    CounterIncrement str -> stringValue str
    WithStep str step -> spacePairValue stringValue intValue (str, step)
    InitialCounterIncrement -> initialValue
    InheritCounterIncrement -> inheritValue
    NoCounterIncrement -> noneValue
    UnsetCounterIncrement -> unsetValue
    OtherCounterIncrement val -> otherValue val

-------------------------------------------------------------------------------
type alias CounterResetDescriptor =
  CounterResetFactory -> CounterReset

type CounterReset
  = CounterReset String
  | WithInitialValue String Int
  | InitialCounterReset
  | InheritCounterReset
  | NoCounterReset
  | UnsetCounterReset
  | OtherCounterReset Value
  
type alias CounterResetFactory =
  { withInitialValue : String -> Int -> CounterReset
  , id_ : String -> CounterReset
  , initial_ : CounterReset
  , inherit_ : CounterReset
  , none_ : CounterReset
  , unset_ : CounterReset
  , other_ : Value -> CounterReset
  }  

counterResetFactory : CounterResetFactory
counterResetFactory =
  { withInitialValue str initVal = WithInitialValue str initVal
  , id_ str = CounterReset str
  , initial_ = InitialCounterReset
  , inherit_ = InheritCounterReset
  , none_ = NoCounterReset
  , unset_ = UnsetCounterReset
  , other_ val = OtherCounterReset val
  }  

counterResetValue : CounterReset -> Value
counterResetValue counterReset =
  case counterReset of
    CounterReset str -> stringValue str
    WithInitialValue str init -> spacePairValue stringValue intValue (str, init)
    InitialCounterReset -> initialValue
    InheritCounterReset -> inheritValue
    NoCounterReset -> noneValue
    UnsetCounterReset -> unsetValue
    OtherCounterReset val -> otherValue val
