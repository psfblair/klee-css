module Css.Internal.Content 
  ( ContentDescriptor, ComposableContentDescriptor, contentFactory, contentValue
  , CounterControlFactory
  , CounterIncrementDescriptor, counterIncrementFactory
  , CounterResetDescriptor, counterResetFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.List as List
import Css.Internal.Property as Property

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
  | StringContent Property.Literal
  | UrlContent String
  | CounterContent String (Maybe List.ListStyleTypeDescriptor)
  | CountersContent String Property.Literal (Maybe List.ListStyleTypeDescriptor)
  | OpenQuoteContent
  | CloseQuoteContent
  | NoOpenQuoteContent
  | NoCloseQuoteContent
  | NoContent
  | NormalContent
  | InitialContent
  | InheritContent
  | UnsetContent
  | OtherContent Property.Value

type alias ContentFactory =
  { attributeContent : String -> ComposableContent
  , stringContent : String -> ComposableContent
  , urlContent : String -> ComposableContent
  , counter : String -> Maybe List.ListStyleTypeDescriptor -> ComposableContent
  , counters : String -> String -> Maybe List.ListStyleTypeDescriptor -> ComposableContent
  , openQuote : ComposableContent
  , closeQuote : ComposableContent
  , noOpenQuote : ComposableContent
  , noCloseQuote : ComposableContent
  , none_ : BareContent {}
  , normal_ : BareContent {}
  , initial_ : BareContent {}
  , inherit_ : BareContent {}
  , unset_ : BareContent {}
  , other_ : Property.Value -> ComposableContent
  }

contentFactory : ContentFactory
contentFactory =
  { attributeContent str = AttributeContent str |> toComposableContent
  , stringContent str = Property.toLiteral str |> StringContent |> toComposableContent
  , urlContent str = UrlContent str |> toComposableContent
  , counter name maybeStyle = CounterContent name maybeStyle |> toComposableContent
  , counters name separator maybeStyle = 
      CountersContent name (Property.toLiteral separator) maybeStyle |> toComposableContent
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

contentValue : BareContent a -> Property.Value 
contentValue theContent =
  let styleTypeValue maybeDescriptor =
    case maybeDescriptor of
      Nothing -> Property.emptyValue
      Just descriptor -> 
        [ Property.stringValue ",", descriptor List.listStyleTypeFactory ] 
        |> Property.concatenateValues
  in 
    case theContent.content of
      AttributeContent str -> 
        [ "attr(", str, ")" ] 
        |> List.map Property.stringValue 
        |> Property.concatenateValues
      StringContent literal -> Property.literalValue literal
      UrlContent urlString ->  -- The URL string doesn't get quoted.
        [ "url(", urlString, ")" ] 
        |> List.map Property.stringValue 
        |> Property.concatenateValues
      CounterContent name maybeStyleDescriptor ->
        [ Property.stringValue "counter("
        , Property.stringValue name
        , styleTypeValue maybeStyleDescriptor
        , Property.stringValue ")" 
        ] |> Property.concatenateValues
      CountersContent name separator maybeStyleDescriptor ->
        [ Property.stringValue "counters("
        , Property.stringValue name
        , Property.stringValue ","
        , Property.literalValue separator
        , styleTypeValue maybeStyleDescriptor
        , Property.stringValue ")" 
        ] |> Property.concatenateValues
      OpenQuoteContent -> Property.stringValue "open-quote"
      CloseQuoteContent -> Property.stringValue "close-quote"
      NoOpenQuoteContent -> Property.stringValue "no-open-quote"
      NoCloseQuoteContent -> Property.stringValue "no-close-quote"
      NoContent -> Common.noneValue
      NormalContent -> Common.normalValue
      InitialContent -> Common.initialValue
      InheritContent -> Common.inheritValue
      UnsetContent -> Common.unsetValue
      OtherContent val -> Common.otherValue val

-------------------------------------------------------------------------------

type alias CounterControlFactory a rec =  { rec | id_ : String -> a }

-------------------------------------------------------------------------------
type alias CounterIncrementDescriptor = CounterIncrementFactory -> Property.Value
  
type alias CounterIncrementFactory =
  { withStep : String -> Int -> Property.Value
  , id_ : String -> Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , none_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }  

counterIncrementFactory : CounterIncrementFactory
counterIncrementFactory =
  { withStep str step = 
      Property.spacePairValue Property.stringValue Property.intValue (str, step)
  , id_ str = Property.stringValue str
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , none_ = Common.noneValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }  

-------------------------------------------------------------------------------

type alias CounterResetDescriptor = CounterResetFactory -> Property.Value
  
type alias CounterResetFactory =
  { withInitialValue : String -> Int -> Property.Value
  , id_ : String -> Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , none_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }  

counterResetFactory : CounterResetFactory
counterResetFactory =
  { withInitialValue str initVal = 
      Property.spacePairValue Property.stringValue Property.intValue (str, initVal)
  , id_ str = Property.stringValue str
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , none_ = Common.noneValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }  
  
