module Css.Internal.List 
  ( ListStyleTypeDescriptor, listStyleTypeFactory
  , ListStylePositionDescriptor, listStylePositionFactory
  , ListStyleImageDescriptor, listStyleImageFactory
  , ListStyleDescriptor, ComposedListStyleDescriptor, listStyleValue
  , initialListStyleFactory
  , ListStyleComponents (..), adjoinListStyle 
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property exposing 
  ( Value, Literal, toLiteral, concatenateValues
  , literalValue, stringValue, spaceListValue
  )
-------------------------------------------------------------------------------

type alias ListStyleTypeDescriptor = ListStyleTypeFactory -> Value
  
type alias ListStyleTypeFactory =
  { disc : Value
  , armenian : Value
  , circleListStyleType : Value
  , cjkIdeographic : Value
  , decimal : Value
  , decimalLeadingZero : Value
  , georgian : Value
  , hebrew : Value
  , hiragana : Value
  , hiraganaIroha : Value
  , katakana : Value
  , katakanaIroha : Value
  , lowerAlpha : Value
  , lowerGreek : Value
  , lowerLatin : Value
  , lowerRoman : Value
  , square : Value
  , upperAlpha : Value
  , upperLatin : Value
  , upperRoman : Value
  , initial_ : Value
  , inherit_ : Value
  , unset_   : Value
  , none_ : Value
  , other_ : Value -> Value
  }

listStyleTypeFactory : ListStyleTypeFactory
listStyleTypeFactory =
  { disc = stringValue "disc"
  , armenian = stringValue "armenian"
  , circleListStyleType = stringValue "circle"
  , cjkIdeographic = stringValue "cjk-ideographic"
  , decimal = stringValue "decimal"
  , decimalLeadingZero = stringValue "decimal-leading-zero"
  , georgian = stringValue "georgian"
  , hebrew = stringValue "hebrew"
  , hiragana = stringValue "hiragana"
  , hiraganaIroha = stringValue "hiragana-iroha"
  , katakana = stringValue "katakana"
  , katakanaIroha = stringValue "katakana-iroha"
  , lowerAlpha = stringValue "lower-alpha"
  , lowerGreek = stringValue "lower-greek"
  , lowerLatin = stringValue "lower-latin"
  , lowerRoman = stringValue "lower-roman"
  , square = stringValue "square"
  , upperAlpha = stringValue "upper-alpha"
  , upperLatin = stringValue "upper-latin"
  , upperRoman = stringValue "upper-roman"
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_   = Common.unsetValue
  , none_    = Common.noneValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias ListStylePositionDescriptor = ListStylePositionFactory -> Value
  
type alias ListStylePositionFactory =
  { inside : Value
  , outside : Value
  , initial_ : Value
  , inherit_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }

listStylePositionFactory : ListStylePositionFactory
listStylePositionFactory =
  { inside = stringValue "inside"
  , outside = stringValue "outside"
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_   = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias ListStyleImageDescriptor = ListStyleImageFactory -> Value
  
type alias ListStyleImageFactory =
  { url : String -> Value
  , initial_ : Value
  , inherit_ : Value
  , none_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }

listStyleImageFactory : ListStyleImageFactory
listStyleImageFactory =
  { url urlString =
      let urlValue = toLiteral urlString |> literalValue
      in [ stringValue "url(", urlValue, stringValue ")" ] |> concatenateValues
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , none_ = Common.noneValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias ListStyle rec = 
  { rec | listStyle : ListStyleAlternative  }

type ListStyleAlternative
  = CompositeListStyle ListStyleComponents
  | InitialListStyle
  | InheritListStyle
  | UnsetListStyle
  | OtherListStyle Value

type ListStyleComponents
  = NoListStyleComponents
  | WithStyleType Value ListStyleComponents
  | WithStylePosition Value ListStyleComponents
  | WithStyleImage Value ListStyleComponents

type alias ListStyleDescriptor rec = ListStyleFactory {} -> ListStyle rec
  
type alias ComposedListStyleDescriptor rec = 
  { rec | listStyle : ListStyleAlternative
        , styleComponents : ListStyleComponents } ->
  ListStyleFactory {}

type alias ListStyleFactory rec =
  ListStyle 
    (WithListStyleComponents
      (Common.Initial (ListStyle {})
        (Common.Inherit (ListStyle {})
          (Common.Unset (ListStyle {})
            (Common.Other (ListStyle {}) rec)))))

type alias WithListStyleComponents rec = 
  { rec | styleComponents : ListStyleComponents }

initialListStyleFactory : ListStyleFactory {}
initialListStyleFactory =
  { listStyle       = CompositeListStyle NoListStyleComponents
  , styleComponents = NoListStyleComponents
  , initial_    = { listStyle = InitialListStyle   }
  , inherit_    = { listStyle = InheritListStyle   }
  , unset_      = { listStyle = UnsetListStyle     }
  , other_ val  = { listStyle = OtherListStyle val }
  }

adjoinListStyle : ListStyleComponents -> ListStyleFactory {}
adjoinListStyle newComponents = 
  { initialListStyleFactory | listStyle <- CompositeListStyle newComponents
                            , styleComponents <- newComponents }

listStyleValue : ListStyleAlternative -> Value
listStyleValue style =
  case style of
    InitialListStyle   -> Common.initialValue
    InheritListStyle   -> Common.inheritValue
    UnsetListStyle     -> Common.unsetValue
    OtherListStyle val -> Common.otherValue val
    CompositeListStyle components -> componentsToValue components

componentsToValue : ListStyleComponents -> Value
componentsToValue components = 
  componentsToValueRecursive components Nothing Nothing Nothing
  
componentsToValueRecursive : ListStyleComponents -> 
                             Maybe Value -> -- style type
                             Maybe Value -> -- position
                             Maybe Value -> -- image
                             Value
componentsToValueRecursive components maybeType maybePosition maybeImage =
  case components of
      -- If the ListStyleComponents combinators are called more than once,
      -- the last (outer) one wins. So if it's already set we don't reset it.
    WithStyleType styleType inner ->
      case maybeType of
        Just _ -> 
          componentsToValueRecursive inner maybeType maybePosition maybeImage
        Nothing -> 
          componentsToValueRecursive inner (Just styleType) maybePosition maybeImage
    WithStylePosition stylePosition inner ->
      case maybePosition of
        Just _ -> 
          componentsToValueRecursive inner maybeType maybePosition maybeImage
        Nothing -> 
          componentsToValueRecursive inner maybeType (Just stylePosition) maybeImage
    WithStyleImage styleImage inner ->
      case maybeImage of
        Just _ -> 
          componentsToValueRecursive inner maybeType maybePosition maybeImage
        Nothing -> 
          componentsToValueRecursive inner maybeType maybePosition (Just styleImage)
    NoListStyleComponents ->
      let allValues = 
            [ maybeType
            , maybePosition
            , maybeImage
            ] |> List.filterMap identity
          
      in spaceListValue identity allValues
