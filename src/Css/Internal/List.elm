module Css.Internal.List 
  ( ListStyleTypeDescriptor, listStyleTypeFactory
  , ListStylePositionDescriptor, listStylePositionFactory
  , ListStyleImageDescriptor, listStyleImageFactory
  , ListStyleDescriptor, ComposedListStyleDescriptor, listStyleValue
  , initialListStyleFactory
  , ListStyleComponents (..), adjoinListStyle 
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property
-------------------------------------------------------------------------------

type alias ListStyleTypeDescriptor = ListStyleTypeFactory -> Property.Value
  
type alias ListStyleTypeFactory =
  { disc : Property.Value
  , armenian : Property.Value
  , circleListStyleType : Property.Value
  , cjkIdeographic : Property.Value
  , decimal : Property.Value
  , decimalLeadingZero : Property.Value
  , georgian : Property.Value
  , hebrew : Property.Value
  , hiragana : Property.Value
  , hiraganaIroha : Property.Value
  , katakana : Property.Value
  , katakanaIroha : Property.Value
  , lowerAlpha : Property.Value
  , lowerGreek : Property.Value
  , lowerLatin : Property.Value
  , lowerRoman : Property.Value
  , square : Property.Value
  , upperAlpha : Property.Value
  , upperLatin : Property.Value
  , upperRoman : Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_   : Property.Value
  , none_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

listStyleTypeFactory : ListStyleTypeFactory
listStyleTypeFactory =
  { disc = Property.stringValue "disc"
  , armenian = Property.stringValue "armenian"
  , circleListStyleType = Property.stringValue "circle"
  , cjkIdeographic = Property.stringValue "cjk-ideographic"
  , decimal = Property.stringValue "decimal"
  , decimalLeadingZero = Property.stringValue "decimal-leading-zero"
  , georgian = Property.stringValue "georgian"
  , hebrew = Property.stringValue "hebrew"
  , hiragana = Property.stringValue "hiragana"
  , hiraganaIroha = Property.stringValue "hiragana-iroha"
  , katakana = Property.stringValue "katakana"
  , katakanaIroha = Property.stringValue "katakana-iroha"
  , lowerAlpha = Property.stringValue "lower-alpha"
  , lowerGreek = Property.stringValue "lower-greek"
  , lowerLatin = Property.stringValue "lower-latin"
  , lowerRoman = Property.stringValue "lower-roman"
  , square = Property.stringValue "square"
  , upperAlpha = Property.stringValue "upper-alpha"
  , upperLatin = Property.stringValue "upper-latin"
  , upperRoman = Property.stringValue "upper-roman"
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_   = Common.unsetValue
  , none_    = Common.noneValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias ListStylePositionDescriptor = 
  ListStylePositionFactory -> Property.Value
  
type alias ListStylePositionFactory =
  { inside : Property.Value
  , outside : Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

listStylePositionFactory : ListStylePositionFactory
listStylePositionFactory =
  { inside = Property.stringValue "inside"
  , outside = Property.stringValue "outside"
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_   = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias ListStyleImageDescriptor = ListStyleImageFactory -> Property.Value
  
type alias ListStyleImageFactory =
  { url : String -> Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , none_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

listStyleImageFactory : ListStyleImageFactory
listStyleImageFactory =
  { url urlString =
      let urlValue = Property.toLiteral urlString |> Property.literalValue
      in  [ Property.stringValue "url("
          , urlValue
          , Property.stringValue ")" 
          ] |> Property.concatenateValues
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
  | OtherListStyle Property.Value

type ListStyleComponents
  = NoListStyleComponents
  | WithStyleType Property.Value ListStyleComponents
  | WithStylePosition Property.Value ListStyleComponents
  | WithStyleImage Property.Value ListStyleComponents

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

listStyleValue : ListStyleAlternative -> Property.Value
listStyleValue style =
  case style of
    InitialListStyle   -> Common.initialValue
    InheritListStyle   -> Common.inheritValue
    UnsetListStyle     -> Common.unsetValue
    OtherListStyle val -> Common.otherValue val
    CompositeListStyle components -> componentsToValue components

componentsToValue : ListStyleComponents -> Property.Value
componentsToValue components = 
  componentsToValueRecursive components Nothing Nothing Nothing
  
componentsToValueRecursive : ListStyleComponents -> 
                             Maybe Property.Value -> -- style type
                             Maybe Property.Value -> -- position
                             Maybe Property.Value -> -- image
                             Property.Value
componentsToValueRecursive components maybeType maybePosition maybeImage =
  let recurse = componentsToValueRecursive
  in case components of
      -- If the ListStyleComponents combinators are called more than once,
      -- the last (outer) one wins. So if it's already set we don't reset it.
    WithStyleType styleType inner ->
      case maybeType of
        Just _ -> 
          recurse inner maybeType maybePosition maybeImage
        Nothing -> 
          recurse inner (Just styleType) maybePosition maybeImage
    WithStylePosition stylePosition inner ->
      case maybePosition of
        Just _ -> 
          recurse inner maybeType maybePosition maybeImage
        Nothing -> 
          recurse inner maybeType (Just stylePosition) maybeImage
    WithStyleImage styleImage inner ->
      case maybeImage of
        Just _ -> 
          recurse inner maybeType maybePosition maybeImage
        Nothing -> 
          recurse inner maybeType maybePosition (Just styleImage)
    NoListStyleComponents ->
      let allValues = 
            [ maybeType
            , maybePosition
            , maybeImage
            ] |> List.filterMap identity
          
      in Property.spaceListValue identity allValues
