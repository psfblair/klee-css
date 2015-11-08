module Css.Internal.List 
  ( ListStyleType, ListStyleTypeDescriptor, listStyleTypeFactory, listStyleTypeValue
  , ListStylePositionDescriptor, listStylePositionFactory, listStylePositionValue
  , ListStyleImageDescriptor, listStyleImageFactory, listStyleImageValue
  , ListStyleDescriptor, ComposedListStyleDescriptor, listStyleValue
  , initialListStyleFactory
  , ListStyleComponents (..), adjoinListStyle 
  ) where

import Css.Internal.Common exposing 
  (initialValue, inheritValue, noneValue, otherValue)
import Css.Internal.Property exposing 
  ( Value, Literal, toLiteral, concatenateValues
  , literalValue, stringValue, spaceListValue
  )
-------------------------------------------------------------------------------

type alias ListStyleTypeDescriptor =
  ListStyleTypeFactory -> ListStyleType
  
type ListStyleType 
  = Disc               
  | Armenian           
  | CircleListStyleType    
  | CjkIdeographic     
  | Decimal            
  | DecimalLeadingZero 
  | Georgian           
  | Hebrew             
  | Hiragana           
  | HiraganaIroha      
  | Katakana           
  | KatakanaIroha      
  | LowerAlpha         
  | LowerGreek         
  | LowerLatin         
  | LowerRoman         
  | Square             
  | UpperAlpha         
  | UpperLatin         
  | UpperRoman
  | InitialListStyleType
  | InheritListStyleType
  | NoListStyleType
  | OtherListStyleType Value
  
type alias ListStyleTypeFactory =
  { disc : ListStyleType
  , armenian : ListStyleType
  , circleListStyleType : ListStyleType
  , cjkIdeographic : ListStyleType
  , decimal : ListStyleType
  , decimalLeadingZero : ListStyleType
  , georgian : ListStyleType
  , hebrew : ListStyleType
  , hiragana : ListStyleType
  , hiraganaIroha : ListStyleType
  , katakana : ListStyleType
  , katakanaIroha : ListStyleType
  , lowerAlpha : ListStyleType
  , lowerGreek : ListStyleType
  , lowerLatin : ListStyleType
  , lowerRoman : ListStyleType
  , square : ListStyleType
  , upperAlpha : ListStyleType
  , upperLatin : ListStyleType
  , upperRoman : ListStyleType
  , initial_ : ListStyleType
  , inherit_ : ListStyleType
  , none_ : ListStyleType
  , other_ : Value -> ListStyleType
  }

listStyleTypeFactory : ListStyleTypeFactory
listStyleTypeFactory =
  { disc = Disc
  , armenian = Armenian
  , circleListStyleType = CircleListStyleType
  , cjkIdeographic = CjkIdeographic
  , decimal = Decimal
  , decimalLeadingZero = DecimalLeadingZero
  , georgian = Georgian
  , hebrew = Hebrew
  , hiragana = Hiragana
  , hiraganaIroha = HiraganaIroha
  , katakana = Katakana
  , katakanaIroha = KatakanaIroha
  , lowerAlpha = LowerAlpha
  , lowerGreek = LowerGreek
  , lowerLatin = LowerLatin
  , lowerRoman = LowerRoman
  , square = Square
  , upperAlpha = UpperAlpha
  , upperLatin = UpperLatin
  , upperRoman = UpperRoman
  , initial_ = InitialListStyleType
  , inherit_ = InheritListStyleType
  , none_ = NoListStyleType
  , other_ val = OtherListStyleType val
  }

listStyleTypeValue : ListStyleType -> Value
listStyleTypeValue listStyle =
  case listStyle of
    Disc -> stringValue "disc"
    Armenian -> stringValue "armenian"
    CircleListStyleType -> stringValue "circleListStyleType"
    CjkIdeographic -> stringValue "cjk-ideographic"
    Decimal -> stringValue "decimal"
    DecimalLeadingZero -> stringValue "decimal-leading-zero"
    Georgian -> stringValue "georgian"
    Hebrew -> stringValue "hebrew"
    Hiragana -> stringValue "hiragana"
    HiraganaIroha -> stringValue "hiragana-iroha"
    Katakana -> stringValue "katakana"
    KatakanaIroha -> stringValue "katakana-iroha"
    LowerAlpha -> stringValue "lower-alpha"
    LowerGreek -> stringValue "lower-greek"
    LowerLatin -> stringValue "lower-latin"
    LowerRoman -> stringValue "lower-roman"
    Square -> stringValue "square"
    UpperAlpha -> stringValue "upper-alpha"
    UpperLatin -> stringValue "upper-latin"
    UpperRoman -> stringValue "upper-roman"
    InitialListStyleType -> initialValue
    InheritListStyleType -> inheritValue
    NoListStyleType -> noneValue
    OtherListStyleType val -> otherValue val

-------------------------------------------------------------------------------

type alias ListStylePositionDescriptor =
  ListStylePositionFactory -> ListStylePosition
  
type ListStylePosition 
  = InsideListStylePosition
  | OutsideListStylePosition           
  | InitialListStylePosition
  | InheritListStylePosition
  | OtherListStylePosition Value
  
type alias ListStylePositionFactory =
  { inside : ListStylePosition
  , outside : ListStylePosition
  , initial_ : ListStylePosition
  , inherit_ : ListStylePosition
  , other_ : Value -> ListStylePosition
  }

listStylePositionFactory : ListStylePositionFactory
listStylePositionFactory =
  { inside = InsideListStylePosition
  , outside = OutsideListStylePosition
  , initial_ = InitialListStylePosition
  , inherit_ = InheritListStylePosition
  , other_ val = OtherListStylePosition val
  }

listStylePositionValue : ListStylePosition -> Value
listStylePositionValue listStyle =
  case listStyle of
    InsideListStylePosition -> stringValue "inside"
    OutsideListStylePosition -> stringValue "outside"
    InitialListStylePosition -> initialValue
    InheritListStylePosition -> inheritValue
    OtherListStylePosition val -> otherValue val

-------------------------------------------------------------------------------

type alias ListStyleImageDescriptor =
  ListStyleImageFactory -> ListStyleImage
  
type ListStyleImage 
  = ListStyleImageUrl Literal
  | InitialListStyleImage
  | InheritListStyleImage
  | NoListStyleImage
  | OtherListStyleImage Value
  
type alias ListStyleImageFactory =
  { url : String -> ListStyleImage
  , initial_ : ListStyleImage
  , inherit_ : ListStyleImage
  , none_ : ListStyleImage
  , other_ : Value -> ListStyleImage
  }

listStyleImageFactory : ListStyleImageFactory
listStyleImageFactory =
  { url str = toLiteral str |> ListStyleImageUrl
  , initial_ = InitialListStyleImage
  , inherit_ = InheritListStyleImage
  , none_ = NoListStyleImage
  , other_ val = OtherListStyleImage val
  }

listStyleImageValue : ListStyleImage -> Value
listStyleImageValue imageStyle =
  case imageStyle of
    ListStyleImageUrl url -> 
      [ stringValue "url(", literalValue url, stringValue ")" ]
      |> concatenateValues
    InitialListStyleImage -> initialValue
    InheritListStyleImage -> inheritValue
    NoListStyleImage -> noneValue
    OtherListStyleImage val -> otherValue val

-------------------------------------------------------------------------------

type alias ListStyle a = 
  { a | listStyle : ListStyleAlternative  }

type ListStyleAlternative
  = CompositeListStyle ListStyleComponents
  | InitialListStyle
  | InheritListStyle
  | OtherListStyle Value

type ListStyleComponents
  = NoListStyleComponents
  | WithStyleType ListStyleType ListStyleComponents
  | WithStylePosition ListStylePosition ListStyleComponents
  | WithStyleImage ListStyleImage ListStyleComponents

type alias ListStyleDescriptor a = ListStyleFactory -> ListStyle a
  
type alias ComposedListStyleDescriptor a = 
  { a | listStyle : ListStyle a
      , styleComponents : ListStyleComponents } ->
  ListStyleFactory

type alias ListStyleFactory =
  ListStyle (GenericListStyleFactory WithListStyleComponents)

type alias GenericListStyleFactory a =   
  { a | initial_ : ListStyle {}
      , inherit_ : ListStyle {}
      , other_ : Value -> ListStyle {}
  }

type alias WithListStyleComponents = 
  { styleComponents : ListStyleComponents
  }
                 
initialListStyleFactory : ListStyleFactory
initialListStyleFactory =
  { listStyle = CompositeListStyle NoListStyleComponents
  , styleComponents = NoListStyleComponents
  , initial_ = { listStyle = InitialListStyle }
  , inherit_ = { listStyle = InheritListStyle }
  , other_ val  = { listStyle = OtherListStyle val }
  }

adjoinListStyle : ListStyleComponents -> ListStyleFactory
adjoinListStyle newComponents = 
  { initialListStyleFactory | listStyle <- CompositeListStyle newComponents
                            , styleComponents <- newComponents }

listStyleValue : ListStyle a -> Value
listStyleValue style =
  case style.listStyle of
    InitialListStyle -> initialValue
    InheritListStyle -> inheritValue
    OtherListStyle val -> otherValue val
    CompositeListStyle components -> componentsToValue components

componentsToValue : ListStyleComponents -> Value
componentsToValue components = 
  componentsToValueRecursive components Nothing Nothing Nothing
  
componentsToValueRecursive : ListStyleComponents -> 
                             Maybe ListStyleType ->
                             Maybe ListStylePosition ->
                             Maybe ListStyleImage ->
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
      let maybeTypeValue = Maybe.map listStyleTypeValue maybeType
          maybePositionValue = Maybe.map listStylePositionValue maybePosition
          maybeImageValue = Maybe.map listStyleImageValue maybeImage

          allValues = 
            [ maybeTypeValue
            , maybePositionValue
            , maybeImageValue
            ] |> List.filterMap identity
          
      in spaceListValue identity allValues
