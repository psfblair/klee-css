module Css.List 
  ( listStyleType
  , disc
  , armenian
  , circleListStyle
  , cjkIdeographic
  , decimal
  , decimalLeadingZero
  , georgian
  , hebrew
  , hiragana
  , hiraganaIroha
  , katakana
  , katakanaIroha
  , lowerAlpha
  , lowerGreek
  , lowerLatin
  , lowerRoman
  , square
  , upperAlpha
  , upperLatin
  , upperRoman

  , listStylePosition
  , inside
  , outside

  , listStyleImage
  , imageUrl

  , listStyle
  , withListType, withListPosition, withListImage
  ) where

import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)
import Css.Internal.List exposing (..)

-------------------------------------------------------------------------------

listStyleType : ListStyleTypeDescriptor -> PropertyRuleAppender
listStyleType descriptor = 
  let styleValue = descriptor listStyleTypeFactory
  in simpleProperty "list-style-type" styleValue

disc : ListStyleTypeDescriptor
disc factory = factory.disc

armenian : ListStyleTypeDescriptor
armenian factory = factory.armenian

circleListStyle : ListStyleTypeDescriptor
circleListStyle factory = factory.circleListStyleType

cjkIdeographic : ListStyleTypeDescriptor
cjkIdeographic factory = factory.cjkIdeographic

decimal : ListStyleTypeDescriptor
decimal factory = factory.decimal

decimalLeadingZero : ListStyleTypeDescriptor
decimalLeadingZero factory = factory.decimalLeadingZero

georgian : ListStyleTypeDescriptor
georgian factory = factory.georgian

hebrew : ListStyleTypeDescriptor
hebrew factory = factory.hebrew

hiragana : ListStyleTypeDescriptor
hiragana factory = factory.hiragana

hiraganaIroha : ListStyleTypeDescriptor
hiraganaIroha factory = factory.hiraganaIroha

katakana : ListStyleTypeDescriptor
katakana factory = factory.katakana

katakanaIroha : ListStyleTypeDescriptor
katakanaIroha factory = factory.katakanaIroha

lowerAlpha : ListStyleTypeDescriptor
lowerAlpha factory = factory.lowerAlpha

lowerGreek : ListStyleTypeDescriptor
lowerGreek factory = factory.lowerGreek

lowerLatin : ListStyleTypeDescriptor
lowerLatin factory = factory.lowerLatin

lowerRoman : ListStyleTypeDescriptor
lowerRoman factory = factory.lowerRoman

square : ListStyleTypeDescriptor
square factory = factory.square

upperAlpha : ListStyleTypeDescriptor
upperAlpha factory = factory.upperAlpha

upperLatin : ListStyleTypeDescriptor
upperLatin factory = factory.upperLatin

upperRoman : ListStyleTypeDescriptor
upperRoman factory = factory.upperRoman

-------------------------------------------------------------------------------

listStylePosition : ListStylePositionDescriptor -> PropertyRuleAppender
listStylePosition descriptor = 
  let positionValue = descriptor listStylePositionFactory
  in simpleProperty "list-style-position" positionValue

inside : ListStylePositionDescriptor
inside factory = factory.inside

outside : ListStylePositionDescriptor
outside factory = factory.outside

-------------------------------------------------------------------------------

listStyleImage : ListStyleImageDescriptor -> PropertyRuleAppender
listStyleImage descriptor = 
  let imageValue = descriptor listStyleImageFactory
  in simpleProperty "list-style-image" imageValue

imageUrl : String -> ListStyleImageDescriptor
imageUrl urlString factory = factory.url urlString

-------------------------------------------------------------------------------
-- list-style-type list-style-position list-style-image . All Three are optional.
-- Also takes initial and inherit
listStyle : ListStyleDescriptor a -> PropertyRuleAppender
listStyle descriptor = 
  let styleRecord = descriptor initialListStyleFactory 
      styleValue = listStyleValue styleRecord.listStyle
  in simpleProperty "list-style" styleValue

withListType : ListStyleTypeDescriptor -> ComposedListStyleDescriptor a
withListType typeDescriptor inner =
  let styleType = typeDescriptor listStyleTypeFactory
      innerComponents = inner.styleComponents
      newComponents = WithStyleType styleType innerComponents
  in adjoinListStyle newComponents
  
withListPosition : ListStylePositionDescriptor -> ComposedListStyleDescriptor a
withListPosition positionDescriptor inner =
  let stylePos = positionDescriptor listStylePositionFactory
      innerComponents = inner.styleComponents
      newComponents = WithStylePosition stylePos innerComponents
  in adjoinListStyle newComponents
  
withListImage : ListStyleImageDescriptor -> ComposedListStyleDescriptor a
withListImage imageDescriptor inner =
  let imageType = imageDescriptor listStyleImageFactory
      innerComponents = inner.styleComponents
      newComponents = WithStyleImage imageType innerComponents
  in adjoinListStyle newComponents
  
