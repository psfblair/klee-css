module Css.Internal.Geometry.Linear 
  ( SizeDescriptor, sizeValue
  , BasicSizeDescriptor, AutoSizableDescriptor, SizeDescriptorWithNone
  
  -- * Generic linear size constructors
  
  , nil, unitless

  -- * Positioning properties.
    
  , top, left, bottom, right

  -- * Sizing properties.
    
  , width, height, minWidth, minHeight, maxWidth, maxHeight
  
  -- * Padding.
  , padding
  , paddingTop, paddingLeft, paddingRight, paddingBottom

  -- * Margin.
  , margin
  , marginTop, marginLeft, marginRight, marginBottom

  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

{-  `rec` makes the descriptor extensible; `c` is the constraint type (`Abs` or `
Rel`). If you want to keep the descriptor from allowing any generic properties
besides `other`, e.g., if the descriptor is used in constructing a more
complex descriptor, use `SizeDescriptor {}` 
-}
type alias SizeDescriptor rec c = SizeFactory rec c -> Property.Value

-- has initial, inherit, unset
type alias BasicSizeDescriptor c = BasicSizeFactory {} c -> Property.Value 

-- adds auto to initial, inherit, unset
type alias AutoSizableDescriptor c = AutoSizableFactory {} c -> Property.Value

-- adds none to initial, inherit, unset
type alias SizeDescriptorWithNone c = SizeFactoryWithNone {} c -> Property.Value

-- For other modules that use bare size descriptors for more complex descriptors.
sizeValue : SizeDescriptor {} c -> Property.Value
sizeValue descriptor =
  let coreSizeFactory = { size val = val, other_ val = Common.otherValue val }
  in descriptor coreSizeFactory
-------------------------------------------------------------------------------

-- | Zero size.
nil : SizeDescriptor rec c
nil = \factory -> factory.size (Property.stringValue "0")

-- | Unitless size (as recommended for line-height).
unitless : Float -> SizeDescriptor rec c
unitless length = \factory -> factory.size (Property.floatValue length)

-------------------------------------------------------------------------------

top : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
top sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "top" sizeValue

left : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
left sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "left" sizeValue

bottom : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
bottom sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "bottom" sizeValue

right : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
right sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "right" sizeValue

width : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
width sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "width" sizeValue

height : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
height sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "height" sizeValue

-------------------------------------------------------------------------------

minWidth : BasicSizeDescriptor c -> Stylesheet.PropertyRuleAppender
minWidth sizeDescriptor = 
  let sizeValue = sizeDescriptor basicSizeFactory 
  in Stylesheet.simpleProperty "min-width" sizeValue

minHeight : BasicSizeDescriptor c -> Stylesheet.PropertyRuleAppender
minHeight sizeDescriptor = 
  let sizeValue = sizeDescriptor basicSizeFactory 
  in Stylesheet.simpleProperty "min-height" sizeValue

maxWidth : SizeDescriptorWithNone c -> Stylesheet.PropertyRuleAppender
maxWidth sizeDescriptor = 
  let sizeValue = sizeDescriptor sizeFactoryWithNone 
  in Stylesheet.simpleProperty "max-width" sizeValue

maxHeight : SizeDescriptorWithNone c -> Stylesheet.PropertyRuleAppender
maxHeight sizeDescriptor = 
  let sizeValue = sizeDescriptor sizeFactoryWithNone 
  in Stylesheet.simpleProperty "max-height" sizeValue

-------------------------------------------------------------------------------

padding : BasicSizeDescriptor c ->
          BasicSizeDescriptor c ->
          BasicSizeDescriptor c ->
          BasicSizeDescriptor c ->
          Stylesheet.PropertyRuleAppender
padding topDescriptor rightDescriptor bottomDescriptor leftDescriptor =
    let topValue     = topDescriptor    basicSizeFactory
        rightValue   = rightDescriptor  basicSizeFactory
        bottomValue  = bottomDescriptor basicSizeFactory
        leftValue    = leftDescriptor   basicSizeFactory
        valueFactory =
          Property.spaceQuadrupleValue identity identity identity identity
        sizeValues   = valueFactory (topValue, rightValue, bottomValue, leftValue)
    in Stylesheet.simpleProperty "padding" sizeValues

paddingTop : BasicSizeDescriptor c -> Stylesheet.PropertyRuleAppender
paddingTop sizeDescriptor =
  let sizeValue = sizeDescriptor basicSizeFactory
  in Stylesheet.simpleProperty "padding-top" sizeValue

paddingLeft : BasicSizeDescriptor c -> Stylesheet.PropertyRuleAppender
paddingLeft sizeDescriptor =
  let sizeValue = sizeDescriptor basicSizeFactory
  in Stylesheet.simpleProperty "padding-left" sizeValue

paddingRight : BasicSizeDescriptor c -> Stylesheet.PropertyRuleAppender
paddingRight sizeDescriptor =
  let sizeValue = sizeDescriptor basicSizeFactory
  in Stylesheet.simpleProperty "padding-right" sizeValue

paddingBottom : BasicSizeDescriptor c -> Stylesheet.PropertyRuleAppender
paddingBottom sizeDescriptor =
  let sizeValue = sizeDescriptor basicSizeFactory
  in Stylesheet.simpleProperty "padding-bottom" sizeValue

-------------------------------------------------------------------------------

margin : AutoSizableDescriptor c ->
         AutoSizableDescriptor c ->
         AutoSizableDescriptor c ->
         AutoSizableDescriptor c ->
         Stylesheet.PropertyRuleAppender
margin topDescriptor rightDescriptor bottomDescriptor leftDescriptor =
    let topValue     = topDescriptor    autoSizableFactory
        rightValue   = rightDescriptor  autoSizableFactory
        bottomValue  = bottomDescriptor autoSizableFactory
        leftValue    = leftDescriptor   autoSizableFactory
        valueFactory =
          Property.spaceQuadrupleValue identity identity identity identity
        sizeValues   = valueFactory (topValue, rightValue, bottomValue, leftValue)
    in Stylesheet.simpleProperty "margin" sizeValues

marginTop : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
marginTop sizeDescriptor =
  let sizeValue = sizeDescriptor autoSizableFactory
  in Stylesheet.simpleProperty "margin-top" sizeValue

marginLeft : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
marginLeft sizeDescriptor =
  let sizeValue = sizeDescriptor autoSizableFactory
  in Stylesheet.simpleProperty "margin-left" sizeValue

marginRight : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
marginRight sizeDescriptor =
  let sizeValue = sizeDescriptor autoSizableFactory
  in Stylesheet.simpleProperty "margin-right" sizeValue

marginBottom : AutoSizableDescriptor c -> Stylesheet.PropertyRuleAppender
marginBottom sizeDescriptor =
  let sizeValue = sizeDescriptor autoSizableFactory
  in Stylesheet.simpleProperty "margin-bottom" sizeValue

-------------------------------------------------------------------------------

-- c is the constraint type (Abs or Rel)
type alias SizeFactory rec c =            -- 
  { rec | size: Property.Value -> Property.Value
        , other_: Property.Value -> Property.Value
  }

type alias BasicSizeFactory rec c =
  SizeFactory (
    Common.Initial Property.Value (
      Common.Inherit Property.Value (
        Common.Unset Property.Value rec
    ))) c
  
basicSizeFactory : BasicSizeFactory {} c
basicSizeFactory =
  { size value = value
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

type alias AutoSizableFactory rec c =  
  BasicSizeFactory (Common.Auto Property.Value rec) c

autoSizableFactory : AutoSizableFactory {} c
autoSizableFactory = { basicSizeFactory | auto_ = Common.autoValue }

type alias SizeFactoryWithNone rec c =  
  BasicSizeFactory (Common.None Property.Value rec) c

sizeFactoryWithNone : SizeFactoryWithNone {} c
sizeFactoryWithNone = { basicSizeFactory | none_ = Common.noneValue }
