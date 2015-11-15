module Css.Internal.Geometry.Linear 
  ( Size, SizeDescriptor, toSize, sizeValue
  , BasicSizeDescriptor, BasicSizeFactory, basicSizeFactory
  , AutoSizableDescriptor, AutoSizableFactory, autoSizableFactory
  , SizeDescriptorWithNone, SizeFactoryWithNone, sizeFactoryWithNone
  
  -- * Generic linear size constructors
  
  , unitless

  -- * Positioning properties.
    
  , top, left, bottom, right

  -- * Sizing properties.
    
  , width, height, minWidth, minHeight, maxWidth, maxHeight

  -- * Size composers.
    
  , Rect
  , rect

  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

-- sz is the constraint type (`Abs` or `Rel` or unitless `{}`)
type Size sz = Size sz Property.Value

{-  `rec` makes the descriptor extensible; `sz` is the size constraint type.
If you want to keep the descriptor from allowing any generic properties
besides `other`, e.g., if the descriptor is used in constructing a more
complex descriptor, use `SizeDescriptor {}` 
-}
type alias SizeDescriptor rec sz = SizeFactory rec sz -> Property.Value

-- has initial, inherit, unset
type alias BasicSizeDescriptor sz = BasicSizeFactory {} sz -> Property.Value 

-- adds auto to initial, inherit, unset
type alias AutoSizableDescriptor sz = AutoSizableFactory {} sz -> Property.Value

-- adds none to initial, inherit, unset
type alias SizeDescriptorWithNone sz = SizeFactoryWithNone {} sz -> Property.Value

toSize : a -> Property.Value -> Size a
toSize constraint val = Size constraint val

-- For other modules that use bare size descriptors for more complex descriptors.
sizeValue : SizeDescriptor {} sz -> Property.Value
sizeValue descriptor = descriptor nubSizeFactory
-------------------------------------------------------------------------------

-- | Unitless size (as recommended for line-height).
unitless : Float -> SizeDescriptor rec {}
unitless length = 
  let unitlessSize = Size {} (Property.floatValue length)
  in \factory -> factory.size unitlessSize

-------------------------------------------------------------------------------

top : AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
top sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "top" sizeValue

left : AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
left sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "left" sizeValue

bottom : AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
bottom sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "bottom" sizeValue

right : AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
right sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "right" sizeValue

width : AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
width sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "width" sizeValue

height : AutoSizableDescriptor sz -> Stylesheet.PropertyRuleAppender
height sizeDescriptor = 
  let sizeValue = sizeDescriptor autoSizableFactory 
  in Stylesheet.simpleProperty "height" sizeValue

-------------------------------------------------------------------------------

minWidth : BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
minWidth sizeDescriptor = 
  let sizeValue = sizeDescriptor basicSizeFactory 
  in Stylesheet.simpleProperty "min-width" sizeValue

minHeight : BasicSizeDescriptor sz -> Stylesheet.PropertyRuleAppender
minHeight sizeDescriptor = 
  let sizeValue = sizeDescriptor basicSizeFactory 
  in Stylesheet.simpleProperty "min-height" sizeValue

maxWidth : SizeDescriptorWithNone sz -> Stylesheet.PropertyRuleAppender
maxWidth sizeDescriptor = 
  let sizeValue = sizeDescriptor sizeFactoryWithNone 
  in Stylesheet.simpleProperty "max-width" sizeValue

maxHeight : SizeDescriptorWithNone sz -> Stylesheet.PropertyRuleAppender
maxHeight sizeDescriptor = 
  let sizeValue = sizeDescriptor sizeFactoryWithNone 
  in Stylesheet.simpleProperty "max-height" sizeValue

-------------------------------------------------------------------------------

type alias Rect a sz rec = 
  { rec | rect_ : SizeDescriptor {} sz ->
                  SizeDescriptor {} sz ->
                  SizeDescriptor {} sz ->
                  SizeDescriptor {} sz ->
                  a 
  }

rect : SizeDescriptor {} sz ->
       SizeDescriptor {} sz ->
       SizeDescriptor {} sz ->
       SizeDescriptor {} sz ->
       Rect a sz rec -> 
       a
rect top right bottom left factory = factory.rect_ top right bottom left

-------------------------------------------------------------------------------

type alias SizeFactory rec sz =
  { rec | size : Size sz -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubSizeFactory : SizeFactory {} sz 
nubSizeFactory =
  { size (Size constraint value) = value
  , other_ val = Common.otherValue val
  }

type alias BasicSizeFactory rec sz = 
  SizeFactory 
    (Common.Initial Property.Value 
      (Common.Inherit Property.Value 
        (Common.Unset Property.Value rec))) sz

basicSizeFactory : BasicSizeFactory {} sz
basicSizeFactory =
  let withInitial = { nubSizeFactory | initial_ = Common.initialValue }
      withInherit = { withInitial    | inherit_ = Common.inheritValue }
      withUnset =   { withInherit    | unset_ = Common.unsetValue }
  in withUnset

type alias AutoSizableFactory rec sz =  
  BasicSizeFactory (Common.Auto Property.Value rec) sz

autoSizableFactory : AutoSizableFactory {} sz
autoSizableFactory = { basicSizeFactory | auto_ = Common.autoValue }

type alias SizeFactoryWithNone rec sz =  
  BasicSizeFactory (Common.None Property.Value rec) sz

sizeFactoryWithNone : SizeFactoryWithNone {} sz
sizeFactoryWithNone = { basicSizeFactory | none_ = Common.noneValue }
