module Css.Internal.Geometry.Linear 
  ( Size, SizeDescriptor, SizeFactory, toSize, nubSizeFactory
  , BasicSizeDescriptor, BasicSizeFactory, basicSizeFactory
  , AutoSizableDescriptor, AutoSizableFactory, autoSizableFactory
  , SizeDescriptorWithNone, SizeFactoryWithNone, sizeFactoryWithNone
  , SizeDescriptorWithNormal, sizeFactoryWithNormal
  
  , unitlessSize
  , Abs, absolute
  , Rel, relative

  , Rect

  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

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

-- adds normal to initial, inherit, unset
type alias SizeDescriptorWithNormal sz = SizeFactoryWithNormal {} sz -> Property.Value

-------------------------------------------------------------------------------

toSize : a -> Property.Value -> Size a
toSize constraint val = Size constraint val

-------------------------------------------------------------------------------

unitlessSize : Float -> Size {}
unitlessSize length = Size {} ( Property.floatValue length )

type Abs = Abs

absolute : Property.Value -> Size Abs
absolute lengthValue = toSize Abs lengthValue

-- | Sizes can be relative like percentages or rems.
type Rel = Rel

relative : Property.Value -> Size Rel
relative lengthValue = toSize Rel lengthValue

-------------------------------------------------------------------------------

type alias Rect a sz rec = 
  { rec | rect_ : SizeDescriptor {} sz ->
                  SizeDescriptor {} sz ->
                  SizeDescriptor {} sz ->
                  SizeDescriptor {} sz ->
                  a 
  }

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
      withUnset =   { withInherit    | unset_   = Common.unsetValue   }
  in withUnset

type alias AutoSizableFactory rec sz =  
  BasicSizeFactory (Common.Auto Property.Value rec) sz

autoSizableFactory : AutoSizableFactory {} sz
autoSizableFactory = { basicSizeFactory | auto_ = Common.autoValue }

type alias SizeFactoryWithNone rec sz =  
  BasicSizeFactory (Common.None Property.Value rec) sz

sizeFactoryWithNone : SizeFactoryWithNone {} sz
sizeFactoryWithNone = { basicSizeFactory | none_ = Common.noneValue }

type alias SizeFactoryWithNormal rec sz =  
  BasicSizeFactory (Common.Normal Property.Value rec) sz

sizeFactoryWithNormal : SizeFactoryWithNormal {} sz
sizeFactoryWithNormal = { basicSizeFactory | normal_ = Common.normalValue }
