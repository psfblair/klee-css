module Css.Internal.Geometry.Linear 
  ( SizeDescriptor, Size, sizeFactory, sizeValue
  
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

-- See the bottom of this file for why there are two type parameters.
type alias SizeDescriptor a c = SizeFactory a c -> a -- c is the constraint type (Abs or Rel)

type Size a -- Phantom type, for type safety. The type parameter is for Abs or Rel.
  = Size Property.Value
  | OtherSize Property.Value

-------------------------------------------------------------------------------

-- | Zero size.
nil : SizeDescriptor a c
nil = \factory -> factory.size (Property.stringValue "0")

-- | Unitless size (as recommended for line-height).
unitless : Float -> SizeDescriptor a c
unitless length = \factory -> factory.size (Property.floatValue length)

-------------------------------------------------------------------------------

-- TODO needs auto, initial, inherit
top : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
top sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "top" (sizeValue sz)

-- TODO needs auto, initial, inherit
left : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
left sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "left" (sizeValue sz)

-- TODO needs auto, initial, inherit
bottom : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
bottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "bottom" (sizeValue sz)

-- TODO needs auto, initial, inherit
right : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
right sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "right" (sizeValue sz)

-- TODO needs auto, initial, inherit
width : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
width sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "width" (sizeValue sz)

-- TODO needs auto, initial, inherit
height : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
height sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "height" (sizeValue sz)

-- TODO needs initial, inherit
minWidth : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
minWidth sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "min-width" (sizeValue sz)

-- TODO needs initial, inherit
minHeight : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
minHeight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "min-height" (sizeValue sz)

-- TODO needs none, initial, inherit
maxWidth : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
maxWidth sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "max-width" (sizeValue sz)

-- TODO needs none, initial, inherit
maxHeight : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
maxHeight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "max-height" (sizeValue sz)

-------------------------------------------------------------------------------

-- TODO needs initial, inherit
padding : SizeDescriptor (Size a) a -> 
          SizeDescriptor (Size a) a -> 
          SizeDescriptor (Size a) a -> 
          SizeDescriptor (Size a) a -> 
          Stylesheet.PropertyRuleAppender
padding sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD = 
    let szA = sizeDescriptorA sizeFactory 
        szB = sizeDescriptorB sizeFactory 
        szC = sizeDescriptorC sizeFactory 
        szD = sizeDescriptorD sizeFactory 
        valueFactory = Property.spaceQuadrupleValue sizeValue sizeValue sizeValue sizeValue
    in Stylesheet.simpleProperty "padding" (valueFactory (szA, szB, szC, szD))

paddingTop : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
paddingTop sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "padding-top" (sizeValue sz)

paddingLeft : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
paddingLeft sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "padding-left" (sizeValue sz)

paddingRight : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
paddingRight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "padding-right" (sizeValue sz)

paddingBottom : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
paddingBottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "padding-bottom" (sizeValue sz)

-------------------------------------------------------------------------------

-- TODO needs auto, initial, inherit
margin : SizeDescriptor (Size a) a -> 
         SizeDescriptor (Size a) a -> 
         SizeDescriptor (Size a) a -> 
         SizeDescriptor (Size a) a -> 
         Stylesheet.PropertyRuleAppender
margin sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD = 
    let szA = sizeDescriptorA sizeFactory 
        szB = sizeDescriptorB sizeFactory 
        szC = sizeDescriptorC sizeFactory 
        szD = sizeDescriptorD sizeFactory 
        valueFactory = Property.spaceQuadrupleValue sizeValue sizeValue sizeValue sizeValue
    in Stylesheet.simpleProperty "margin" (valueFactory (szA, szB, szC, szD))

-- TODO needs auto, initial, inherit
marginTop : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
marginTop sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "margin-top" (sizeValue sz)

-- TODO needs auto, initial, inherit
marginLeft : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
marginLeft sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "margin-left" (sizeValue sz)

-- TODO needs auto, initial, inherit
marginRight : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
marginRight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "margin-right" (sizeValue sz)

-- TODO needs auto, initial, inherit
marginBottom : SizeDescriptor (Size a) a -> Stylesheet.PropertyRuleAppender
marginBottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in Stylesheet.simpleProperty "margin-bottom" (sizeValue sz)

-------------------------------------------------------------------------------

type alias SizeFactory a c = -- c is the constraint type (Abs or Rel)
  { size: Property.Value -> a -- This has to be more open than Size so that 
                              -- we can use sizes in properties requiring various. 
                              -- other descriptors. 
                              -- See e.g., Css.Display.verticalAlign.
  , other: Property.Value -> Size c
  }

sizeFactory : SizeFactory (Size a) a
sizeFactory =
  {
    size value = Size value
  , other val = OtherSize val
  }

sizeValue : Size a -> Property.Value
sizeValue size =
  case size of
    Size val -> val
    OtherSize val -> Common.otherValue val
