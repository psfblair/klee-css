module Css.Geometry 
  (
  -- * Positioning.
    top, left, bottom, right

  -- * Sizing.
  , width, height, minWidth, minHeight, maxWidth, maxHeight

  -- * Padding.
  , padding
  , paddingTop, paddingLeft, paddingRight, paddingBottom

  -- * Margin.
  , margin
  , marginTop, marginLeft, marginRight, marginBottom
  ) where

import Css.Internal.Property exposing (spaceQuadrupleValue)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)
import Css.Internal.Size exposing 
  (Size, SizeDescriptor, sizeFactory, sizeValue)

-------------------------------------------------------------------------------
-- TODO needs auto, initial, inherit
top : SizeDescriptor (Size a) a -> PropertyRuleAppender
top sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "top" (sizeValue sz)

-- TODO needs auto, initial, inherit
left : SizeDescriptor (Size a) a -> PropertyRuleAppender
left sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "left" (sizeValue sz)

-- TODO needs auto, initial, inherit
bottom : SizeDescriptor (Size a) a -> PropertyRuleAppender
bottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "bottom" (sizeValue sz)

-- TODO needs auto, initial, inherit
right : SizeDescriptor (Size a) a -> PropertyRuleAppender
right sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "right" (sizeValue sz)

-- TODO needs auto, initial, inherit
width : SizeDescriptor (Size a) a -> PropertyRuleAppender
width sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "width" (sizeValue sz)

-- TODO needs auto, initial, inherit
height : SizeDescriptor (Size a) a -> PropertyRuleAppender
height sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "height" (sizeValue sz)

-- TODO needs initial, inherit
minWidth : SizeDescriptor (Size a) a -> PropertyRuleAppender
minWidth sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "min-width" (sizeValue sz)

-- TODO needs initial, inherit
minHeight : SizeDescriptor (Size a) a -> PropertyRuleAppender
minHeight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "min-height" (sizeValue sz)

-- TODO needs none, initial, inherit
maxWidth : SizeDescriptor (Size a) a -> PropertyRuleAppender
maxWidth sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "max-width" (sizeValue sz)

-- TODO needs none, initial, inherit
maxHeight : SizeDescriptor (Size a) a -> PropertyRuleAppender
maxHeight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "max-height" (sizeValue sz)

-------------------------------------------------------------------------------
-- TODO needs initial, inherit
padding : SizeDescriptor (Size a) a -> 
          SizeDescriptor (Size a) a -> 
          SizeDescriptor (Size a) a -> 
          SizeDescriptor (Size a) a -> 
          PropertyRuleAppender
padding sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD = 
    let szA = sizeDescriptorA sizeFactory 
        szB = sizeDescriptorB sizeFactory 
        szC = sizeDescriptorC sizeFactory 
        szD = sizeDescriptorD sizeFactory 
        valueFactory = spaceQuadrupleValue sizeValue sizeValue sizeValue sizeValue
    in simpleProperty "padding" (valueFactory (szA, szB, szC, szD))

paddingTop : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingTop sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "padding-top" (sizeValue sz)

paddingLeft : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingLeft sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "padding-left" (sizeValue sz)

paddingRight : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingRight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "padding-right" (sizeValue sz)

paddingBottom : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingBottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "padding-bottom" (sizeValue sz)

-------------------------------------------------------------------------------
-- TODO needs auto, initial, inherit
margin : SizeDescriptor (Size a) a -> 
         SizeDescriptor (Size a) a -> 
         SizeDescriptor (Size a) a -> 
         SizeDescriptor (Size a) a -> 
         PropertyRuleAppender
margin sizeDescriptorA sizeDescriptorB sizeDescriptorC sizeDescriptorD = 
    let szA = sizeDescriptorA sizeFactory 
        szB = sizeDescriptorB sizeFactory 
        szC = sizeDescriptorC sizeFactory 
        szD = sizeDescriptorD sizeFactory 
        valueFactory = spaceQuadrupleValue sizeValue sizeValue sizeValue sizeValue
    in simpleProperty "margin" (valueFactory (szA, szB, szC, szD))

-- TODO needs auto, initial, inherit
marginTop : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginTop sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "margin-top" (sizeValue sz)

-- TODO needs auto, initial, inherit
marginLeft : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginLeft sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "margin-left" (sizeValue sz)

-- TODO needs auto, initial, inherit
marginRight : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginRight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "margin-right" (sizeValue sz)

-- TODO needs auto, initial, inherit
marginBottom : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginBottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in simpleProperty "margin-bottom" (sizeValue sz)

  
  
