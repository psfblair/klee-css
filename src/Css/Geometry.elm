module Css.Geometry 
  (
  -- * Positioning.
    size, top, left, bottom, right

  -- * Sizing.
  , width, height, minWidth, minHeight, maxWidth, maxHeight

  -- * Padding.
  , padding
  , paddingTop, paddingLeft, paddingRight, paddingBottom

  -- * Margin.
  , margin
  , marginTop, marginLeft, marginRight, marginBottom
  ) where

import Css.Internal.Property exposing (stringKey, spaceQuadrupleValueFactory)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, key)
import Css.Size exposing (Size, SizeDescriptor, sizeFactory, sizeValueFactory)

-------------------------------------------------------------------------------

size : SizeDescriptor (Size a) a -> PropertyRuleAppender
size sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "size") sz sizeValueFactory

top : SizeDescriptor (Size a) a -> PropertyRuleAppender
top sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "top") sz sizeValueFactory

left : SizeDescriptor (Size a) a -> PropertyRuleAppender
left sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "left") sz sizeValueFactory

bottom : SizeDescriptor (Size a) a -> PropertyRuleAppender
bottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "bottom") sz sizeValueFactory

right : SizeDescriptor (Size a) a -> PropertyRuleAppender
right sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "right") sz sizeValueFactory

width : SizeDescriptor (Size a) a -> PropertyRuleAppender
width sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "width") sz sizeValueFactory

height : SizeDescriptor (Size a) a -> PropertyRuleAppender
height sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "height") sz sizeValueFactory

minWidth : SizeDescriptor (Size a) a -> PropertyRuleAppender
minWidth sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "min-width") sz sizeValueFactory

minHeight : SizeDescriptor (Size a) a -> PropertyRuleAppender
minHeight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "min-height") sz sizeValueFactory

maxWidth : SizeDescriptor (Size a) a -> PropertyRuleAppender
maxWidth sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "max-width") sz sizeValueFactory

maxHeight : SizeDescriptor (Size a) a -> PropertyRuleAppender
maxHeight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "max-height") sz sizeValueFactory

-------------------------------------------------------------------------------

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
        svf = sizeValueFactory
        valueFactory = spaceQuadrupleValueFactory svf svf svf svf
    in key (stringKey "padding") (szA, szB, szC, szD) valueFactory

paddingTop : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingTop sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "padding-top") sz sizeValueFactory

paddingLeft : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingLeft sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "padding-left") sz sizeValueFactory

paddingRight : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingRight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "padding-right") sz sizeValueFactory

paddingBottom : SizeDescriptor (Size a) a -> PropertyRuleAppender
paddingBottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "padding-bottom") sz sizeValueFactory

-------------------------------------------------------------------------------

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
        svf = sizeValueFactory
        valueFactory = spaceQuadrupleValueFactory svf svf svf svf
    in key (stringKey "margin") (szA, szB, szC, szD) valueFactory

marginTop : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginTop sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "margin-top") sz sizeValueFactory

marginLeft : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginLeft sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "margin-left") sz sizeValueFactory

marginRight : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginRight sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "margin-right") sz sizeValueFactory

marginBottom : SizeDescriptor (Size a) a -> PropertyRuleAppender
marginBottom sizeDescriptor = 
  let sz = sizeDescriptor sizeFactory 
  in key (stringKey "margin-bottom") sz sizeValueFactory

  
  
