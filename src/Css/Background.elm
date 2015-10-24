module Css.Background
  (
  -- * Generic background property.

--    Background (background)

  -- * The background-color.

  backgroundColor

  -- * The background-position.

  , backgroundPosition
  , placed
  , positioned

  -- * The background-size.

  , backgroundSize
  , contain, cover

  -- * The background-repeat.

  , backgroundRepeat
  , repeat, space, round, noRepeat
  , repeatX, repeatY

  -- * The background-origin.

  , backgroundOrigin
  , origin

  -- * The background-clip.

  , backgroundClip
  , boxClip

  -- * The background-attachment.

  , backgroundAttachment
  , attachFixed, attachScroll

  -- * The background-image.

  , backgroundImage
  , url

  -- * Specifying sides.

  , sideTop
  , sideLeft
  , sideRight
  , sideBottom
  , sideCenter
  , sideMiddle
  ) where
  
import Css.Internal.Property exposing
  ( Value, ValueFactory
  , stringKey, stringValueFactory
  )
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, key)
import Css.Internal.Background exposing (..)
import Css.Internal.Box exposing (BoxType)

import Css.Color exposing (colorValueFactory)
import Css.Size exposing (Size)

{-
The background shorthand property sets all the background properties in one declaration.
The properties that can be set, are: background-color, background-image, background-position, 
background-size, background-repeat, background-origin, background-clip, and 
background-attachment. These can be in any order.

background: color image position/size repeat origin clip attachment initial|inherit;

If one of the properties in the shorthand declaration is the background-size property, 
you must use a / (slash) to separate it from the background-position property, e.g. 
background:url(smiley.gif) 10px 20px/50px 50px; will result in a background image, 
positioned 10 pixels from the left, 20 pixels from the top, and the size of the 
image will be 50 pixels wide and 50 pixels high.
-}
{-
-- | We implement the generic background property as a type class that accepts
-- multiple value types. This allows us to combine different background aspects
-- into a shorthand syntax.

class Val a => Background a where
  background : a -> PropertyRuleAppender
  background = key "background"

instance Background a => Background [a]
instance (Background a, Background b) => Background (a, b)

instance Background Color
instance Background BackgroundPosition
instance Background BackgroundSize
instance Background BackgroundRepeat
instance Background BackgroundOrigin
instance Background BackgroundClip
instance Background BackgroundAttachment
instance Background BackgroundImage
-}
-------------------------------------------------------------------------------

-- NOTE background-color takes "transparent" as well as the standard color 
-- descriptors, which is one reason we need ColorDescriptor to be parameterized.

backgroundColor : BackgroundColorDescriptor -> PropertyRuleAppender
backgroundColor colorDescriptor = 
  let color = colorDescriptor backgroundColorFactory
  in key (stringKey "background-color") color colorValueFactory

transparent : BackgroundColorDescriptor
transparent factory = factory.transparent

-------------------------------------------------------------------------------

backgroundPosition : BackgroundPositionDescriptor sz -> PropertyRuleAppender
backgroundPosition descriptor = 
  let value = descriptor backgroundPositionFactory
  in key (stringKey "background-position") value backgroundPositionValueFactory
  
placed : HorizontalSide -> VerticalSide -> BackgroundPositionDescriptor sz
placed horiz vert factory = factory.sidedPosition horiz vert

positioned : Size sz -> Size sz -> BackgroundPositionDescriptor sz
positioned horiz vert factory = factory.sizedPosition horiz vert

-------------------------------------------------------------------------------

-- | These value names start with "side" to avoid conflict with existing property
-- names.
sideTop : HorizontalSide
sideTop = HorizontalSide "top"

sideLeft : HorizontalSide
sideLeft = HorizontalSide "left"

sideRight : HorizontalSide
sideRight = HorizontalSide "right"

sideBottom : VerticalSide
sideBottom = VerticalSide "bottom"

sideCenter : VerticalSide
sideCenter = VerticalSide "center"

sideMiddle : VerticalSide
sideMiddle = VerticalSide "middle"

-------------------------------------------------------------------------------

backgroundSize : BackgroundSizeDescriptor sz -> PropertyRuleAppender
backgroundSize descriptor = 
  let bgSize = descriptor backgroundSizeFactory
  in key (stringKey "background-size") bgSize backgroundSizeValueFactory

contain : BackgroundSizeDescriptor sz
contain factory = backgroundSizeFactory.named "contain"

cover : BackgroundSizeDescriptor sz
cover factory = backgroundSizeFactory.named "cover"

by : Size sz -> Size sz -> BackgroundSizeDescriptor sz
by width height factory = backgroundSizeFactory.backgroundSize width height

bgWidth : Size sz -> BackgroundSizeDescriptor sz
bgWidth width factory = backgroundSizeFactory.partial width

-------------------------------------------------------------------------------

backgroundRepeat : BackgroundRepeatDescriptor -> PropertyRuleAppender
backgroundRepeat descriptor = 
  let repeat = descriptor backgroundRepeatFactory
  in key (stringKey "background-repeat") repeat backgroundRepeatValueFactory

repeat : BackgroundRepeatDescriptor
repeat factory = backgroundRepeatFactory.repeat "repeat"

space : BackgroundRepeatDescriptor
space factory = backgroundRepeatFactory.repeat "space"

round : BackgroundRepeatDescriptor
round factory = backgroundRepeatFactory.repeat "round"

noRepeat : BackgroundRepeatDescriptor
noRepeat factory = backgroundRepeatFactory.repeat "no-repeat"

repeatX : BackgroundRepeatDescriptor
repeatX factory = backgroundRepeatFactory.repeat "repeat-x"

repeatY : BackgroundRepeatDescriptor
repeatY factory = backgroundRepeatFactory.repeat "repeat-y"

-------------------------------------------------------------------------------

backgroundImage : BackgroundImageDescriptor -> PropertyRuleAppender
backgroundImage descriptor = 
  let bgImage = descriptor backgroundImageFactory
  in key (stringKey "background-image") bgImage backgroundImageValueFactory

-- TODO Validate that it's a proper url?
url : String -> BackgroundImageDescriptor
url bgImageUrl factory = factory.url bgImageUrl

-------------------------------------------------------------------------------

backgroundOrigin : BackgroundOriginDescriptor -> PropertyRuleAppender
backgroundOrigin descriptor = 
  let bgOrigin = descriptor backgroundOriginFactory
  in key (stringKey "background-origin") bgOrigin backgroundOriginValueFactory

origin : BoxType -> BackgroundOriginDescriptor
origin boxType factory = factory.origin boxType

-------------------------------------------------------------------------------

backgroundClip : BackgroundClipDescriptor -> PropertyRuleAppender
backgroundClip descriptor = 
  let bgClip = descriptor backgroundClipFactory
  in key (stringKey "background-clip") bgClip backgroundClipValueFactory

boxClip : BoxType -> BackgroundClipDescriptor
boxClip boxType factory = factory.clip boxType

-------------------------------------------------------------------------------

backgroundAttachment : BackgroundAttachmentDescriptor -> PropertyRuleAppender
backgroundAttachment descriptor = 
  let bgAttachment = descriptor backgroundAttachmentFactory
  in key (stringKey "background-attachment") bgAttachment backgroundAttachmentValueFactory

attachFixed : BackgroundAttachmentDescriptor
attachFixed factory = factory.bgAttachment "fixed"

attachScroll : BackgroundAttachmentDescriptor
attachScroll factory = factory.bgAttachment "scroll"
