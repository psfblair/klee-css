module Css.Background
  (
  -- * The background-position.

    backgroundPosition
  , placed
  , positioned

  -- * Specifying sides.

  , sideTop
  , sideLeft
  , sideRight
  , sideBottom
  , sideCenter
  , sideMiddle

  -- * The background-size.

  , backgroundSize
  , contain, cover

  -- * The background-color.

  , backgroundColor

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

  -- * Generic background property.

  , background

  ) where

import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)
import Css.Internal.Box exposing (BoxType)
import Css.Internal.Color exposing (colorValue)
import Css.Internal.Size exposing (Size)

import Css.Internal.Background exposing (..)

-------------------------------------------------------------------------------

backgroundPosition : BackgroundPositionDescriptor sz -> PropertyRuleAppender
backgroundPosition descriptor = 
  let value = descriptor backgroundPositionFactory
  in simpleProperty "background-position" (backgroundPositionValue value)
  
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
  in simpleProperty "background-size" (backgroundSizeValue bgSize)

contain : BackgroundSizeDescriptor sz
contain factory = backgroundSizeFactory.named "contain"

cover : BackgroundSizeDescriptor sz
cover factory = backgroundSizeFactory.named "cover"

by : Size sz -> Size sz -> BackgroundSizeDescriptor sz
by width height factory = backgroundSizeFactory.backgroundSize width height

bgWidth : Size sz -> BackgroundSizeDescriptor sz
bgWidth width factory = backgroundSizeFactory.partial width

-------------------------------------------------------------------------------

-- NOTE background-color takes "transparent" as well as the standard color 
-- descriptors, which is one reason we need ColorDescriptor to be parameterized.
backgroundColor : BackgroundColorDescriptor -> PropertyRuleAppender
backgroundColor colorDescriptor = 
  let color = colorDescriptor backgroundColorFactory
  in simpleProperty "background-color" (colorValue color)

transparent : BackgroundColorDescriptor
transparent factory = factory.transparent

-------------------------------------------------------------------------------

backgroundImage : BackgroundImageDescriptor -> PropertyRuleAppender
backgroundImage descriptor = 
  let bgImage = descriptor backgroundImageFactory
  in simpleProperty "background-image" (backgroundImageValue bgImage)

-- TODO Validate that it's a proper url?
url : String -> BackgroundImageDescriptor
url bgImageUrl factory = factory.url bgImageUrl

-------------------------------------------------------------------------------

backgroundRepeat : BackgroundRepeatDescriptor -> PropertyRuleAppender
backgroundRepeat descriptor = 
  let repeat = descriptor backgroundRepeatFactory
  in simpleProperty "background-repeat" (backgroundRepeatValue repeat)

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

backgroundOrigin : BackgroundOriginDescriptor -> PropertyRuleAppender
backgroundOrigin descriptor = 
  let bgOrigin = descriptor backgroundOriginFactory
  in simpleProperty "background-origin" (backgroundOriginValue bgOrigin)

origin : BoxType -> BackgroundOriginDescriptor
origin boxType factory = factory.origin boxType

-------------------------------------------------------------------------------

backgroundClip : BackgroundClipDescriptor -> PropertyRuleAppender
backgroundClip descriptor = 
  let bgClip = descriptor backgroundClipFactory
  in simpleProperty "background-clip" (backgroundClipValue bgClip)

boxClip : BoxType -> BackgroundClipDescriptor
boxClip boxType factory = factory.clip boxType

-------------------------------------------------------------------------------

backgroundAttachment : BackgroundAttachmentDescriptor -> PropertyRuleAppender
backgroundAttachment descriptor = 
  let bgAttachment = descriptor backgroundAttachmentFactory
  in simpleProperty "background-attachment" (backgroundAttachmentValue bgAttachment)

attachFixed : BackgroundAttachmentDescriptor
attachFixed factory = factory.bgAttachment "fixed"

attachScroll : BackgroundAttachmentDescriptor
attachScroll factory = factory.bgAttachment "scroll"

-------------------------------------------------------------------------------
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
background : BackgroundDescriptor a sz1 sz2 -> PropertyRuleAppender
background backgroundDescriptor = 
  --   BackgroundFactory a b sz1 sz2 -> (Background a sz1 sz2 -> Background b sz1 sz2)
  let bgValue = 
    (backgroundDescriptor backgroundFactory) emptyBackground
    |> backgroundValue
  in simpleProperty "background" bgValue

{- Equivalent to 
withBgColor : BackgroundColorFactory -> CssColor -> 
             (BackgroundFactory sz1 sz2 -> (Background a sz1 sz2 -> Background a sz1 sz2))
             BackgroundFactory sz1 sz2  -> 
             (BackgroundFactory sz1 sz2 -> (Background a sz1 sz2 -> Background a sz1 sz2))

The previous combinator in the chain doesn't bind either the innerDescriptor
or the compositeFactory. So `background` binds the compositeFactory to get 
out the fully-composed function of Background to Background, and then binds
with an empty background to get the ultimate background.
-}
withBgColor : BackgroundColorDescriptor -> 
              ComposedBackgroundDescriptor a sz1 sz2 -> 
              ComposedBackgroundDescriptor a sz1 sz2
withBgColor colorDescriptor innerDescriptor compositeFactory = 
   let color = colorDescriptor backgroundColorFactory
       innerBg = innerDescriptor compositeFactory
   in compositeFactory.composite (WithColor color) innerBg

withPosition : BackgroundPositionDescriptor sz1 -> 
               Maybe (BackgroundSizeDescriptor sz2) -> 
               ComposedBackgroundDescriptor a sz1 sz2 -> 
               ComposedBackgroundDescriptor a sz1 sz2
withPosition positionDescriptor 
             maybeSizeDescriptor 
             innerDescriptor 
             compositeFactory =
   let position = positionDescriptor backgroundPositionFactory
       maybeSize = 
         maybeSizeDescriptor |> Maybe.map (\desc -> desc backgroundSizeFactory)
       innerBg = innerDescriptor compositeFactory
   in compositeFactory.composite (WithPositionAndSize position maybeSize) innerBg

withRepeat : BackgroundRepeatDescriptor -> 
             ComposedBackgroundDescriptor a sz1 sz2 -> 
             ComposedBackgroundDescriptor a sz1 sz2
withRepeat repeatDescriptor innerDescriptor compositeFactory =
   let repeat = repeatDescriptor backgroundRepeatFactory
       innerBg = innerDescriptor compositeFactory
   in compositeFactory.composite (WithRepeat repeat) innerBg

withImage : BackgroundImageDescriptor -> 
            ComposedBackgroundDescriptor a sz1 sz2 -> 
            ComposedBackgroundDescriptor a sz1 sz2
withImage imageDescriptor innerDescriptor compositeFactory =
   let image = imageDescriptor backgroundImageFactory
       innerBg = innerDescriptor compositeFactory
   in compositeFactory.composite (WithImage image) innerBg

withOrigin : BackgroundOriginDescriptor -> 
             ComposedBackgroundDescriptor a sz1 sz2 -> 
             ComposedBackgroundDescriptor a sz1 sz2
withOrigin originDescriptor innerDescriptor compositeFactory =
   let origin = originDescriptor backgroundOriginFactory
       innerBg = innerDescriptor compositeFactory
   in compositeFactory.composite (WithOrigin origin) innerBg
  
withClip : BackgroundClipDescriptor -> 
           ComposedBackgroundDescriptor a sz1 sz2 -> 
           ComposedBackgroundDescriptor a sz1 sz2
withClip clipDescriptor innerDescriptor compositeFactory =
   let clip = clipDescriptor backgroundClipFactory
       innerBg = innerDescriptor compositeFactory
   in compositeFactory.composite (WithClip clip) innerBg

withAttachment : BackgroundAttachmentDescriptor -> 
                 ComposedBackgroundDescriptor a sz1 sz2 -> 
                 ComposedBackgroundDescriptor a sz1 sz2
withAttachment attachmentDescriptor innerDescriptor compositeFactory =
   let attachment = attachmentDescriptor backgroundAttachmentFactory
       innerBg = innerDescriptor compositeFactory
   in compositeFactory.composite (WithAttachment attachment) innerBg
