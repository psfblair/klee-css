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
  , by, bgWidth

  -- * The background-color.

  , backgroundColor, transparent

  -- * The background-repeat.

  , backgroundRepeat
  , repeat, space, roundRepeat, noRepeat
  , repeatX, repeatY

  -- * The background-origin.

  , backgroundOrigin
  , origin

  -- * The background-clip.

  , backgroundClip
  , boxClip

  -- * The background-attachment.

  , backgroundAttachment
  , attachFixed, attachScroll, attachLocal

  -- * The background-image.

  , backgroundImage
  , url

  -- * Generic background property.

  , background
  , withPosition, withBgColor, withRepeat
  , withImage, withOrigin, withClip, withAttachment
  
  ) where

import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)
import Css.Internal.Box exposing (BoxType, BoxTypeDescriptor, boxTypeFactory)
import Css.Internal.Color exposing (colorValue)
import Css.Internal.Size exposing (Size, SizeDescriptor)

import Css.Internal.Background exposing (..)

-------------------------------------------------------------------------------

backgroundPosition : BackgroundPositionDescriptor sz1 sz2 -> PropertyRuleAppender
backgroundPosition descriptor = 
  let value = descriptor backgroundPositionFactory
  in simpleProperty "background-position" (backgroundPositionValue value)
  
placed : HorizontalSide -> VerticalSide -> BackgroundPositionDescriptor sz1 sz2
placed horiz vert factory = factory.sidedPosition horiz vert

positioned : SizeDescriptor (Size sz1) sz1 -> 
             SizeDescriptor (Size sz2) sz2 -> 
             BackgroundPositionDescriptor sz1 sz2
positioned horiz vert factory = factory.sizedPosition horiz vert

-------------------------------------------------------------------------------

-- | These value names start with "side" to avoid conflict with existing property
-- names.
sideLeft : HorizontalSide
sideLeft = HorizontalSide "left"

sideCenter : HorizontalSide
sideCenter = HorizontalSide "center"

sideRight : HorizontalSide
sideRight = HorizontalSide "right"

sideTop : VerticalSide
sideTop = VerticalSide "top"

sideMiddle : VerticalSide
sideMiddle = VerticalSide "center"

sideBottom : VerticalSide
sideBottom = VerticalSide "bottom"

-------------------------------------------------------------------------------

backgroundSize : BackgroundSizeDescriptor sz -> PropertyRuleAppender
backgroundSize descriptor = 
  let bgSize = descriptor backgroundSizeFactory
  in simpleProperty "background-size" (backgroundSizeValue bgSize)

contain : BackgroundSizeDescriptor sz
contain factory = backgroundSizeFactory.named "contain"

cover : BackgroundSizeDescriptor sz
cover factory = backgroundSizeFactory.named "cover"

by : SizeDescriptor (Size sz) sz -> 
     SizeDescriptor (Size sz) sz -> 
     BackgroundSizeDescriptor sz
by width height factory = backgroundSizeFactory.backgroundSize width height

bgWidth : SizeDescriptor (Size sz) sz -> BackgroundSizeDescriptor sz
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

roundRepeat : BackgroundRepeatDescriptor
roundRepeat factory = backgroundRepeatFactory.repeat "round"

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

origin : BoxTypeDescriptor -> BackgroundOriginDescriptor
origin boxTypeDescriptor factory = 
  let boxType = boxTypeDescriptor boxTypeFactory
  in factory.origin boxType

-------------------------------------------------------------------------------

backgroundClip : BackgroundClipDescriptor -> PropertyRuleAppender
backgroundClip descriptor = 
  let bgClip = descriptor backgroundClipFactory
  in simpleProperty "background-clip" (backgroundClipValue bgClip)

boxClip : BoxTypeDescriptor -> BackgroundClipDescriptor
boxClip boxTypeDescriptor factory = 
  let boxType = boxTypeDescriptor boxTypeFactory
  in factory.clip boxType

-------------------------------------------------------------------------------

backgroundAttachment : BackgroundAttachmentDescriptor -> PropertyRuleAppender
backgroundAttachment descriptor = 
  let bgAttachment = descriptor backgroundAttachmentFactory
  in simpleProperty "background-attachment" (backgroundAttachmentValue bgAttachment)

attachFixed : BackgroundAttachmentDescriptor
attachFixed factory = factory.bgAttachment "fixed"

attachScroll : BackgroundAttachmentDescriptor
attachScroll factory = factory.bgAttachment "scroll"

attachLocal : BackgroundAttachmentDescriptor
attachLocal factory = factory.bgAttachment "local"

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
-- The type signature here obscures the fact that the background descriptor
-- can be a function that accepts a more generic type of record as its parameter.
background : BackgroundDescriptor a sz1 sz2 sz3 -> PropertyRuleAppender
background backgroundDescriptor = 
  let backgroundRecord = backgroundDescriptor initialBackgroundFactory
      bgValue = backgroundRecord.background |> backgroundValue
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
withPosition : BackgroundPositionDescriptor sz1 sz2 -> 
               Maybe (BackgroundSizeDescriptor sz3) -> 
               ComposedBackgroundDescriptor a sz1 sz2 sz3
withPosition positionDescriptor 
             maybeSizeDescriptor 
             composedDescriptor =
   let position = positionDescriptor backgroundPositionFactory
       maybeSize = 
         maybeSizeDescriptor |> Maybe.map (\desc -> desc backgroundSizeFactory)
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithPositionAndSize position maybeSize innerComponents
   in adjoinComponents newComponents

withBgColor : BackgroundColorDescriptor -> 
              ComposedBackgroundDescriptor a sz1 sz2 sz3 
withBgColor colorDescriptor composedDescriptor = 
   let color = colorDescriptor backgroundColorFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithColor color innerComponents
   in adjoinComponents newComponents

withRepeat : BackgroundRepeatDescriptor -> 
             ComposedBackgroundDescriptor a sz1 sz2 sz3
withRepeat repeatDescriptor composedDescriptor =
   let repeat = repeatDescriptor backgroundRepeatFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithRepeat repeat innerComponents
   in adjoinComponents newComponents

withImage : BackgroundImageDescriptor -> 
            ComposedBackgroundDescriptor a sz1 sz2 sz3
withImage imageDescriptor composedDescriptor =
   let image = imageDescriptor backgroundImageFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithImage image innerComponents
   in adjoinComponents newComponents

withOrigin : BackgroundOriginDescriptor -> 
             ComposedBackgroundDescriptor a sz1 sz2 sz3
withOrigin originDescriptor composedDescriptor =
   let origin = originDescriptor backgroundOriginFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithOrigin origin innerComponents
   in adjoinComponents newComponents
  
withClip : BackgroundClipDescriptor -> 
           ComposedBackgroundDescriptor a sz1 sz2 sz3
withClip clipDescriptor composedDescriptor =
   let clip = clipDescriptor backgroundClipFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithClip clip innerComponents
   in adjoinComponents newComponents

withAttachment : BackgroundAttachmentDescriptor -> 
                 ComposedBackgroundDescriptor a sz1 sz2 sz3
withAttachment attachmentDescriptor composedDescriptor  =
   let attachment = attachmentDescriptor backgroundAttachmentFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithAttachment attachment innerComponents
   in adjoinComponents newComponents
