module Css.Background
  (
  -- * The background-position.

    backgroundPosition
  , placed
  , positioned

  -- * The background-size.

  , backgroundSize
  , contain, cover
  , by, bgWidth

  -- * The background-color.

  , backgroundColor

  -- * The background-repeat.

  , backgroundRepeat
  , repeat, space, roundRepeat, noRepeat
  , repeatX, repeatY

  -- * The background-origin.

  , backgroundOrigin

  -- * The background-clip.

  , backgroundClip

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

import Css.Internal.Box.Sizing as BoxSizing
import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides

import Css.Internal.Background exposing (..)


-------------------------------------------------------------------------------

backgroundPosition : BackgroundPositionDescriptor sz1 sz2 -> PropertyRuleAppender
backgroundPosition descriptor = 
  let value = descriptor backgroundPositionFactory
  in simpleProperty "background-position" value
  
placed : Sides.HorizontalSide -> Sides.VerticalSide -> BackgroundPositionDescriptor sz1 sz2
placed horiz vert factory = factory.sidedPosition horiz vert

positioned : Linear.SizeDescriptor {} sz1 -> 
             Linear.SizeDescriptor {} sz2 -> 
             BackgroundPositionDescriptor sz1 sz2
positioned horiz vert factory = factory.sizedPosition horiz vert

-------------------------------------------------------------------------------

backgroundSize : BackgroundSizeDescriptor sz -> PropertyRuleAppender
backgroundSize descriptor = 
  let bgSize = descriptor backgroundSizeFactory
  in simpleProperty "background-size" bgSize

contain : BackgroundSizeDescriptor sz
contain factory = backgroundSizeFactory.named "contain"

cover : BackgroundSizeDescriptor sz
cover factory = backgroundSizeFactory.named "cover"

by : Linear.SizeDescriptor {} sz -> 
     Linear.SizeDescriptor {} sz -> 
     BackgroundSizeDescriptor sz
by width height factory = backgroundSizeFactory.backgroundSize width height

bgWidth : Linear.SizeDescriptor {} sz -> BackgroundSizeDescriptor sz
bgWidth width factory = backgroundSizeFactory.partial width

-------------------------------------------------------------------------------

backgroundColor : Color.BasicColorDescriptor -> PropertyRuleAppender
backgroundColor colorDescriptor = 
  let colorValue = colorDescriptor Color.colorFactory
  in simpleProperty "background-color" colorValue

-------------------------------------------------------------------------------

backgroundImage : BackgroundImageDescriptor -> PropertyRuleAppender
backgroundImage descriptor = 
  let bgImage = descriptor backgroundImageFactory
  in simpleProperty "background-image" bgImage

-- TODO Validate that it's a proper url?
url : String -> BackgroundImageDescriptor
url bgImageUrl factory = factory.url bgImageUrl

-------------------------------------------------------------------------------

backgroundRepeat : BackgroundRepeatDescriptor -> PropertyRuleAppender
backgroundRepeat descriptor = 
  let repeatValue = descriptor backgroundRepeatFactory
  in simpleProperty "background-repeat" repeatValue

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

backgroundOrigin : BoxSizing.BoxSizingDescriptor -> PropertyRuleAppender
backgroundOrigin descriptor = 
  let bgOrigin = descriptor BoxSizing.boxTypeFactory  
  in simpleProperty "background-origin" bgOrigin

-------------------------------------------------------------------------------

backgroundClip : BoxSizing.BoxSizingDescriptor -> PropertyRuleAppender
backgroundClip descriptor = 
  let bgClip = descriptor BoxSizing.boxTypeFactory 
  in simpleProperty "background-clip" bgClip

-------------------------------------------------------------------------------

backgroundAttachment : BackgroundAttachmentDescriptor -> PropertyRuleAppender
backgroundAttachment descriptor = 
  let bgAttachment = descriptor backgroundAttachmentFactory
  in simpleProperty "background-attachment" bgAttachment

attachFixed : BackgroundAttachmentDescriptor
attachFixed factory = factory.bgAttachment "fixed"

attachScroll : BackgroundAttachmentDescriptor
attachScroll factory = factory.bgAttachment "scroll"

attachLocal : BackgroundAttachmentDescriptor
attachLocal factory = factory.bgAttachment "local"

-------------------------------------------------------------------------------
{-
The background shorthand property sets all the background properties in one 
declaration:

    background: color image position/size repeat origin clip attachment initial|inherit;

If one of the properties in the shorthand declaration is the background size 
property, a / (slash) must separate it from the background position property.
-}
background : BackgroundDescriptor a sz1 sz2 sz3 -> PropertyRuleAppender
background backgroundDescriptor = 
  let backgroundRecord = backgroundDescriptor initialBackgroundFactory
      bgValue = backgroundRecord.background |> backgroundValue
  in simpleProperty "background" bgValue

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

withBgColor : Color.NubColorDescriptor {} -> 
              ComposedBackgroundDescriptor a sz1 sz2 sz3 
withBgColor colorDescriptor composedDescriptor = 
   let colorValue = colorDescriptor Color.nubColorFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithColor colorValue innerComponents
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

withOrigin : BoxSizing.NubBoxTypeDescriptor -> 
             ComposedBackgroundDescriptor a sz1 sz2 sz3
withOrigin originDescriptor composedDescriptor =
   let origin = originDescriptor BoxSizing.nubBoxTypeFactory 
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = WithOrigin origin innerComponents
   in adjoinComponents newComponents
  
withClip : BoxSizing.NubBoxTypeDescriptor -> 
           ComposedBackgroundDescriptor a sz1 sz2 sz3
withClip clipDescriptor composedDescriptor =
   let clip = clipDescriptor BoxSizing.nubBoxTypeFactory 
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
