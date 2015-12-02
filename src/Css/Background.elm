module Css.Background
  (
  -- * Generic background property.

    background
  , withPosition, withBgColor, withRepeat
  , withImage, withOrigin, withClip, withAttachment

  -- * The background-attachment.

  , backgroundAttachment
  , attachFixed, attachScroll, attachLocal

  -- * The background-clip.

  , backgroundClip

  -- * The background-color.

  , backgroundColor

  -- * The background-image.

  , backgroundImage
  , url

  -- * The background-origin.

  , backgroundOrigin

  -- * The background-position.

  , backgroundPosition
  , placed
  , positioned

  -- * The background-repeat.

  , backgroundRepeat
  , repeat, space, roundRepeat, noRepeat
  , repeatX, repeatY

  -- * The background-size.

  , backgroundSize
  , contain, cover
  , by, bgWidth
  
  ) where

import Css.Internal.Background as Background
import Css.Internal.Background.Attachment as Attachment
import Css.Internal.Background.Image as BgImage
import Css.Internal.Background.Position as BgPosition
import Css.Internal.Background.Repeat as Repeat
import Css.Internal.Background.Size as BgSize
import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides
import Css.Internal.Layout.BoxSizing as BoxSizing
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------
{-
The background shorthand property sets all the background properties in one 
declaration:

    background: color image position/size repeat origin clip attachment initial|inherit;

If one of the properties in the shorthand declaration is the background size 
property, a / (slash) must separate it from the background position property.
-}
background : Background.BackgroundDescriptor rec -> 
             Stylesheet.PropertyRuleAppender
background backgroundDescriptor = 
  let backgroundRecord = backgroundDescriptor Background.initialBackgroundFactory
      bgValue = backgroundRecord.background |> Background.backgroundValue
  in Stylesheet.simpleProperty "background" bgValue

withAttachment : Attachment.BackgroundAttachmentDescriptor -> 
                 Background.ComposedBackgroundDescriptor rec
withAttachment attachmentDescriptor composedDescriptor  =
   let attachment = attachmentDescriptor Attachment.backgroundAttachmentFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withAttachment attachment innerComponents
   in Background.adjoinComponents newComponents

withBgColor : Color.NubColorDescriptor {} -> 
              Background.ComposedBackgroundDescriptor rec 
withBgColor colorDescriptor composedDescriptor = 
   let colorValue = colorDescriptor Color.nubColorFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withColor colorValue innerComponents
   in Background.adjoinComponents newComponents

withClip : BoxSizing.NubBoxTypeDescriptor -> 
           Background.ComposedBackgroundDescriptor rec
withClip clipDescriptor composedDescriptor =
   let clip = clipDescriptor BoxSizing.nubBoxTypeFactory 
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withClip clip innerComponents
   in Background.adjoinComponents newComponents

withImage : BgImage.BackgroundImageDescriptor -> 
            Background.ComposedBackgroundDescriptor rec
withImage imageDescriptor composedDescriptor =
   let image = imageDescriptor BgImage.backgroundImageFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withImage image innerComponents
   in Background.adjoinComponents newComponents

withOrigin : BoxSizing.NubBoxTypeDescriptor -> 
             Background.ComposedBackgroundDescriptor rec
withOrigin originDescriptor composedDescriptor =
   let origin = originDescriptor BoxSizing.nubBoxTypeFactory 
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withOrigin origin innerComponents
   in Background.adjoinComponents newComponents

-- TODO - Size can actually take "auto" and image can take "none"
withPosition : BgPosition.BackgroundPositionDescriptor sz1 sz2 -> 
               Maybe (BgSize.BackgroundSizeDescriptor sz3) -> 
               Background.ComposedBackgroundDescriptor rec
withPosition positionDescriptor 
             maybeSizeDescriptor 
             composedDescriptor =
   let position = positionDescriptor BgPosition.backgroundPositionFactory
       maybeSize = 
         maybeSizeDescriptor 
         |> Maybe.map (\desc -> desc BgSize.backgroundSizeFactory)
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = 
         Background.withPositionAndSize position maybeSize innerComponents
   in Background.adjoinComponents newComponents

withRepeat : Repeat.BackgroundRepeatDescriptor -> 
             Background.ComposedBackgroundDescriptor rec
withRepeat repeatDescriptor composedDescriptor =
   let repeat = repeatDescriptor Repeat.backgroundRepeatFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withRepeat repeat innerComponents
   in Background.adjoinComponents newComponents

-------------------------------------------------------------------------------

backgroundAttachment : Attachment.BackgroundAttachmentDescriptor -> 
                       Stylesheet.PropertyRuleAppender
backgroundAttachment descriptor = 
  let bgAttachment = descriptor Attachment.backgroundAttachmentFactory
  in Stylesheet.simpleProperty "background-attachment" bgAttachment

attachFixed : Attachment.BackgroundAttachmentDescriptor
attachFixed = \factory -> factory.bgAttachment "fixed"

attachScroll : Attachment.BackgroundAttachmentDescriptor
attachScroll = \factory -> factory.bgAttachment "scroll"

attachLocal : Attachment.BackgroundAttachmentDescriptor
attachLocal = \factory -> factory.bgAttachment "local"

-------------------------------------------------------------------------------

backgroundColor : Color.ColorDescriptor -> Stylesheet.PropertyRuleAppender
backgroundColor colorDescriptor = 
  let colorValue = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "background-color" colorValue

-------------------------------------------------------------------------------

backgroundImage : BgImage.BackgroundImageDescriptor -> 
                  Stylesheet.PropertyRuleAppender
backgroundImage descriptor = 
  let bgImage = descriptor BgImage.backgroundImageFactory
  in Stylesheet.simpleProperty "background-image" bgImage

url : String -> BgImage.BackgroundImageDescriptor
url bgImageUrl = \factory -> factory.url bgImageUrl

-------------------------------------------------------------------------------

backgroundOrigin : BoxSizing.BoxSizingDescriptor -> 
                   Stylesheet.PropertyRuleAppender
backgroundOrigin descriptor = 
  let bgOrigin = descriptor BoxSizing.boxTypeFactory  
  in Stylesheet.simpleProperty "background-origin" bgOrigin

backgroundClip : BoxSizing.BoxSizingDescriptor -> 
                 Stylesheet.PropertyRuleAppender
backgroundClip descriptor = 
  let bgClip = descriptor BoxSizing.boxTypeFactory 
  in Stylesheet.simpleProperty "background-clip" bgClip

-------------------------------------------------------------------------------

backgroundPosition : BgPosition.BackgroundPositionDescriptor szH szV -> 
                     Stylesheet.PropertyRuleAppender
backgroundPosition descriptor = 
  let value = descriptor BgPosition.backgroundPositionFactory
  in Stylesheet.simpleProperty "background-position" value
  
placed : Sides.HorizontalSide -> 
         Sides.VerticalSide -> 
         BgPosition.BackgroundPositionDescriptor szH szV
placed horiz vert = \factory -> factory.sidedPosition horiz vert

positioned : Linear.NubSizeDescriptor {} szH -> 
             Linear.NubSizeDescriptor {} szV -> 
             BgPosition.BackgroundPositionDescriptor szH szV
positioned horiz vert = \factory -> factory.sizedPosition horiz vert

-------------------------------------------------------------------------------
-- TODO syntax that takes horizontal and vertical values separately
backgroundRepeat : Repeat.BackgroundRepeatDescriptor -> 
                   Stylesheet.PropertyRuleAppender
backgroundRepeat descriptor = 
  let repeatValue = descriptor Repeat.backgroundRepeatFactory
  in Stylesheet.simpleProperty "background-repeat" repeatValue

repeat : Repeat.BackgroundRepeatDescriptor
repeat = \factory -> factory.repeat "repeat"

space : Repeat.BackgroundRepeatDescriptor
space = \factory -> factory.repeat "space"

roundRepeat : Repeat.BackgroundRepeatDescriptor
roundRepeat = \factory -> factory.repeat "round"

noRepeat : Repeat.BackgroundRepeatDescriptor
noRepeat = \factory -> factory.repeat "no-repeat"

repeatX : Repeat.BackgroundRepeatDescriptor
repeatX = \factory -> factory.repeat "repeat-x"

repeatY : Repeat.BackgroundRepeatDescriptor
repeatY = \factory -> factory.repeat "repeat-y"

-------------------------------------------------------------------------------

backgroundSize : BgSize.BackgroundSizeDescriptor sz -> 
                 Stylesheet.PropertyRuleAppender
backgroundSize descriptor = 
  let bgSize = descriptor BgSize.backgroundSizeFactory
  in Stylesheet.simpleProperty "background-size" bgSize

contain : BgSize.BackgroundSizeDescriptor sz
contain = \factory -> factory.named "contain"

cover : BgSize.BackgroundSizeDescriptor sz
cover = \factory -> factory.named "cover"

by : Linear.NubSizeDescriptor {} sz -> 
     Linear.NubSizeDescriptor {} sz -> 
     BgSize.BackgroundSizeDescriptor sz
by width height = \factory -> factory.backgroundSize width height

-- TODO two-value syntax can take "auto" twice or in the first position.
bgWidth : Linear.NubSizeDescriptor {} sz -> BgSize.BackgroundSizeDescriptor sz
bgWidth width = \factory -> factory.partial width
