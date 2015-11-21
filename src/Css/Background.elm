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

import Css.Internal.Background as Background
import Css.Internal.Box.Sizing as BoxSizing
import Css.Internal.Color as Color
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides
import Css.Internal.Stylesheet as Stylesheet

-------------------------------------------------------------------------------

backgroundPosition : Background.BackgroundPositionDescriptor sz1 sz2 -> 
                     Stylesheet.PropertyRuleAppender
backgroundPosition descriptor = 
  let value = descriptor Background.backgroundPositionFactory
  in Stylesheet.simpleProperty "background-position" value
  
placed : Sides.HorizontalSide -> 
         Sides.VerticalSide -> 
         Background.BackgroundPositionDescriptor sz1 sz2
placed horiz vert = \factory -> factory.sidedPosition horiz vert

positioned : Linear.NubSizeDescriptor {} sz1 -> 
             Linear.NubSizeDescriptor {} sz2 -> 
             Background.BackgroundPositionDescriptor sz1 sz2
positioned horiz vert = \factory -> factory.sizedPosition horiz vert

-------------------------------------------------------------------------------

backgroundSize : Background.BackgroundSizeDescriptor sz -> 
                 Stylesheet.PropertyRuleAppender
backgroundSize descriptor = 
  let bgSize = descriptor Background.backgroundSizeFactory
  in Stylesheet.simpleProperty "background-size" bgSize

contain : Background.BackgroundSizeDescriptor sz
contain = \factory -> factory.named "contain"

cover : Background.BackgroundSizeDescriptor sz
cover = \factory -> factory.named "cover"

by : Linear.NubSizeDescriptor {} sz -> 
     Linear.NubSizeDescriptor {} sz -> 
     Background.BackgroundSizeDescriptor sz
by width height = \factory -> factory.backgroundSize width height

bgWidth : Linear.NubSizeDescriptor {} sz -> Background.BackgroundSizeDescriptor sz
bgWidth width = \factory -> factory.partial width

-------------------------------------------------------------------------------

backgroundColor : Color.ColorDescriptor -> Stylesheet.PropertyRuleAppender
backgroundColor colorDescriptor = 
  let colorValue = colorDescriptor Color.colorFactory
  in Stylesheet.simpleProperty "background-color" colorValue

-------------------------------------------------------------------------------

backgroundImage : Background.BackgroundImageDescriptor -> 
                  Stylesheet.PropertyRuleAppender
backgroundImage descriptor = 
  let bgImage = descriptor Background.backgroundImageFactory
  in Stylesheet.simpleProperty "background-image" bgImage

-- TODO Validate that it's a proper url?
url : String -> Background.BackgroundImageDescriptor
url bgImageUrl = \factory -> factory.url bgImageUrl

-------------------------------------------------------------------------------

backgroundRepeat : Background.BackgroundRepeatDescriptor -> 
                   Stylesheet.PropertyRuleAppender
backgroundRepeat descriptor = 
  let repeatValue = descriptor Background.backgroundRepeatFactory
  in Stylesheet.simpleProperty "background-repeat" repeatValue

repeat : Background.BackgroundRepeatDescriptor
repeat = \factory -> factory.repeat "repeat"

space : Background.BackgroundRepeatDescriptor
space = \factory -> factory.repeat "space"

roundRepeat : Background.BackgroundRepeatDescriptor
roundRepeat = \factory -> factory.repeat "round"

noRepeat : Background.BackgroundRepeatDescriptor
noRepeat = \factory -> factory.repeat "no-repeat"

repeatX : Background.BackgroundRepeatDescriptor
repeatX = \factory -> factory.repeat "repeat-x"

repeatY : Background.BackgroundRepeatDescriptor
repeatY = \factory -> factory.repeat "repeat-y"

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

backgroundAttachment : Background.BackgroundAttachmentDescriptor -> 
                       Stylesheet.PropertyRuleAppender
backgroundAttachment descriptor = 
  let bgAttachment = descriptor Background.backgroundAttachmentFactory
  in Stylesheet.simpleProperty "background-attachment" bgAttachment

attachFixed : Background.BackgroundAttachmentDescriptor
attachFixed = \factory -> factory.bgAttachment "fixed"

attachScroll : Background.BackgroundAttachmentDescriptor
attachScroll = \factory -> factory.bgAttachment "scroll"

attachLocal : Background.BackgroundAttachmentDescriptor
attachLocal = \factory -> factory.bgAttachment "local"

-------------------------------------------------------------------------------
{-
The background shorthand property sets all the background properties in one 
declaration:

    background: color image position/size repeat origin clip attachment initial|inherit;

If one of the properties in the shorthand declaration is the background size 
property, a / (slash) must separate it from the background position property.
-}
background : Background.BackgroundDescriptor rec sz1 sz2 sz3 -> 
             Stylesheet.PropertyRuleAppender
background backgroundDescriptor = 
  let backgroundRecord = backgroundDescriptor Background.initialBackgroundFactory
      bgValue = backgroundRecord.background |> Background.backgroundValue
  in Stylesheet.simpleProperty "background" bgValue

withPosition : Background.BackgroundPositionDescriptor sz1 sz2 -> 
               Maybe (Background.BackgroundSizeDescriptor sz3) -> 
               Background.ComposedBackgroundDescriptor rec sz1 sz2 sz3
withPosition positionDescriptor 
             maybeSizeDescriptor 
             composedDescriptor =
   let position = positionDescriptor Background.backgroundPositionFactory
       maybeSize = 
         maybeSizeDescriptor 
         |> Maybe.map (\desc -> desc Background.backgroundSizeFactory)
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = 
         Background.withPositionAndSize position maybeSize innerComponents
   in Background.adjoinComponents newComponents

withBgColor : Color.NubColorDescriptor {} -> 
              Background.ComposedBackgroundDescriptor rec sz1 sz2 sz3 
withBgColor colorDescriptor composedDescriptor = 
   let colorValue = colorDescriptor Color.nubColorFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withColor colorValue innerComponents
   in Background.adjoinComponents newComponents

withRepeat : Background.BackgroundRepeatDescriptor -> 
             Background.ComposedBackgroundDescriptor rec sz1 sz2 sz3
withRepeat repeatDescriptor composedDescriptor =
   let repeat = repeatDescriptor Background.backgroundRepeatFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withRepeat repeat innerComponents
   in Background.adjoinComponents newComponents

withImage : Background.BackgroundImageDescriptor -> 
            Background.ComposedBackgroundDescriptor rec sz1 sz2 sz3
withImage imageDescriptor composedDescriptor =
   let image = imageDescriptor Background.backgroundImageFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withImage image innerComponents
   in Background.adjoinComponents newComponents

withOrigin : BoxSizing.NubBoxTypeDescriptor -> 
             Background.ComposedBackgroundDescriptor rec sz1 sz2 sz3
withOrigin originDescriptor composedDescriptor =
   let origin = originDescriptor BoxSizing.nubBoxTypeFactory 
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withOrigin origin innerComponents
   in Background.adjoinComponents newComponents
  
withClip : BoxSizing.NubBoxTypeDescriptor -> 
           Background.ComposedBackgroundDescriptor rec sz1 sz2 sz3
withClip clipDescriptor composedDescriptor =
   let clip = clipDescriptor BoxSizing.nubBoxTypeFactory 
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withClip clip innerComponents
   in Background.adjoinComponents newComponents

withAttachment : Background.BackgroundAttachmentDescriptor -> 
                 Background.ComposedBackgroundDescriptor rec sz1 sz2 sz3
withAttachment attachmentDescriptor composedDescriptor  =
   let attachment = attachmentDescriptor Background.backgroundAttachmentFactory
       innerComponents = composedDescriptor.backgroundComponents
       newComponents = Background.withAttachment attachment innerComponents
   in Background.adjoinComponents newComponents
