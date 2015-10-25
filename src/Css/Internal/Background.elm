module Css.Internal.Background
  ( BackgroundColorDescriptor, backgroundColorFactory
  , HorizontalSide (..), VerticalSide (..)
  , horizontalSideValue, verticalSideValue
  , BackgroundPositionDescriptor
  , backgroundPositionFactory, backgroundPositionValue
  , BackgroundSizeDescriptor
  , backgroundSizeFactory, backgroundSizeValue
  , BackgroundRepeatDescriptor
  , backgroundRepeatFactory, backgroundRepeatValue
  , BackgroundImageDescriptor
  , backgroundImageFactory, backgroundImageValue
  , BackgroundOriginDescriptor
  , backgroundOriginFactory, backgroundOriginValue
  , BackgroundClipDescriptor
  , backgroundClipFactory, backgroundClipValue
  , BackgroundAttachmentDescriptor
  , backgroundAttachmentFactory, backgroundAttachmentValue
  ) where

import String

import Css.Internal.Box exposing (BoxType, boxTypeValue)  
import Css.Internal.Color exposing (CssColor (..), ColorFactory, colorFactory)
import Css.Internal.Common exposing 
  (autoValue, initialValue, inheritValue, noneValue, otherValue)
import Css.Internal.Property exposing 
  ( Value, Element
  , simpleElement, stringValue, spacePairValue
  )
import Css.Internal.Size exposing (Size, sizeValue)
-------------------------------------------------------------------------------

type alias BackgroundColorDescriptor = BackgroundColorFactory -> CssColor

type alias TransparentColorFactory = { transparent: CssColor }

type alias BackgroundColorFactory = ColorFactory TransparentColorFactory

backgroundColorFactory : BackgroundColorFactory
backgroundColorFactory = 
  { colorFactory | transparent = OtherColor (simpleElement "transparent") }

-------------------------------------------------------------------------------

type HorizontalSide 
  = HorizontalSide String
  | InitialHorizontalSide
  | InheritHorizontalSide
  | OtherHorizontalSide Element

horizontalSideValue : HorizontalSide -> Value 
horizontalSideValue side = 
  case side of
    HorizontalSide str -> stringValue str
    InitialHorizontalSide -> initialValue
    InheritHorizontalSide -> inheritValue
    OtherHorizontalSide valElement -> otherValue valElement

type VerticalSide
  = VerticalSide String
  | InitialVerticalSide
  | InheritVerticalSide
  | OtherVerticalSide Element

verticalSideValue : VerticalSide -> Value 
verticalSideValue side = 
  case side of
    VerticalSide str -> stringValue str
    InitialVerticalSide -> initialValue
    InheritVerticalSide -> inheritValue
    OtherVerticalSide valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias BackgroundPositionDescriptor sz = 
    BackgroundPositionFactory sz -> BackgroundPosition sz

type BackgroundPosition sz
  = SizedBackgroundPosition (Size sz, Size sz)
  | SidedBackgroundPosition (HorizontalSide, VerticalSide)
  | InitialBackgroundPosition
  | InheritBackgroundPosition
  | OtherBackgroundPosition String

type alias BackgroundPositionFactory sz =
  { sizedPosition : Size sz -> Size sz -> BackgroundPosition sz
  , sidedPosition : HorizontalSide -> VerticalSide -> BackgroundPosition sz
  , initial: BackgroundPosition sz
  , inherit: BackgroundPosition sz
  , other: String -> BackgroundPosition sz
  }

backgroundPositionFactory : BackgroundPositionFactory sz
backgroundPositionFactory = 
  { sizedPosition horiz vert = SizedBackgroundPosition (horiz, vert)
  , sidedPosition horiz vert = SidedBackgroundPosition (horiz, vert)
  , initial = InitialBackgroundPosition
  , inherit = InheritBackgroundPosition
  , other str = OtherBackgroundPosition str
  }

backgroundPositionValue : BackgroundPosition sz -> Value 
backgroundPositionValue position =
  case position of
    SizedBackgroundPosition sizes -> 
      let valueFactory = spacePairValue sizeValue sizeValue
      in valueFactory sizes
    SidedBackgroundPosition sides -> 
      let valueFactory = spacePairValue horizontalSideValue verticalSideValue
      in valueFactory sides
    InitialBackgroundPosition -> initialValue
    InheritBackgroundPosition -> inheritValue
    OtherBackgroundPosition str -> stringValue str

-------------------------------------------------------------------------------

type alias BackgroundSizeDescriptor sz = 
  BackgroundSizeFactory sz -> BackgroundSize sz

type BackgroundSize sz
  = BackgroundSize (Size sz) (Size sz)
  | PartiallySpecifiedBackgroundSize (Size sz)
  | NamedBackgroundSize String
  | AutoBackgroundSize
  | InitialBackgroundSize
  | InheritBackgroundSize
  | OtherBackgroundSize Element

type alias BackgroundSizeFactory sz =
  { backgroundSize : (Size sz) -> (Size sz) -> BackgroundSize sz
  , partial : (Size sz) -> BackgroundSize sz
  , named : String -> BackgroundSize sz
  , auto : BackgroundSize sz
  , initial: BackgroundSize sz
  , inherit: BackgroundSize sz
  , other: Element -> BackgroundSize sz
  }

backgroundSizeFactory : BackgroundSizeFactory sz
backgroundSizeFactory =
  { backgroundSize width height = BackgroundSize width height
  , partial width = PartiallySpecifiedBackgroundSize width
  , named str = NamedBackgroundSize str
  , auto = AutoBackgroundSize
  , initial = InitialBackgroundSize
  , inherit = InheritBackgroundSize
  , other valElement = OtherBackgroundSize valElement
  }
  
backgroundSizeValue : BackgroundSize sz -> Value 
backgroundSizeValue bgSize =
  case bgSize of
    BackgroundSize width height -> 
      let valueFactory = spacePairValue sizeValue sizeValue
      in valueFactory (width, height)
    PartiallySpecifiedBackgroundSize width ->
      let valueFactory = spacePairValue sizeValue identity
      in valueFactory (width, autoValue)
    NamedBackgroundSize str -> stringValue str
    AutoBackgroundSize -> autoValue
    InitialBackgroundSize -> initialValue
    InheritBackgroundSize -> inheritValue
    OtherBackgroundSize valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias BackgroundRepeatDescriptor = 
  BackgroundRepeatFactory -> BackgroundRepeat

type BackgroundRepeat 
  = BackgroundRepeat String
  | InitialBackgroundRepeat
  | InheritBackgroundRepeat
  | OtherBackgroundRepeat Element

type alias BackgroundRepeatFactory =
  { repeat : String -> BackgroundRepeat 
  , initial : BackgroundRepeat 
  , inherit : BackgroundRepeat 
  , other : Element -> BackgroundRepeat 
  }  

backgroundRepeatFactory : BackgroundRepeatFactory
backgroundRepeatFactory =
  { repeat str = BackgroundRepeat str 
  , initial = InitialBackgroundRepeat 
  , inherit = InheritBackgroundRepeat 
  , other valElement = OtherBackgroundRepeat valElement 
  }  

backgroundRepeatValue : BackgroundRepeat -> Value 
backgroundRepeatValue repeat = 
  case repeat of
    BackgroundRepeat str -> stringValue str
    InitialBackgroundRepeat -> initialValue
    InheritBackgroundRepeat -> inheritValue
    OtherBackgroundRepeat valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias BackgroundImageDescriptor = 
  BackgroundImageFactory -> BackgroundImage

type BackgroundImage 
  = BackgroundImage String
  | NoBackgroundImage
  | InitialBackgroundImage
  | InheritBackgroundImage
  | OtherBackgroundImage Element

type alias BackgroundImageFactory =
  { url : String -> BackgroundImage 
  , none : BackgroundImage 
  , initial : BackgroundImage 
  , inherit : BackgroundImage 
  , other : Element -> BackgroundImage 
  }  

backgroundImageFactory : BackgroundImageFactory
backgroundImageFactory =
  { url str = BackgroundImage str 
  , none = NoBackgroundImage
  , initial = InitialBackgroundImage 
  , inherit = InheritBackgroundImage 
  , other valElement = OtherBackgroundImage valElement 
  }  

backgroundImageValue : BackgroundImage -> Value 
backgroundImageValue bgImage = 
  case bgImage of
    BackgroundImage imgUrl -> 
      stringValue (String.concat ["url(\"", imgUrl ,"\")"])
    NoBackgroundImage -> noneValue
    InitialBackgroundImage -> initialValue
    InheritBackgroundImage -> inheritValue
    OtherBackgroundImage valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias BackgroundOriginDescriptor = 
  BackgroundOriginFactory -> BackgroundOrigin

type BackgroundOrigin 
  = BackgroundOrigin BoxType
  | InitialBackgroundOrigin
  | InheritBackgroundOrigin
  | OtherBackgroundOrigin Element

type alias BackgroundOriginFactory =
  { origin : BoxType -> BackgroundOrigin 
  , initial : BackgroundOrigin 
  , inherit : BackgroundOrigin 
  , other : Element -> BackgroundOrigin 
  }  

backgroundOriginFactory : BackgroundOriginFactory
backgroundOriginFactory =
  { origin boxType = BackgroundOrigin boxType 
  , initial = InitialBackgroundOrigin 
  , inherit = InheritBackgroundOrigin 
  , other valElement = OtherBackgroundOrigin valElement 
  }  

backgroundOriginValue : BackgroundOrigin -> Value 
backgroundOriginValue bgOrigin = 
  case bgOrigin of
    BackgroundOrigin boxType -> boxTypeValue boxType
    InitialBackgroundOrigin -> initialValue
    InheritBackgroundOrigin -> inheritValue
    OtherBackgroundOrigin valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias BackgroundClipDescriptor = 
  BackgroundClipFactory -> BackgroundClip

type BackgroundClip 
  = BackgroundClip BoxType
  | InitialBackgroundClip
  | InheritBackgroundClip
  | OtherBackgroundClip Element

type alias BackgroundClipFactory =
  { clip : BoxType -> BackgroundClip 
  , initial : BackgroundClip 
  , inherit : BackgroundClip 
  , other : Element -> BackgroundClip 
  }  

backgroundClipFactory : BackgroundClipFactory
backgroundClipFactory =
  { clip boxType = BackgroundClip boxType 
  , initial = InitialBackgroundClip 
  , inherit = InheritBackgroundClip 
  , other valElement = OtherBackgroundClip valElement 
  }  

backgroundClipValue : BackgroundClip -> Value 
backgroundClipValue bgClip = 
  case bgClip of
    BackgroundClip boxType -> boxTypeValue boxType
    InitialBackgroundClip -> initialValue
    InheritBackgroundClip -> inheritValue
    OtherBackgroundClip valElement -> otherValue valElement

-------------------------------------------------------------------------------

type alias BackgroundAttachmentDescriptor = 
  BackgroundAttachmentFactory -> BackgroundAttachment

type BackgroundAttachment 
  = BackgroundAttachment String
  | InitialBackgroundAttachment
  | InheritBackgroundAttachment
  | OtherBackgroundAttachment Element

type alias BackgroundAttachmentFactory =
  { bgAttachment : String -> BackgroundAttachment 
  , initial : BackgroundAttachment 
  , inherit : BackgroundAttachment 
  , other : Element -> BackgroundAttachment 
  }  

backgroundAttachmentFactory : BackgroundAttachmentFactory
backgroundAttachmentFactory =
  { bgAttachment str = BackgroundAttachment str 
  , initial = InitialBackgroundAttachment 
  , inherit = InheritBackgroundAttachment 
  , other valElement = OtherBackgroundAttachment valElement 
  }  

backgroundAttachmentValue : BackgroundAttachment -> Value 
backgroundAttachmentValue bgAttachment = 
  case bgAttachment of
    BackgroundAttachment str -> stringValue str
    InitialBackgroundAttachment -> initialValue
    InheritBackgroundAttachment -> inheritValue
    OtherBackgroundAttachment valElement -> otherValue valElement
