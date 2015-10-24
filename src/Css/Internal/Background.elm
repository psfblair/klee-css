module Css.Internal.Background
  ( BackgroundColorDescriptor, backgroundColorFactory
  , HorizontalSide (..), VerticalSide (..)
  , horizontalSideValueFactory, verticalSideValueFactory
  , BackgroundPositionDescriptor
  , backgroundPositionFactory, backgroundPositionValueFactory
  , BackgroundSizeDescriptor
  , backgroundSizeFactory, backgroundSizeValueFactory
  , BackgroundRepeatDescriptor
  , backgroundRepeatFactory, backgroundRepeatValueFactory
  , BackgroundImageDescriptor
  , backgroundImageFactory, backgroundImageValueFactory
  , BackgroundOriginDescriptor
  , backgroundOriginFactory, backgroundOriginValueFactory
  , BackgroundClipDescriptor
  , backgroundClipFactory, backgroundClipValueFactory
  , BackgroundAttachmentDescriptor
  , backgroundAttachmentFactory, backgroundAttachmentValueFactory
  ) where

import String

import Css.Internal.Box exposing (BoxType, boxTypeValueFactory)  
import Css.Internal.Color exposing (CssColor (..), ColorFactory, colorFactory)
import Css.Internal.Common exposing 
  (autoValue, initialValue, inheritValue, noneValue, otherValue)
import Css.Internal.Property exposing 
  ( ValueFactory, stringValueFactory, valueValueFactory, spacePairValueFactory)
import Css.Internal.Size exposing (Size, sizeValueFactory)
-------------------------------------------------------------------------------

type alias BackgroundColorDescriptor = BackgroundColorFactory -> CssColor

type alias TransparentColorFactory = { transparent: CssColor }

type alias BackgroundColorFactory = ColorFactory TransparentColorFactory

backgroundColorFactory : BackgroundColorFactory
backgroundColorFactory = 
  { colorFactory | transparent = OtherColor "transparent" }

-------------------------------------------------------------------------------

type HorizontalSide 
  = HorizontalSide String
  | InitialHorizontalSide
  | InheritHorizontalSide
  | OtherHorizontalSide String

horizontalSideValueFactory : ValueFactory HorizontalSide
horizontalSideValueFactory = 
  { value side = 
      case side of
        HorizontalSide str -> stringValueFactory.value str
        InitialHorizontalSide -> initialValue
        InheritHorizontalSide -> inheritValue
        OtherHorizontalSide str -> otherValue str
  }

type VerticalSide
  = VerticalSide String
  | InitialVerticalSide
  | InheritVerticalSide
  | OtherVerticalSide String

verticalSideValueFactory : ValueFactory VerticalSide
verticalSideValueFactory = 
  { value side = 
      case side of
        VerticalSide str -> stringValueFactory.value str
        InitialVerticalSide -> initialValue
        InheritVerticalSide -> inheritValue
        OtherVerticalSide str -> otherValue str
  }

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

backgroundPositionValueFactory : ValueFactory (BackgroundPosition sz)
backgroundPositionValueFactory =
  { value position = 
      case position of
        SizedBackgroundPosition sizes -> 
          let szf = sizeValueFactory
              valueFactory = spacePairValueFactory szf szf
          in valueFactory.value sizes
        SidedBackgroundPosition sides -> 
          let hsdf = horizontalSideValueFactory
              vsdf = verticalSideValueFactory
              valueFactory = spacePairValueFactory hsdf vsdf
          in valueFactory.value sides
        InitialBackgroundPosition -> initialValue
        InheritBackgroundPosition -> inheritValue
        OtherBackgroundPosition str -> stringValueFactory.value str
  }

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
  | OtherBackgroundSize String

type alias BackgroundSizeFactory sz =
  { backgroundSize : (Size sz) -> (Size sz) -> BackgroundSize sz
  , partial : (Size sz) -> BackgroundSize sz
  , named : String -> BackgroundSize sz
  , auto : BackgroundSize sz
  , initial: BackgroundSize sz
  , inherit: BackgroundSize sz
  , other: String -> BackgroundSize sz
  }

backgroundSizeFactory : BackgroundSizeFactory sz
backgroundSizeFactory =
  { backgroundSize width height = BackgroundSize width height
  , partial width = PartiallySpecifiedBackgroundSize width
  , named str = NamedBackgroundSize str
  , auto = AutoBackgroundSize
  , initial = InitialBackgroundSize
  , inherit = InheritBackgroundSize
  , other str = OtherBackgroundSize str
  }
  
backgroundSizeValueFactory : ValueFactory (BackgroundSize sz)
backgroundSizeValueFactory =
  { value bgSize =
      case bgSize of
        BackgroundSize width height -> 
          let szf = sizeValueFactory
              valueFactory = spacePairValueFactory szf szf
          in valueFactory.value (width, height)
        PartiallySpecifiedBackgroundSize width ->
          let szf = sizeValueFactory
              valueFactory = spacePairValueFactory szf valueValueFactory
          in valueFactory.value (width, autoValue)
        NamedBackgroundSize str -> stringValueFactory.value str
        AutoBackgroundSize -> autoValue
        InitialBackgroundSize -> initialValue
        InheritBackgroundSize -> inheritValue
        OtherBackgroundSize str -> otherValue str
  }  

-------------------------------------------------------------------------------

type alias BackgroundRepeatDescriptor = 
  BackgroundRepeatFactory -> BackgroundRepeat

type BackgroundRepeat 
  = BackgroundRepeat String
  | InitialBackgroundRepeat
  | InheritBackgroundRepeat
  | OtherBackgroundRepeat String

type alias BackgroundRepeatFactory =
  { repeat : String -> BackgroundRepeat 
  , initial : BackgroundRepeat 
  , inherit : BackgroundRepeat 
  , other : String -> BackgroundRepeat 
  }  

backgroundRepeatFactory : BackgroundRepeatFactory
backgroundRepeatFactory =
  { repeat str = BackgroundRepeat str 
  , initial = InitialBackgroundRepeat 
  , inherit = InheritBackgroundRepeat 
  , other str = OtherBackgroundRepeat str 
  }  

backgroundRepeatValueFactory : ValueFactory BackgroundRepeat
backgroundRepeatValueFactory = 
  { value repeat =
      case repeat of
        BackgroundRepeat str -> stringValueFactory.value str
        InitialBackgroundRepeat -> initialValue
        InheritBackgroundRepeat -> inheritValue
        OtherBackgroundRepeat str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias BackgroundImageDescriptor = 
  BackgroundImageFactory -> BackgroundImage

type BackgroundImage 
  = BackgroundImage String
  | NoBackgroundImage
  | InitialBackgroundImage
  | InheritBackgroundImage
  | OtherBackgroundImage String

type alias BackgroundImageFactory =
  { url : String -> BackgroundImage 
  , none : BackgroundImage 
  , initial : BackgroundImage 
  , inherit : BackgroundImage 
  , other : String -> BackgroundImage 
  }  

backgroundImageFactory : BackgroundImageFactory
backgroundImageFactory =
  { url str = BackgroundImage str 
  , none = NoBackgroundImage
  , initial = InitialBackgroundImage 
  , inherit = InheritBackgroundImage 
  , other str = OtherBackgroundImage str 
  }  

backgroundImageValueFactory : ValueFactory BackgroundImage
backgroundImageValueFactory = 
  { value bgImage =
      case bgImage of
        BackgroundImage imgUrl -> 
          stringValueFactory.value (String.concat ["url(\"", imgUrl ,"\")"])
        NoBackgroundImage -> noneValue
        InitialBackgroundImage -> initialValue
        InheritBackgroundImage -> inheritValue
        OtherBackgroundImage str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias BackgroundOriginDescriptor = 
  BackgroundOriginFactory -> BackgroundOrigin

type BackgroundOrigin 
  = BackgroundOrigin BoxType
  | InitialBackgroundOrigin
  | InheritBackgroundOrigin
  | OtherBackgroundOrigin String

type alias BackgroundOriginFactory =
  { origin : BoxType -> BackgroundOrigin 
  , initial : BackgroundOrigin 
  , inherit : BackgroundOrigin 
  , other : String -> BackgroundOrigin 
  }  

backgroundOriginFactory : BackgroundOriginFactory
backgroundOriginFactory =
  { origin boxType = BackgroundOrigin boxType 
  , initial = InitialBackgroundOrigin 
  , inherit = InheritBackgroundOrigin 
  , other str = OtherBackgroundOrigin str 
  }  

backgroundOriginValueFactory : ValueFactory BackgroundOrigin
backgroundOriginValueFactory = 
  { value bgOrigin =
      case bgOrigin of
        BackgroundOrigin boxType -> boxTypeValueFactory.value boxType
        InitialBackgroundOrigin -> initialValue
        InheritBackgroundOrigin -> inheritValue
        OtherBackgroundOrigin str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias BackgroundClipDescriptor = 
  BackgroundClipFactory -> BackgroundClip

type BackgroundClip 
  = BackgroundClip BoxType
  | InitialBackgroundClip
  | InheritBackgroundClip
  | OtherBackgroundClip String

type alias BackgroundClipFactory =
  { clip : BoxType -> BackgroundClip 
  , initial : BackgroundClip 
  , inherit : BackgroundClip 
  , other : String -> BackgroundClip 
  }  

backgroundClipFactory : BackgroundClipFactory
backgroundClipFactory =
  { clip boxType = BackgroundClip boxType 
  , initial = InitialBackgroundClip 
  , inherit = InheritBackgroundClip 
  , other str = OtherBackgroundClip str 
  }  

backgroundClipValueFactory : ValueFactory BackgroundClip
backgroundClipValueFactory = 
  { value bgClip =
      case bgClip of
        BackgroundClip boxType -> boxTypeValueFactory.value boxType
        InitialBackgroundClip -> initialValue
        InheritBackgroundClip -> inheritValue
        OtherBackgroundClip str -> otherValue str
  }

-------------------------------------------------------------------------------

type alias BackgroundAttachmentDescriptor = 
  BackgroundAttachmentFactory -> BackgroundAttachment

type BackgroundAttachment 
  = BackgroundAttachment String
  | InitialBackgroundAttachment
  | InheritBackgroundAttachment
  | OtherBackgroundAttachment String

type alias BackgroundAttachmentFactory =
  { bgAttachment : String -> BackgroundAttachment 
  , initial : BackgroundAttachment 
  , inherit : BackgroundAttachment 
  , other : String -> BackgroundAttachment 
  }  

backgroundAttachmentFactory : BackgroundAttachmentFactory
backgroundAttachmentFactory =
  { bgAttachment str = BackgroundAttachment str 
  , initial = InitialBackgroundAttachment 
  , inherit = InheritBackgroundAttachment 
  , other str = OtherBackgroundAttachment str 
  }  

backgroundAttachmentValueFactory : ValueFactory BackgroundAttachment
backgroundAttachmentValueFactory = 
  { value bgAttachment =
      case bgAttachment of
        BackgroundAttachment str -> stringValueFactory.value str
        InitialBackgroundAttachment -> initialValue
        InheritBackgroundAttachment -> inheritValue
        OtherBackgroundAttachment str -> otherValue str
  }
