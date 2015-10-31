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
  , BackgroundDescriptor, ComposedBackgroundDescriptor, BackgroundComponents (..) 
  , emptyBackground, backgroundFactory, backgroundValue
  ) where

import String

import Css.Internal.Box exposing (BoxType, boxTypeValue)  
import Css.Internal.Color exposing 
  (CssColor (..), ColorFactory, colorFactory, colorValue)
import Css.Internal.Common exposing 
  (autoValue, initialValue, inheritValue, noneValue, otherValue)
import Css.Internal.Property exposing 
  (Value, intersperse, stringValue, spacePairValue, maybeValue, spaceListValue)
import Css.Internal.Size exposing (Size, sizeValue)

-------------------------------------------------------------------------------

type HorizontalSide 
  = HorizontalSide String
  | InitialHorizontalSide
  | InheritHorizontalSide
  | OtherHorizontalSide Value

horizontalSideValue : HorizontalSide -> Value 
horizontalSideValue side = 
  case side of
    HorizontalSide str -> stringValue str
    InitialHorizontalSide -> initialValue
    InheritHorizontalSide -> inheritValue
    OtherHorizontalSide val -> otherValue val

type VerticalSide
  = VerticalSide String
  | InitialVerticalSide
  | InheritVerticalSide
  | OtherVerticalSide Value

verticalSideValue : VerticalSide -> Value 
verticalSideValue side = 
  case side of
    VerticalSide str -> stringValue str
    InitialVerticalSide -> initialValue
    InheritVerticalSide -> inheritValue
    OtherVerticalSide val -> otherValue val

-------------------------------------------------------------------------------

type alias BackgroundPositionDescriptor sz = 
    BackgroundPositionFactory sz -> BackgroundPosition sz

type BackgroundPosition sz
  = SizedBackgroundPosition (Size sz, Size sz)
  | SidedBackgroundPosition (HorizontalSide, VerticalSide)
  | InitialBackgroundPosition
  | InheritBackgroundPosition
  | OtherBackgroundPosition Value

type alias BackgroundPositionFactory sz =
  { sizedPosition : Size sz -> Size sz -> BackgroundPosition sz
  , sidedPosition : HorizontalSide -> VerticalSide -> BackgroundPosition sz
  , initial: BackgroundPosition sz
  , inherit: BackgroundPosition sz
  , other: Value -> BackgroundPosition sz
  }

backgroundPositionFactory : BackgroundPositionFactory sz
backgroundPositionFactory = 
  { sizedPosition horiz vert = SizedBackgroundPosition (horiz, vert)
  , sidedPosition horiz vert = SidedBackgroundPosition (horiz, vert)
  , initial = InitialBackgroundPosition
  , inherit = InheritBackgroundPosition
  , other val = OtherBackgroundPosition val
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
    OtherBackgroundPosition val -> val

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
  | OtherBackgroundSize Value

type alias BackgroundSizeFactory sz =
  { backgroundSize : (Size sz) -> (Size sz) -> BackgroundSize sz
  , partial : (Size sz) -> BackgroundSize sz
  , named : String -> BackgroundSize sz
  , auto : BackgroundSize sz
  , initial: BackgroundSize sz
  , inherit: BackgroundSize sz
  , other: Value -> BackgroundSize sz
  }

backgroundSizeFactory : BackgroundSizeFactory sz
backgroundSizeFactory =
  { backgroundSize width height = BackgroundSize width height
  , partial width = PartiallySpecifiedBackgroundSize width
  , named str = NamedBackgroundSize str
  , auto = AutoBackgroundSize
  , initial = InitialBackgroundSize
  , inherit = InheritBackgroundSize
  , other val = OtherBackgroundSize val
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
    OtherBackgroundSize val -> otherValue val

-------------------------------------------------------------------------------
type alias BackgroundColorDescriptor = BackgroundColorFactory -> CssColor

type alias TransparentColorFactory = { transparent: CssColor }

type alias BackgroundColorFactory = ColorFactory TransparentColorFactory

backgroundColorFactory : BackgroundColorFactory
backgroundColorFactory = 
  { colorFactory | transparent = OtherColor (stringValue "transparent") }

-------------------------------------------------------------------------------

type alias BackgroundImageDescriptor = 
  BackgroundImageFactory -> BackgroundImage

type BackgroundImage 
  = BackgroundImage String
  | NoBackgroundImage
  | InitialBackgroundImage
  | InheritBackgroundImage
  | OtherBackgroundImage Value

type alias BackgroundImageFactory =
  { url : String -> BackgroundImage 
  , none : BackgroundImage 
  , initial : BackgroundImage 
  , inherit : BackgroundImage 
  , other : Value -> BackgroundImage 
  }  

backgroundImageFactory : BackgroundImageFactory
backgroundImageFactory =
  { url str = BackgroundImage str 
  , none = NoBackgroundImage
  , initial = InitialBackgroundImage 
  , inherit = InheritBackgroundImage 
  , other val = OtherBackgroundImage val 
  }  

backgroundImageValue : BackgroundImage -> Value 
backgroundImageValue bgImage = 
  case bgImage of
    BackgroundImage imgUrl -> 
      stringValue (String.concat ["url(\"", imgUrl ,"\")"])
    NoBackgroundImage -> noneValue
    InitialBackgroundImage -> initialValue
    InheritBackgroundImage -> inheritValue
    OtherBackgroundImage val -> otherValue val

-------------------------------------------------------------------------------

type alias BackgroundRepeatDescriptor = 
  BackgroundRepeatFactory -> BackgroundRepeat

type BackgroundRepeat 
  = BackgroundRepeat String
  | InitialBackgroundRepeat
  | InheritBackgroundRepeat
  | OtherBackgroundRepeat Value

type alias BackgroundRepeatFactory =
  { repeat : String -> BackgroundRepeat 
  , initial : BackgroundRepeat 
  , inherit : BackgroundRepeat 
  , other : Value -> BackgroundRepeat 
  }  

backgroundRepeatFactory : BackgroundRepeatFactory
backgroundRepeatFactory =
  { repeat str = BackgroundRepeat str 
  , initial = InitialBackgroundRepeat 
  , inherit = InheritBackgroundRepeat 
  , other val = OtherBackgroundRepeat val 
  }  

backgroundRepeatValue : BackgroundRepeat -> Value 
backgroundRepeatValue repeat = 
  case repeat of
    BackgroundRepeat str -> stringValue str
    InitialBackgroundRepeat -> initialValue
    InheritBackgroundRepeat -> inheritValue
    OtherBackgroundRepeat val -> otherValue val

-------------------------------------------------------------------------------

type alias BackgroundOriginDescriptor = 
  BackgroundOriginFactory -> BackgroundOrigin

type BackgroundOrigin 
  = BackgroundOrigin BoxType
  | InitialBackgroundOrigin
  | InheritBackgroundOrigin
  | OtherBackgroundOrigin Value

type alias BackgroundOriginFactory =
  { origin : BoxType -> BackgroundOrigin 
  , initial : BackgroundOrigin 
  , inherit : BackgroundOrigin 
  , other : Value -> BackgroundOrigin 
  }  

backgroundOriginFactory : BackgroundOriginFactory
backgroundOriginFactory =
  { origin boxType = BackgroundOrigin boxType 
  , initial = InitialBackgroundOrigin 
  , inherit = InheritBackgroundOrigin 
  , other val = OtherBackgroundOrigin val 
  }  

backgroundOriginValue : BackgroundOrigin -> Value 
backgroundOriginValue bgOrigin = 
  case bgOrigin of
    BackgroundOrigin boxType -> boxTypeValue boxType
    InitialBackgroundOrigin -> initialValue
    InheritBackgroundOrigin -> inheritValue
    OtherBackgroundOrigin val -> otherValue val

-------------------------------------------------------------------------------

type alias BackgroundClipDescriptor = 
  BackgroundClipFactory -> BackgroundClip

type BackgroundClip 
  = BackgroundClip BoxType
  | InitialBackgroundClip
  | InheritBackgroundClip
  | OtherBackgroundClip Value

type alias BackgroundClipFactory =
  { clip : BoxType -> BackgroundClip 
  , initial : BackgroundClip 
  , inherit : BackgroundClip 
  , other : Value -> BackgroundClip 
  }  

backgroundClipFactory : BackgroundClipFactory
backgroundClipFactory =
  { clip boxType = BackgroundClip boxType 
  , initial = InitialBackgroundClip 
  , inherit = InheritBackgroundClip 
  , other val = OtherBackgroundClip val 
  }  

backgroundClipValue : BackgroundClip -> Value 
backgroundClipValue bgClip = 
  case bgClip of
    BackgroundClip boxType -> boxTypeValue boxType
    InitialBackgroundClip -> initialValue
    InheritBackgroundClip -> inheritValue
    OtherBackgroundClip val -> otherValue val

-------------------------------------------------------------------------------

type alias BackgroundAttachmentDescriptor = 
  BackgroundAttachmentFactory -> BackgroundAttachment

type BackgroundAttachment 
  = BackgroundAttachment String
  | InitialBackgroundAttachment
  | InheritBackgroundAttachment
  | OtherBackgroundAttachment Value

type alias BackgroundAttachmentFactory =
  { bgAttachment : String -> BackgroundAttachment 
  , initial : BackgroundAttachment 
  , inherit : BackgroundAttachment 
  , other : Value -> BackgroundAttachment 
  }  

backgroundAttachmentFactory : BackgroundAttachmentFactory
backgroundAttachmentFactory =
  { bgAttachment str = BackgroundAttachment str 
  , initial = InitialBackgroundAttachment 
  , inherit = InheritBackgroundAttachment 
  , other val = OtherBackgroundAttachment val 
  }  

backgroundAttachmentValue : BackgroundAttachment -> Value 
backgroundAttachmentValue bgAttachment = 
  case bgAttachment of
    BackgroundAttachment str -> stringValue str
    InitialBackgroundAttachment -> initialValue
    InheritBackgroundAttachment -> inheritValue
    OtherBackgroundAttachment val -> otherValue val

-------------------------------------------------------------------------------

type alias Background a sz1 sz2 = { a | background : BackgroundAlternative sz1 sz2 }
type alias WithBgComponents sz1 sz2 = { backgroundComponents : BackgroundComponents sz1 sz2 }
type alias ComposedBackground sz1 sz2 = Background (WithBgComponents sz1 sz2) sz1 sz2

type BackgroundAlternative sz1 sz2
  = CompositeBackground (BackgroundComponents sz1 sz2)
  | InitialBackground
  | InheritBackground

-- This produces functions of Background -> Background because all the parameters
-- of a CSS background shorthand property (and so of a ComposedBackground) are 
-- optional. The `background` function will feed in an empty background to get
-- the result out.
type alias BackgroundDescriptor a sz1 sz2 = 
  BackgroundFactory a sz1 sz2 -> (ComposedBackground sz1 sz2 -> Background a sz1 sz2)
    
type alias ComposedBackgroundDescriptor a sz1 sz2 = 
  BackgroundFactory a sz1 sz2 -> 
  (ComposedBackground sz1 sz2 -> ComposedBackground sz1 sz2)

type BackgroundComponents sz1 sz2
  = NoComponents
  | WithPositionAndSize (BackgroundPosition sz1) (Maybe (BackgroundSize sz2)) (ComposedBackground sz1 sz2)
  | WithColor CssColor (ComposedBackground sz1 sz2)
  | WithImage BackgroundImage (ComposedBackground sz1 sz2)
  | WithRepeat BackgroundRepeat (ComposedBackground sz1 sz2)
  | WithOrigin BackgroundOrigin (ComposedBackground sz1 sz2)
  | WithClip BackgroundClip (ComposedBackground sz1 sz2)
  | WithAttachment BackgroundAttachment (ComposedBackground sz1 sz2)

emptyBackground : ComposedBackground sz1 sz2
emptyBackground = 
  { background = CompositeBackground NoComponents, backgroundComponents = NoComponents } 

-- Need two type parameters because `initial_` and `inherit_` tranform a 
-- `Background` parameterized with `a` to one parameterized with `{}`.
type alias BackgroundFactory a sz1 sz2 =
  { composite : (ComposedBackground sz1 sz2 -> BackgroundComponents sz1 sz2) -> 
                (ComposedBackground sz1 sz2 -> ComposedBackground sz1 sz2) -> 
                (ComposedBackground sz1 sz2 -> ComposedBackground sz1 sz2)
  , initial_ : Background a sz1 sz2 -> Background {} sz1 sz2
  , inherit_ : Background a sz1 sz2 -> Background {} sz1 sz2
  }

{- `composer` is a partially-bound `BackgroundComponents`; i.e., without the  
   `ComposedBackground` parameter.
   `composite` has to return a function of `ComposedBackground` 
   to `ComposedBackground`.
-}
backgroundFactory : BackgroundFactory a sz1 sz2
backgroundFactory =
  { composite composer innerBackgroundTransformer =
      \inputBackground -> 
        let innerBg = innerBackgroundTransformer inputBackground
            newComponents = composer innerBg
        in { background = CompositeBackground newComponents, 
             backgroundComponents = newComponents } 
    , initial_   = \x -> { background = InitialBackground }
    , inherit_   = \x -> { background = InheritBackground }
  }
  
backgroundValue : Background a sz1 sz2 -> Value
backgroundValue background =
  case background.background of
    InitialBackground -> initialValue
    InheritBackground -> inheritValue
    CompositeBackground backgroundComponents -> componentsToValue backgroundComponents

componentsToValue : BackgroundComponents sz1 sz2 -> Value
componentsToValue bgComponents = 
  let nll = Nothing
  in componentsToValueRecursive bgComponents nll nll nll nll nll nll nll nll

componentsToValueRecursive : BackgroundComponents sz1 sz2 -> 
                             Maybe (BackgroundPosition sz1) -> 
                             Maybe (BackgroundSize sz2) -> 
                             Maybe CssColor ->
                             Maybe BackgroundImage ->
                             Maybe BackgroundRepeat ->
                             Maybe BackgroundOrigin ->
                             Maybe BackgroundClip ->
                             Maybe BackgroundAttachment ->
                             Value
componentsToValueRecursive 
    components mPos mSiz mColor mImg mRepeat mOrig mClip mAttach =
  case components of
      -- If the FontComponents combinators are called more than once,
      -- the last (outer) one wins. So if it's already set we don't reset it.
      WithPositionAndSize position maybeSize innerComposedBg -> 
        let inner = innerComposedBg.backgroundComponents
        in case mPos of
          Just _ -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip mAttach
          Nothing -> 
            componentsToValueRecursive inner (Just position) maybeSize mColor mImg mRepeat mOrig mClip mAttach
      WithColor color innerComposedBg ->
        let inner = innerComposedBg.backgroundComponents
        in case mColor of
          Just _ -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip mAttach
          Nothing -> 
            componentsToValueRecursive inner mPos mSiz (Just color) mImg mRepeat mOrig mClip mAttach
      WithImage image innerComposedBg ->
        let inner = innerComposedBg.backgroundComponents
        in case mImg of
          Just _ -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip mAttach
          Nothing -> 
            componentsToValueRecursive inner mPos mSiz mColor (Just image) mRepeat mOrig mClip mAttach
      WithRepeat repeat innerComposedBg ->
        let inner = innerComposedBg.backgroundComponents
        in case mRepeat of
          Just _ -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip mAttach
          Nothing -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg (Just repeat) mOrig mClip mAttach
      WithOrigin origin innerComposedBg -> 
        let inner = innerComposedBg.backgroundComponents
        in case mOrig of
          Just _ -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip mAttach
          Nothing -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat (Just origin) mClip mAttach
      WithClip clip innerComposedBg -> 
        let inner = innerComposedBg.backgroundComponents
        in case mClip of
          Just _ -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip mAttach
          Nothing -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig (Just clip) mAttach
      WithAttachment attachment innerComposedBg -> 
        let inner = innerComposedBg.backgroundComponents
        in case mAttach of
          Just _ -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip mAttach
          Nothing -> 
            componentsToValueRecursive inner mPos mSiz mColor mImg mRepeat mOrig mClip (Just attachment)
      NoComponents -> 
        let positionVal = maybeValue backgroundPositionValue mPos
            sizeVal = maybeValue backgroundSizeValue mSiz
            positionAndSizeVal = intersperse "/" [ positionVal, sizeVal ]
            
            colorVal = maybeValue colorValue mColor
            imageVal = maybeValue backgroundImageValue mImg
            repeatVal = maybeValue backgroundRepeatValue mRepeat
            originVal = maybeValue backgroundOriginValue mOrig
            clipVal = maybeValue backgroundClipValue mClip
            attachmentVal = maybeValue backgroundAttachmentValue mAttach
        in spaceListValue identity 
            [ positionAndSizeVal
            , colorVal
            , imageVal
            , repeatVal
            , originVal
            , clipVal
            , attachmentVal
            ] 
