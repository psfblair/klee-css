module Css.Internal.Background
  ( BackgroundColorDescriptor, backgroundColorFactory
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
  , Background, BackgroundDescriptor, ComposedBackgroundDescriptor
  , BackgroundComponents (..)
  , initialBackgroundFactory, adjoinComponents, backgroundValue
  ) where

import String

import Css.Internal.Box exposing (BoxType, boxTypeValue)  
import Css.Internal.Color exposing 
  (CssColor (..), ColorFactory, colorFactory, colorValue)
import Css.Internal.Common exposing 
  (autoValue, initialValue, inheritValue, noneValue, otherValue)
import Css.Internal.Position exposing 
  ( HorizontalSide, VerticalSide
  , sideLeft, sideCenter, sideRight
  , sideTop, sideMiddle, sideBottom
  , horizontalSideValue, verticalSideValue
  )
import Css.Internal.Property exposing 
  (Value, intersperse, stringValue, spacePairValue, maybeValue, spaceListValue)
import Css.Internal.Size exposing (Size, SizeDescriptor, sizeFactory, sizeValue)

-------------------------------------------------------------------------------

type alias BackgroundPositionDescriptor sz1 sz2 = 
    BackgroundPositionFactory sz1 sz2 -> BackgroundPosition sz1 sz2

type BackgroundPosition sz1 sz2
  = SizedBackgroundPosition (Size sz1, Size sz2)
  | SidedBackgroundPosition (HorizontalSide, VerticalSide)
  | InitialBackgroundPosition
  | InheritBackgroundPosition
  | OtherBackgroundPosition Value

type alias BackgroundPositionFactory sz1 sz2 =
  { sizedPosition : SizeDescriptor (Size sz1) sz1 -> 
                    SizeDescriptor (Size sz2) sz2 -> 
                    BackgroundPosition sz1 sz2
  , sidedPosition : HorizontalSide -> VerticalSide -> BackgroundPosition sz1 sz2
  , initial_ : BackgroundPosition sz1 sz2
  , inherit_ : BackgroundPosition sz1 sz2
  , other_ : Value -> BackgroundPosition sz1 sz2
  }

backgroundPositionFactory : BackgroundPositionFactory sz1 sz2
backgroundPositionFactory = 
  { sizedPosition horiz vert = 
      SizedBackgroundPosition (horiz sizeFactory, vert sizeFactory)
  , sidedPosition horiz vert = SidedBackgroundPosition (horiz, vert)
  , initial_ = InitialBackgroundPosition
  , inherit_ = InheritBackgroundPosition
  , other_ val = OtherBackgroundPosition val
  }

backgroundPositionValue : BackgroundPosition sz1 sz2 -> Value 
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
  { backgroundSize : SizeDescriptor (Size sz) sz -> 
                     SizeDescriptor (Size sz) sz -> 
                     BackgroundSize sz
  , partial : SizeDescriptor (Size sz) sz -> BackgroundSize sz
  , named : String -> BackgroundSize sz
  , auto_ : BackgroundSize sz
  , initial_ : BackgroundSize sz
  , inherit_ : BackgroundSize sz
  , other_ : Value -> BackgroundSize sz
  }

backgroundSizeFactory : BackgroundSizeFactory sz
backgroundSizeFactory =
  { backgroundSize widthDescriptor heightDescriptor = 
      let width = widthDescriptor sizeFactory 
          height = heightDescriptor sizeFactory 
      in BackgroundSize width height
  , partial widthDescriptor = 
      let width = widthDescriptor sizeFactory 
      in PartiallySpecifiedBackgroundSize width
  , named str = NamedBackgroundSize str
  , auto_ = AutoBackgroundSize
  , initial_ = InitialBackgroundSize
  , inherit_ = InheritBackgroundSize
  , other_ val = OtherBackgroundSize val
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
  , none_ : BackgroundImage 
  , initial_ : BackgroundImage 
  , inherit_ : BackgroundImage 
  , other_ : Value -> BackgroundImage 
  }  

backgroundImageFactory : BackgroundImageFactory
backgroundImageFactory =
  { url str = BackgroundImage str 
  , none_ = NoBackgroundImage
  , initial_ = InitialBackgroundImage 
  , inherit_ = InheritBackgroundImage 
  , other_ val = OtherBackgroundImage val 
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
  , initial_ : BackgroundRepeat 
  , inherit_ : BackgroundRepeat 
  , other_ : Value -> BackgroundRepeat 
  }  

backgroundRepeatFactory : BackgroundRepeatFactory
backgroundRepeatFactory =
  { repeat str = BackgroundRepeat str 
  , initial_ = InitialBackgroundRepeat 
  , inherit_ = InheritBackgroundRepeat 
  , other_ val = OtherBackgroundRepeat val 
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
  , initial_ : BackgroundOrigin 
  , inherit_ : BackgroundOrigin 
  , other_ : Value -> BackgroundOrigin 
  }  

backgroundOriginFactory : BackgroundOriginFactory
backgroundOriginFactory =
  { origin boxType = BackgroundOrigin boxType 
  , initial_ = InitialBackgroundOrigin 
  , inherit_ = InheritBackgroundOrigin 
  , other_ val = OtherBackgroundOrigin val 
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
  , initial_ : BackgroundClip 
  , inherit_ : BackgroundClip 
  , other_ : Value -> BackgroundClip 
  }  

backgroundClipFactory : BackgroundClipFactory
backgroundClipFactory =
  { clip boxType = BackgroundClip boxType 
  , initial_ = InitialBackgroundClip 
  , inherit_ = InheritBackgroundClip 
  , other_ val = OtherBackgroundClip val 
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
  , initial_ : BackgroundAttachment 
  , inherit_ : BackgroundAttachment 
  , other_ : Value -> BackgroundAttachment 
  }  

backgroundAttachmentFactory : BackgroundAttachmentFactory
backgroundAttachmentFactory =
  { bgAttachment str = BackgroundAttachment str 
  , initial_ = InitialBackgroundAttachment 
  , inherit_ = InheritBackgroundAttachment 
  , other_ val = OtherBackgroundAttachment val 
  }  

backgroundAttachmentValue : BackgroundAttachment -> Value 
backgroundAttachmentValue bgAttachment = 
  case bgAttachment of
    BackgroundAttachment str -> stringValue str
    InitialBackgroundAttachment -> initialValue
    InheritBackgroundAttachment -> inheritValue
    OtherBackgroundAttachment val -> otherValue val

-------------------------------------------------------------------------------

type alias Background a sz1 sz2 sz3 = 
  { a | background : BackgroundAlternative sz1 sz2 sz3 }

type BackgroundAlternative sz1 sz2 sz3
  = CompositeBackground (BackgroundComponents sz1 sz2 sz3)
  | InitialBackground
  | InheritBackground
  | OtherBackground Value

type BackgroundComponents sz1 sz2 sz3
  = NoComponents
  | WithPositionAndSize (BackgroundPosition sz1 sz2) (Maybe (BackgroundSize sz3)) (BackgroundComponents sz1 sz2 sz3)
  | WithColor CssColor (BackgroundComponents sz1 sz2 sz3)
  | WithImage BackgroundImage (BackgroundComponents sz1 sz2 sz3)
  | WithRepeat BackgroundRepeat (BackgroundComponents sz1 sz2 sz3)
  | WithOrigin BackgroundOrigin (BackgroundComponents sz1 sz2 sz3)
  | WithClip BackgroundClip (BackgroundComponents sz1 sz2 sz3)
  | WithAttachment BackgroundAttachment (BackgroundComponents sz1 sz2 sz3)

{- In order to compose descriptors and still include `inherit` and `initial`, we
do as follows:

The `inherit` and `initial` functions take a factory and produce some type `a`.
But all the descriptors have to be of the same type in order to be passed to the
`background` function. This means they all must take a factory and they must all
return the same type.

Furthermore, to compose, the combinator descriptors (e.g., withClip, withRepeat)
have to yield the same type that they take as a parameter. Therefore a descriptor 
has to be a function that takes a factory and produces a factory. 

This means that as it is passed through the composed combinators, the factory 
has to carry along with itself the accumulated background that the `background` 
function will ultimately extract.

Since we don't want `inherit` and `initial` to be composable with the other
combinators, they have to yield a simple `Background` that can't be passed 
through a chain of combinators. So the `BackgroundDescriptor` type is a function
that takes a factory that also contains an accumulated background, and that
produces a record that contains at least a background.

The type signatures of the combinators are more restrictive -- they accept a
factory with an accumulated background, but they also produce a factory with an
accumulated background. This means that `inherit` and `initial` can't be 
composed with the other combinators. (There is one hole: `inherit` and `initial`
can stand at the end of a chain of combinators. There doesn't really seem to be
any obvious way around this.)
-}
type alias BackgroundDescriptor b sz1 sz2 sz3 = 
  BackgroundFactory {} sz1 sz2 sz3 -> Background b sz1 sz2 sz3
  
type alias ComposedBackgroundDescriptor a sz1 sz2 sz3 = 
  { a | background : BackgroundAlternative sz1 sz2 sz3,
        backgroundComponents : BackgroundComponents sz1 sz2 sz3} ->
  BackgroundFactory {} sz1 sz2 sz3

type alias BackgroundFactory b sz1 sz2 sz3 = 
  Background (GenericBackgroundFactory (WithComponents sz1 sz2 sz3) b sz1 sz2 sz3) sz1 sz2 sz3

type alias GenericBackgroundFactory a b sz1 sz2 sz3 =   
  { a | initial_ : Background b sz1 sz2 sz3 
      , inherit_ : Background b sz1 sz2 sz3 
      , other_ : Value -> Background b sz1 sz2 sz3
  }

type alias WithComponents sz1 sz2 sz3 = 
  { backgroundComponents : BackgroundComponents sz1 sz2 sz3
  }
                 
initialBackgroundFactory : BackgroundFactory {} sz1 sz2 sz3
initialBackgroundFactory =
  { background = CompositeBackground NoComponents
  , backgroundComponents = NoComponents
  , initial_ = { background = InitialBackground }
  , inherit_ = { background = InheritBackground }
  , other_ val  = { background = OtherBackground val }
  }

adjoinComponents : BackgroundComponents sz1 sz2 sz3 ->
                   BackgroundFactory {} sz1 sz2 sz3
adjoinComponents newComponents = 
  { initialBackgroundFactory | background <- CompositeBackground newComponents 
                             , backgroundComponents <- newComponents }

backgroundValue : BackgroundAlternative sz1 sz2 sz3 -> Value
backgroundValue backgroundAlternative =
  case backgroundAlternative of
    InitialBackground -> initialValue
    InheritBackground -> inheritValue
    OtherBackground val -> otherValue val
    CompositeBackground backgroundComponents -> componentsToValue backgroundComponents

componentsToValue : BackgroundComponents sz1 sz2 sz3 -> Value
componentsToValue bgComponents = 
  let nll = Nothing
  in componentsToValueRecursive bgComponents nll nll nll nll nll nll nll nll

{- Recommended order:
    image, position / size, repeat, attachment, origin, clip, color
At the least, origin must come before clip.
-}
componentsToValueRecursive : BackgroundComponents sz1 sz2 sz3 -> 
                             Maybe BackgroundImage ->
                             Maybe (BackgroundPosition sz1 sz2) -> 
                             Maybe (BackgroundSize sz3) -> 
                             Maybe BackgroundRepeat ->
                             Maybe BackgroundAttachment ->
                             Maybe BackgroundOrigin ->
                             Maybe BackgroundClip ->
                             Maybe CssColor ->
                             Value
componentsToValueRecursive 
    components mImg mPos mSiz mRepeat mAttach mOrig mClip mColor =
  case components of
      -- If the FontComponents combinators are called more than once,
      -- the last (outer) one wins. So if it's already set we don't reset it.
      WithImage image inner ->
        case mImg of
          Just _ -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip mColor
          Nothing -> 
            componentsToValueRecursive inner (Just image) mPos mSiz mRepeat mAttach mOrig mClip mColor
      WithPositionAndSize position maybeSize inner -> 
        case mPos of
          Just _ -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip mColor
          Nothing -> 
            componentsToValueRecursive inner mImg (Just position) maybeSize mRepeat mAttach mOrig mClip mColor
      WithRepeat repeat inner ->
        case mRepeat of
          Just _ -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip mColor
          Nothing -> 
            componentsToValueRecursive inner mImg mPos mSiz (Just repeat) mAttach mOrig mClip mColor
      WithAttachment attachment inner -> 
        case mAttach of
          Just _ -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip mColor
          Nothing -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat (Just attachment) mOrig mClip mColor
      WithOrigin origin inner -> 
        case mOrig of
          Just _ -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip mColor
          Nothing -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach (Just origin) mClip mColor
      WithClip clip inner -> 
        case mClip of
          Just _ -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip mColor
          Nothing -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig (Just clip) mColor
      WithColor color inner ->
        case mColor of
          Just _ -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip mColor
          Nothing -> 
            componentsToValueRecursive inner mImg mPos mSiz mRepeat mAttach mOrig mClip (Just color)
      NoComponents -> 
        let maybePosition = Maybe.map backgroundPositionValue mPos
            maybeSize = Maybe.map backgroundSizeValue mSiz
            maybePositionAndSize = 
              case (maybePosition, maybeSize) of
                (Just (pos), Just(siz)) -> intersperse "/" [pos, siz] |> Just
                (Just (pos), _) -> Just (pos)
                _ -> Nothing -- Size without position can't happen and is invalid.

            maybeImage = Maybe.map backgroundImageValue mImg
            maybeRepeat = Maybe.map backgroundRepeatValue mRepeat
            maybeAttachment = Maybe.map backgroundAttachmentValue mAttach
            maybeOrigin = Maybe.map backgroundOriginValue mOrig
            maybeClip = Maybe.map backgroundClipValue mClip
            maybeColor = Maybe.map colorValue mColor

            allValues = 
              [ maybeImage
              , maybePositionAndSize
              , maybeRepeat
              , maybeAttachment
              , maybeOrigin
              , maybeClip
              , maybeColor
              ] |> List.filterMap identity
              
        in spaceListValue identity allValues
