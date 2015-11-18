module Css.Internal.Background
  ( BackgroundPositionDescriptor, backgroundPositionFactory
  , BackgroundSizeDescriptor, backgroundSizeFactory
  , BackgroundRepeatDescriptor, backgroundRepeatFactory
  , BackgroundImageDescriptor, backgroundImageFactory
  , BackgroundAttachmentDescriptor, backgroundAttachmentFactory
  , Background, BackgroundDescriptor, ComposedBackgroundDescriptor
  , withPositionAndSize, withColor, withImage, withRepeat
  , withOrigin, withClip, withAttachment
  , initialBackgroundFactory, adjoinComponents, backgroundValue
  ) where

import String

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides

-------------------------------------------------------------------------------

type alias BackgroundPositionDescriptor sz1 sz2 = 
    BackgroundPositionFactory sz1 sz2 -> Property.Value

type alias BackgroundPositionFactory sz1 sz2 =
  { sizedPosition : Linear.SizeDescriptor {} sz1 -> 
                    Linear.SizeDescriptor {} sz2 -> 
                    Property.Value
  , sidedPosition : Sides.HorizontalSide -> Sides.VerticalSide -> Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

backgroundPositionFactory : BackgroundPositionFactory sz1 sz2
backgroundPositionFactory = 
  { sizedPosition horizontalDescriptor verticalDescriptor = 
      let compositeDescriptor = 
        Property.spacePairValue horizontalDescriptor verticalDescriptor
      in compositeDescriptor (Linear.nubSizeFactory, Linear.nubSizeFactory)
  , sidedPosition horizontal vertical = 
      let valueFactory = 
        Property.spacePairValue Sides.horizontalSideValue Sides.verticalSideValue
      in valueFactory (horizontal, vertical)
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias BackgroundSizeDescriptor sz = BackgroundSizeFactory sz -> Property.Value

type alias BackgroundSizeFactory szTyp =
  { backgroundSize : Linear.SizeDescriptor {} szTyp -> 
                     Linear.SizeDescriptor {} szTyp -> 
                     Property.Value
  , partial : Linear.SizeDescriptor {} szTyp -> Property.Value
  , named : String -> Property.Value
  , auto_ : Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

backgroundSizeFactory : BackgroundSizeFactory sz
backgroundSizeFactory =
  { backgroundSize widthDescriptor heightDescriptor = 
      let compositeDescriptor = 
        Property.spacePairValue widthDescriptor heightDescriptor
      in compositeDescriptor (Linear.nubSizeFactory, Linear.nubSizeFactory)
  , partial widthDescriptor = 
      let compositeDescriptor = 
        Property.spacePairValue widthDescriptor identity
      in compositeDescriptor (Linear.nubSizeFactory, Common.autoValue)
  , named str = Property.stringValue str
  , auto_ = Common.autoValue
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias BackgroundImageDescriptor = BackgroundImageFactory -> Property.Value

type alias BackgroundImageFactory =
  { url : String -> Property.Value 
  , none_ : Property.Value 
  , initial_ : Property.Value 
  , inherit_ : Property.Value 
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value 
  }  

backgroundImageFactory : BackgroundImageFactory
backgroundImageFactory =
  { url imgUrl = Property.stringValue (String.concat ["url(\"", imgUrl ,"\")"])
  , none_ = Common.noneValue
  , initial_ = Common.initialValue 
  , inherit_ = Common.inheritValue 
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }  

-------------------------------------------------------------------------------

type alias BackgroundRepeatDescriptor = BackgroundRepeatFactory -> Property.Value

type alias BackgroundRepeatFactory =
  { repeat : String -> Property.Value 
  , initial_ : Property.Value 
  , inherit_ : Property.Value 
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value 
  }  

backgroundRepeatFactory : BackgroundRepeatFactory
backgroundRepeatFactory =
  { repeat str = Property.stringValue str
  , initial_ = Common.initialValue 
  , inherit_ = Common.inheritValue 
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }  

-------------------------------------------------------------------------------

type alias BackgroundAttachmentDescriptor = 
  BackgroundAttachmentFactory -> Property.Value

type alias BackgroundAttachmentFactory =
  { bgAttachment : String -> Property.Value 
  , initial_ : Property.Value 
  , inherit_ : Property.Value 
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value 
  }  

backgroundAttachmentFactory : BackgroundAttachmentFactory
backgroundAttachmentFactory =
  { bgAttachment str = Property.stringValue str
  , initial_ = Common.initialValue 
  , inherit_ = Common.inheritValue 
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val 
  }  

-------------------------------------------------------------------------------

type alias Background a sz1 sz2 sz3 = 
  { a | background : BackgroundAlternative sz1 sz2 sz3 }

type BackgroundAlternative sz1 sz2 sz3
  = CompositeBackground (BackgroundComponents sz1 sz2 sz3)
  | InitialBackground
  | InheritBackground
  | UnsetBackground
  | OtherBackground Property.Value

type BackgroundComponents sz1 sz2 sz3
  = NoComponents
  | WithPositionAndSize Property.Value (Maybe Property.Value) (BackgroundComponents sz1 sz2 sz3)
  | WithColor Property.Value (BackgroundComponents sz1 sz2 sz3)
  | WithImage Property.Value (BackgroundComponents sz1 sz2 sz3)
  | WithRepeat Property.Value (BackgroundComponents sz1 sz2 sz3)
  | WithOrigin Property.Value (BackgroundComponents sz1 sz2 sz3)
  | WithClip Property.Value (BackgroundComponents sz1 sz2 sz3)
  | WithAttachment Property.Value (BackgroundComponents sz1 sz2 sz3)

withPositionAndSize : Property.Value -> 
                      Maybe Property.Value -> 
                      BackgroundComponents sz1 sz2 sz3 ->
                      BackgroundComponents sz1 sz2 sz3
withPositionAndSize position maybeSize innerComponents = 
  WithPositionAndSize position maybeSize innerComponents
  
withColor : Property.Value -> 
            BackgroundComponents sz1 sz2 sz3 ->
            BackgroundComponents sz1 sz2 sz3
withColor colorValue innerComponents = WithColor colorValue innerComponents

withImage : Property.Value -> 
            BackgroundComponents sz1 sz2 sz3 ->
            BackgroundComponents sz1 sz2 sz3
withImage image innerComponents = WithImage image innerComponents

withRepeat : Property.Value -> 
             BackgroundComponents sz1 sz2 sz3 ->
             BackgroundComponents sz1 sz2 sz3
withRepeat repeat innerComponents = WithRepeat repeat innerComponents

withOrigin : Property.Value -> 
             BackgroundComponents sz1 sz2 sz3 ->
             BackgroundComponents sz1 sz2 sz3
withOrigin origin innerComponents = WithOrigin origin innerComponents

withClip : Property.Value -> 
           BackgroundComponents sz1 sz2 sz3 ->
           BackgroundComponents sz1 sz2 sz3
withClip clip innerComponents = WithClip clip innerComponents

withAttachment : Property.Value -> 
                 BackgroundComponents sz1 sz2 sz3 ->
                 BackgroundComponents sz1 sz2 sz3
withAttachment attachment innerComponents = 
  WithAttachment attachment innerComponents

{- In order to compose descriptors and still include `inherit` and `initial`, we
do as follows:

The `inherit` and `initial` generic functions take a factory and produce some 
type `a`. But all the descriptors have to be of the same type in order to be 
passed to the `background` function. This means they all must take a factory and 
they must all return the same type.

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
      , unset_   : Background b sz1 sz2 sz3 
      , other_   : Property.Value -> Background b sz1 sz2 sz3
  }

type alias WithComponents sz1 sz2 sz3 = 
  { backgroundComponents : BackgroundComponents sz1 sz2 sz3
  }
                 
initialBackgroundFactory : BackgroundFactory {} sz1 sz2 sz3
initialBackgroundFactory =
  { background = CompositeBackground NoComponents
  , backgroundComponents = NoComponents
  , initial_    = { background = InitialBackground   }
  , inherit_    = { background = InheritBackground   }
  , unset_      = { background = UnsetBackground     }   
  , other_ val  = { background = OtherBackground val }
  }

adjoinComponents : BackgroundComponents sz1 sz2 sz3 ->
                   BackgroundFactory {} sz1 sz2 sz3
adjoinComponents newComponents = 
  { initialBackgroundFactory | background <- CompositeBackground newComponents 
                             , backgroundComponents <- newComponents }

backgroundValue : BackgroundAlternative sz1 sz2 sz3 -> Property.Value
backgroundValue backgroundAlternative =
  case backgroundAlternative of
    InitialBackground   -> Common.initialValue
    InheritBackground   -> Common.inheritValue
    UnsetBackground     -> Common.unsetValue
    OtherBackground val -> Common.otherValue val
    CompositeBackground backgroundComponents -> componentsToValue backgroundComponents

componentsToValue : BackgroundComponents sz1 sz2 sz3 -> Property.Value
componentsToValue bgComponents = 
  let nll = Nothing
  in componentsToValueRecursive bgComponents nll nll nll nll nll nll nll nll

{- Recommended order:
    image, position / size, repeat, attachment, origin, clip, color
At the least, origin must come before clip.
-}
componentsToValueRecursive : BackgroundComponents sz1 sz2 sz3 -> 
                             Maybe Property.Value ->   -- image
                             Maybe Property.Value ->   -- position
                             Maybe Property.Value ->   -- size
                             Maybe Property.Value ->   -- repeat
                             Maybe Property.Value ->   -- attachment
                             Maybe Property.Value ->   -- origin
                             Maybe Property.Value ->   -- clip
                             Maybe Property.Value ->   -- color
                             Property.Value
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
        let maybePositionAndSize = 
              case (mPos, mSiz) of
                (Just (pos), Just(siz)) -> Property.intersperse "/" [pos, siz] |> Just
                (Just (pos), _) -> Just (pos)
                _ -> Nothing -- Size without position can't happen and is invalid.

            allValues = 
              [ mImg
              , maybePositionAndSize
              , mRepeat
              , mAttach
              , mOrig
              , mClip
              , mColor
              ] |> List.filterMap identity
              
        in Property.spaceListValue identity allValues
