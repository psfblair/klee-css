module Css.Internal.Background
  ( Background, BackgroundDescriptor, ComposedBackgroundDescriptor
  , withPositionAndSize, withColor, withImage, withRepeat
  , withOrigin, withClip, withAttachment
  , initialBackgroundFactory, adjoinComponents, backgroundValue
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias Background rec = 
  { rec | background : BackgroundAlternative }

type BackgroundAlternative
  = CompositeBackground BackgroundComponents
  | InitialBackground
  | InheritBackground
  | UnsetBackground
  | OtherBackground Property.Value

type BackgroundComponents
  = NoComponents
  | WithPositionAndSize Property.Value (Maybe Property.Value) BackgroundComponents
  | WithColor Property.Value BackgroundComponents
  | WithImage Property.Value BackgroundComponents
  | WithRepeat Property.Value BackgroundComponents
  | WithOrigin Property.Value BackgroundComponents
  | WithClip Property.Value BackgroundComponents
  | WithAttachment Property.Value BackgroundComponents

withPositionAndSize : Property.Value -> 
                      Maybe Property.Value -> 
                      BackgroundComponents ->
                      BackgroundComponents
withPositionAndSize position maybeSize innerComponents = 
  WithPositionAndSize position maybeSize innerComponents
  
withColor : Property.Value -> 
            BackgroundComponents ->
            BackgroundComponents
withColor colorValue innerComponents = WithColor colorValue innerComponents

withImage : Property.Value -> 
            BackgroundComponents ->
            BackgroundComponents
withImage image innerComponents = WithImage image innerComponents

withRepeat : Property.Value -> 
             BackgroundComponents ->
             BackgroundComponents
withRepeat repeat innerComponents = WithRepeat repeat innerComponents

withOrigin : Property.Value -> 
             BackgroundComponents ->
             BackgroundComponents
withOrigin origin innerComponents = WithOrigin origin innerComponents

withClip : Property.Value -> 
           BackgroundComponents ->
           BackgroundComponents
withClip clip innerComponents = WithClip clip innerComponents

withAttachment : Property.Value -> 
                 BackgroundComponents ->
                 BackgroundComponents
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
type alias BackgroundDescriptor rec = 
  BackgroundFactory {} -> Background rec
  
type alias ComposedBackgroundDescriptor rec = 
  { rec | background : BackgroundAlternative,
        backgroundComponents : BackgroundComponents
  } ->
  BackgroundFactory {}

type alias WithComponents rec = 
  { rec | backgroundComponents : BackgroundComponents
  }

type alias BackgroundFactory rec = 
  Background
    (WithComponents
      (Common.Initial (Background {})
        (Common.Inherit (Background {})
          (Common.Unset (Background {})
            (Common.Other (Background {}) rec)))))

initialBackgroundFactory : BackgroundFactory {}
initialBackgroundFactory =
  { background = CompositeBackground NoComponents
  , backgroundComponents = NoComponents
  , initial_    = { background = InitialBackground   }
  , inherit_    = { background = InheritBackground   }
  , unset_      = { background = UnsetBackground     }
  , other_ val  = { background = OtherBackground val }
  }

adjoinComponents : BackgroundComponents ->
                   BackgroundFactory {}
adjoinComponents newComponents = 
  { initialBackgroundFactory | background <- CompositeBackground newComponents 
                             , backgroundComponents <- newComponents 
  }

backgroundValue : BackgroundAlternative -> Property.Value
backgroundValue backgroundAlternative =
  case backgroundAlternative of
    InitialBackground   -> Common.initialValue
    InheritBackground   -> Common.inheritValue
    UnsetBackground     -> Common.unsetValue
    OtherBackground val -> Common.otherValue val
    CompositeBackground components -> componentsToValue components

componentsToValue : BackgroundComponents -> Property.Value
componentsToValue bgComponents = 
  let ntg = Nothing
  in componentsToValueRecursive bgComponents ntg ntg ntg ntg ntg ntg ntg ntg

{- Recommended order:
    image, position / size, repeat, attachment, origin, clip, color
At the least, origin must come before clip.
-}
componentsToValueRecursive : BackgroundComponents -> 
                             Maybe Property.Value ->   -- image
                             Maybe Property.Value ->   -- position
                             Maybe Property.Value ->   -- size
                             Maybe Property.Value ->   -- repeat
                             Maybe Property.Value ->   -- attachment
                             Maybe Property.Value ->   -- origin
                             Maybe Property.Value ->   -- clip
                             Maybe Property.Value ->   -- color
                             Property.Value
componentsToValueRecursive components mImg mPos mSiz mRpt mAtt mOrig mClp mColr =
  let recurse = componentsToValueRecursive
  in case components of
      -- If the FontComponents combinators are called more than once,
      -- the last (outer) one wins. So if it's already set we don't reset it.
      WithImage image inner ->
        case mImg of
          Just _ -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp mColr
          Nothing -> 
            recurse inner (Just image) mPos mSiz mRpt mAtt mOrig mClp mColr
      WithPositionAndSize position maybeSize inner -> 
        case mPos of
          Just _ -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp mColr
          Nothing -> 
            recurse inner mImg (Just position) maybeSize mRpt mAtt mOrig mClp mColr
      WithRepeat repeat inner ->
        case mRpt of
          Just _ -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp mColr
          Nothing -> 
            recurse inner mImg mPos mSiz (Just repeat) mAtt mOrig mClp mColr
      WithAttachment attachment inner -> 
        case mAtt of
          Just _ -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp mColr
          Nothing -> 
            recurse inner mImg mPos mSiz mRpt (Just attachment) mOrig mClp mColr
      WithOrigin origin inner -> 
        case mOrig of
          Just _ -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp mColr
          Nothing -> 
            recurse inner mImg mPos mSiz mRpt mAtt (Just origin) mClp mColr
      WithClip clip inner -> 
        case mClp of
          Just _ -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp mColr
          Nothing -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig (Just clip) mColr
      WithColor color inner ->
        case mColr of
          Just _ -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp mColr
          Nothing -> 
            recurse inner mImg mPos mSiz mRpt mAtt mOrig mClp (Just color)
      NoComponents -> 
        let maybePositionAndSize = 
              case (mPos, mSiz) of
                (Just (pos), Just(siz)) -> Property.intersperse "/" [pos, siz] |> Just
                (Just (pos), _) -> Just (pos)
                _ -> Nothing -- Size without position can't happen and is invalid.

            allValues = 
              [ mImg
              , maybePositionAndSize
              , mRpt
              , mAtt
              , mOrig
              , mClp
              , mColr
              ] |> List.filterMap identity
              
        in Property.spaceListValue identity allValues
