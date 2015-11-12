module Css.Internal.Background
  ( BackgroundColorDescriptor, backgroundColorFactory
  , BackgroundPositionDescriptor, backgroundPositionFactory
  , BackgroundSizeDescriptor, backgroundSizeFactory
  , BackgroundRepeatDescriptor, backgroundRepeatFactory
  , BackgroundImageDescriptor, backgroundImageFactory
  , BackgroundAttachmentDescriptor, backgroundAttachmentFactory
  , Background, BackgroundDescriptor, ComposedBackgroundDescriptor
  , BackgroundComponents (..)
  , initialBackgroundFactory, adjoinComponents, backgroundValue
  ) where

import String

import Css.Internal.Color exposing 
  (CssColor (..), ColorFactory, colorFactory, colorValue)
import Css.Internal.Common exposing 
  (autoValue, initialValue, inheritValue, noneValue, otherValue)
import Css.Internal.Property exposing 
  (Value, intersperse, stringValue, spacePairValue, maybeValue, spaceListValue)

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Geometry.Sides as Sides

-------------------------------------------------------------------------------

type alias BackgroundPositionDescriptor sz1 sz2 = 
    BackgroundPositionFactory sz1 sz2 -> Value

type alias BackgroundPositionFactory sz1 sz2 =
  { sizedPosition : Linear.SizeDescriptor {} sz1 -> 
                    Linear.SizeDescriptor {} sz2 -> 
                    Value
  , sidedPosition : Sides.HorizontalSide -> Sides.VerticalSide -> Value
  , initial_ : Value
  , inherit_ : Value
  , other_ : Value -> Value
  }

backgroundPositionFactory : BackgroundPositionFactory sz1 sz2
backgroundPositionFactory = 
  { sizedPosition horizontalDescriptor verticalDescriptor = 
      let valueFactory = spacePairValue Linear.sizeValue Linear.sizeValue
      in valueFactory (horizontalDescriptor, verticalDescriptor)
  , sidedPosition horizontal vertical = 
      let sides = (horizontal, vertical)
          valueFactory = spacePairValue Sides.horizontalSideValue Sides.verticalSideValue
      in valueFactory sides
  , initial_ = initialValue
  , inherit_ = inheritValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias BackgroundSizeDescriptor sz = BackgroundSizeFactory sz -> Value

type alias BackgroundSizeFactory szTyp =
  { backgroundSize : Linear.SizeDescriptor {} szTyp -> 
                     Linear.SizeDescriptor {} szTyp -> 
                     Value
  , partial : Linear.SizeDescriptor {} szTyp -> Value
  , named : String -> Value
  , auto_ : Value
  , initial_ : Value
  , inherit_ : Value
  , other_ : Value -> Value
  }

backgroundSizeFactory : BackgroundSizeFactory sz
backgroundSizeFactory =
  { backgroundSize widthDescriptor heightDescriptor = 
      let valueFactory = spacePairValue Linear.sizeValue Linear.sizeValue
      in valueFactory (widthDescriptor, heightDescriptor)
  , partial widthDescriptor = 
      let valueFactory = spacePairValue Linear.sizeValue identity
      in valueFactory (widthDescriptor, autoValue)
  , named str = stringValue str
  , auto_ = autoValue
  , initial_ = initialValue
  , inherit_ = inheritValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------
--TODO Fix when colors are fixed
type alias BackgroundColorDescriptor = BackgroundColorFactory -> CssColor

type alias TransparentColorFactory = { transparent: CssColor }

type alias BackgroundColorFactory = ColorFactory TransparentColorFactory

backgroundColorFactory : BackgroundColorFactory
backgroundColorFactory = 
  { colorFactory | transparent = OtherColor (stringValue "transparent") }

-------------------------------------------------------------------------------

type alias BackgroundImageDescriptor = BackgroundImageFactory -> Value

type alias BackgroundImageFactory =
  { url : String -> Value 
  , none_ : Value 
  , initial_ : Value 
  , inherit_ : Value 
  , other_ : Value -> Value 
  }  

backgroundImageFactory : BackgroundImageFactory
backgroundImageFactory =
  { url imgUrl = stringValue (String.concat ["url(\"", imgUrl ,"\")"])
  , none_ = noneValue
  , initial_ = initialValue 
  , inherit_ = inheritValue 
  , other_ val = otherValue val
  }  

-------------------------------------------------------------------------------

type alias BackgroundRepeatDescriptor = BackgroundRepeatFactory -> Value

type alias BackgroundRepeatFactory =
  { repeat : String -> Value 
  , initial_ : Value 
  , inherit_ : Value 
  , other_ : Value -> Value 
  }  

backgroundRepeatFactory : BackgroundRepeatFactory
backgroundRepeatFactory =
  { repeat str = stringValue str
  , initial_ = initialValue 
  , inherit_ = inheritValue 
  , other_ val = otherValue val
  }  

-------------------------------------------------------------------------------

type alias BackgroundAttachmentDescriptor = BackgroundAttachmentFactory -> Value

type alias BackgroundAttachmentFactory =
  { bgAttachment : String -> Value 
  , initial_ : Value 
  , inherit_ : Value 
  , other_ : Value -> Value 
  }  

backgroundAttachmentFactory : BackgroundAttachmentFactory
backgroundAttachmentFactory =
  { bgAttachment str = stringValue str
  , initial_ = initialValue 
  , inherit_ = inheritValue 
  , other_ val = otherValue val 
  }  

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
  | WithPositionAndSize Value (Maybe Value) (BackgroundComponents sz1 sz2 sz3)
  | WithColor CssColor (BackgroundComponents sz1 sz2 sz3)
  | WithImage Value (BackgroundComponents sz1 sz2 sz3)
  | WithRepeat Value (BackgroundComponents sz1 sz2 sz3)
  | WithOrigin Value (BackgroundComponents sz1 sz2 sz3)
  | WithClip Value (BackgroundComponents sz1 sz2 sz3)
  | WithAttachment Value (BackgroundComponents sz1 sz2 sz3)

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
                             Maybe Value ->   -- image
                             Maybe Value ->   -- position
                             Maybe Value ->   -- size
                             Maybe Value ->   -- repeat
                             Maybe Value ->   -- attachment
                             Maybe Value ->   -- origin
                             Maybe Value ->   -- clip
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
        let maybePositionAndSize = 
              case (mPos, mSiz) of
                (Just (pos), Just(siz)) -> intersperse "/" [pos, siz] |> Just
                (Just (pos), _) -> Just (pos)
                _ -> Nothing -- Size without position can't happen and is invalid.

            maybeColor = Maybe.map colorValue mColor

            allValues = 
              [ mImg
              , maybePositionAndSize
              , mRepeat
              , mAttach
              , mOrig
              , mClip
              , maybeColor
              ] |> List.filterMap identity
              
        in spaceListValue identity allValues
