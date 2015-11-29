module Css.Internal.Background.Image
  ( BackgroundImageDescriptor, backgroundImageFactory
  ) where

import String

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias BackgroundImageDescriptor = BackgroundImageFactory -> Property.Value

type alias NubBackgroundImageFactory rec =
  { rec | url : String -> Property.Value 
        , other_ : Property.Value -> Property.Value 
  }  

type alias BackgroundImageFactory =
  NubBackgroundImageFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

nubBackgroundImageFactory : NubBackgroundImageFactory {}
nubBackgroundImageFactory =
  { url imgUrl = Property.stringValue (String.concat ["url(\"", imgUrl ,"\")"])
  , other_ val = Common.otherValue val
  }  

backgroundImageFactory : BackgroundImageFactory
backgroundImageFactory =
  let withNone = { nubBackgroundImageFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone
