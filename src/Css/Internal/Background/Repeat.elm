module Css.Internal.Background.Repeat
  ( BackgroundRepeatDescriptor, backgroundRepeatFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias BackgroundRepeatDescriptor = BackgroundRepeatFactory -> Property.Value

type alias NubBackgroundRepeatFactory rec =
  { rec | repeat : String -> Property.Value 
        , other_ : Property.Value -> Property.Value 
  }  

type alias BackgroundRepeatFactory =
  NubBackgroundRepeatFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

nubBackgroundRepeatFactory : NubBackgroundRepeatFactory {}
nubBackgroundRepeatFactory =
  { repeat str = Property.stringValue str
  , other_ val = Common.otherValue val
  }  

backgroundRepeatFactory : BackgroundRepeatFactory
backgroundRepeatFactory = Common.addCommonValues nubBackgroundRepeatFactory
