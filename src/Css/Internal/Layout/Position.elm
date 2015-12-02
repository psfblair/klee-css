module Css.Internal.Layout.Position
  ( PositionDescriptor, positionFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias PositionDescriptor = PositionFactory -> Property.Value

type alias NubPositionFactory rec =
  { rec | position : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubPositionFactory : NubPositionFactory {}
nubPositionFactory =
  { position str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias PositionFactory =
  NubPositionFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

positionFactory : PositionFactory
positionFactory = Common.addCommonValues nubPositionFactory
