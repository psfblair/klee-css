module Css.Internal.Layout.Clear
  ( ClearDescriptor, clearFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias ClearDescriptor = ClearFactory -> Property.Value

type alias NubClearFactory rec =
  { rec | clear: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubClearFactory : NubClearFactory {}
nubClearFactory =
  { clear str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias ClearFactory =
  NubClearFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

clearFactory : ClearFactory
clearFactory =
  let withNone = { nubClearFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone
