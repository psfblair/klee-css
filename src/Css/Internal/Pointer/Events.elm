module Css.Internal.Pointer.Events
  ( PointerEventsDescriptor, pointerEventsFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias PointerEventsDescriptor = PointerEventsFactory -> Property.Value

type alias NubPointerEventsFactory rec =
  { rec | pointerEvents : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubPointerEventsFactory : NubPointerEventsFactory {}
nubPointerEventsFactory =
  { pointerEvents str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
type alias PointerEventsFactory =
  NubPointerEventsFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value 
            (Common.None Property.Value {})))))
  
pointerEventsFactory : PointerEventsFactory
pointerEventsFactory =
  let withNone = { nubPointerEventsFactory | none_ = Common.noneValue }
      withAuto = { withNone                | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto
