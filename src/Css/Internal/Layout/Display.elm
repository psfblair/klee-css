module Css.Internal.Layout.Display
  ( DisplayDescriptor, displayFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias DisplayDescriptor = DisplayFactory -> Property.Value

type alias NubDisplayFactory rec =
  { rec | display : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
nubDisplayFactory : NubDisplayFactory {}
nubDisplayFactory =
  { display str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias DisplayFactory =
  NubDisplayFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))

displayFactory : DisplayFactory
displayFactory =
  let withNone = { nubDisplayFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone
