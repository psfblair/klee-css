module Css.Internal.Layout.Overflow
  ( OverflowDescriptor, overflowFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias OverflowDescriptor = OverflowFactory -> Property.Value

type alias NubOverflowFactory rec =
  { rec | overflow : String -> Property.Value
  , other_ : Property.Value -> Property.Value
  }

nubOverflowFactory : NubOverflowFactory {}
nubOverflowFactory =
  { overflow str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias OverflowFactory =
  NubOverflowFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value 
            (Common.Visible Property.Value 
              (Common.Hidden Property.Value {}))))))
  
overflowFactory : OverflowFactory
overflowFactory =
  let withAuto    = { nubOverflowFactory | auto_ = Common.autoValue       }
      withVisible = { withAuto           | visible_ = Common.visibleValue }
      withHidden  = { withVisible        | hidden_ = Common.hiddenValue   }
  in Common.addCommonValues withHidden
