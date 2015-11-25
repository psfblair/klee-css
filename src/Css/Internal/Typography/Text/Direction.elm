module Css.Internal.Typography.Text.Direction
  ( TextDirectionDescriptor, textDirectionFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias TextDirectionDescriptor = TextDirectionFactory -> Property.Value

type alias NubTextDirectionFactory rec =
  { rec | rightToLeft : Property.Value
        , leftToRight : Property.Value
        , other_ : Property.Value -> Property.Value
  } 
  
nubTextDirectionFactory : NubTextDirectionFactory {}
nubTextDirectionFactory =
  { rightToLeft = Property.stringValue "rtl"
  , leftToRight = Property.stringValue "ltr"
  , other_ val = Common.otherValue val
  }

type alias TextDirectionFactory =
  NubTextDirectionFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

textDirectionFactory : TextDirectionFactory 
textDirectionFactory =
  Common.addCommonValues nubTextDirectionFactory
