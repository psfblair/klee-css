module Css.Internal.Pointer.Cursor
  ( CursorDescriptor, cursorFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias CursorDescriptor = CursorFactory -> Property.Value

type alias NubCursorFactory rec =
  { rec | cursor : String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

type alias CursorFactory =
  NubCursorFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value 
            (Common.None Property.Value {})))))
  
nubCursorFactory : NubCursorFactory {}
nubCursorFactory =
  { cursor str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
cursorFactory : CursorFactory
cursorFactory =
  let withAuto = { nubCursorFactory | auto_ = Common.autoValue }
      withNone = { withAuto         | none_ = Common.noneValue }
  in Common.addCommonValues withNone
