module Css.Internal.Box.Border.Stroke
  ( NubStrokeDescriptor, nubStrokeFactory
  , StrokeDescriptor, strokeFactory
  ) where
  
import Css.Internal.Property as Property
import Css.Internal.Common as Common

-------------------------------------------------------------------------------

type alias StrokeDescriptor rec = StrokeFactory rec -> Property.Value

type alias NubStrokeDescriptor rec = NubStrokeFactory rec -> Property.Value

type alias NubStrokeFactory rec =
  { rec | stroke: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubStrokeFactory : NubStrokeFactory {}
nubStrokeFactory =
  { stroke str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias StrokeFactory rec = 
  NubStrokeFactory 
    (Common.Inherit Property.Value 
      (Common.Initial Property.Value 
        (Common.Auto Property.Value 
          (Common.None Property.Value 
            (Common.Unset Property.Value rec)))))

strokeFactory : StrokeFactory {}
strokeFactory =
  let withInherit = { nubStrokeFactory | inherit_ = Common.inheritValue }
      withInitial = { withInherit      | initial_ = Common.initialValue }
      withAuto    = { withInitial      | auto_    = Common.autoValue }
      withNone    = { withAuto         | none_    = Common.noneValue }
      withUnset   = { withNone         | unset_   = Common.unsetValue }
  in withUnset
