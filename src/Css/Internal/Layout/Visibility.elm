module Css.Internal.Layout.Visibility
  ( VisibilityDescriptor, VisibilityFactory, visibilityFactory
  , ExtendedVisibilityDescriptor, extendedVisibilityFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias VisibilityDescriptor rec =
  VisibilityFactory rec -> Property.Value

type alias NubVisibilityFactory rec =
  { rec | collapse : Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
nubVisibilityFactory : NubVisibilityFactory {}
nubVisibilityFactory =
  { collapse = Property.stringValue "collapse"
  , other_ val = Common.otherValue val
  }

type alias VisibilityFactory rec =
  NubVisibilityFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value rec)))

visibilityFactory : VisibilityFactory {}
visibilityFactory = Common.addCommonValues nubVisibilityFactory

type alias ExtendedVisibilityDescriptor rec = 
  ExtendedVisibilityFactory rec -> Property.Value

type alias WithExtendedVisibility rec = 
  { rec | visible_ : Property.Value
        , hidden_ : Property.Value
  }

type alias ExtendedVisibilityFactory rec = 
  VisibilityFactory (WithExtendedVisibility rec)

extendedVisibilityFactory : ExtendedVisibilityFactory {}
extendedVisibilityFactory =
  let withVisible = { visibilityFactory | visible_ = Common.visibleValue }
      withHidden  = { withVisible       | hidden_  = Common.hiddenValue  }
  in withHidden
