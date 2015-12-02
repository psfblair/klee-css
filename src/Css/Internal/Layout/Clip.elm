module Css.Internal.Layout.Clip
  ( ClipDescriptor, clipFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------
--TODO Remove; use clip-path instead
type alias ClipDescriptor a b c d = ClipFactory a b c d -> Property.Value

type alias NubClipFactory a b c d rec =
  { rec | rect_ : Linear.NubSizeDescriptor {} a -> 
                  Linear.NubSizeDescriptor {} b -> 
                  Linear.NubSizeDescriptor {} c -> 
                  Linear.NubSizeDescriptor {} d -> 
                  Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubClipFactory : NubClipFactory a b c d {}
nubClipFactory =
  { rect_ top right bottom left = 
      let quadrupleDescriptor = Property.commaQuadrupleValue top right bottom left
          factory = 
            (Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory, Linear.nubSizeFactory)
          quadrupleValue = quadrupleDescriptor factory
          prefixValue = Property.stringValue "rect("
          suffixValue = Property.stringValue ")"
      in Property.concatenateValues [ prefixValue, quadrupleValue, suffixValue ]    
  , other_ val = Common.otherValue val
  }

type alias ClipFactory a b c d =
  NubClipFactory a b c d
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Auto Property.Value {}))))

clipFactory : ClipFactory a b c d
clipFactory =
  let withAuto = { nubClipFactory | auto_ = Common.autoValue }
  in Common.addCommonValues withAuto
