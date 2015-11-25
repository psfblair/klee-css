module Css.Internal.Typography.Text.Align
  ( TextAlignDescriptor, textAlignFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Geometry.Sides as Sides

-------------------------------------------------------------------------------

type alias TextAlignDescriptor = TextAlignFactory -> Property.Value

type alias NubTextAlignFactory rec =
  { rec | alignWithSide : Sides.HorizontalSide -> Property.Value
        , justify : Property.Value
        , justifyAll : Property.Value
        , matchParent : Property.Value
        , start : Property.Value
        , end : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubTextAlignFactory : NubTextAlignFactory {}
nubTextAlignFactory =
  { alignWithSide side = Sides.horizontalSideValue side
  , justify = Property.stringValue "justify"
  , justifyAll = Property.stringValue "justify-all"
  , matchParent = Property.stringValue "match-parent"
  , start = Property.stringValue "start"
  , end = Property.stringValue "end"
  , other_ val = Common.otherValue val
  }

type alias TextAlignFactory =
  NubTextAlignFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

textAlignFactory : TextAlignFactory
textAlignFactory =
  Common.addCommonValues nubTextAlignFactory
  
