module Css.Internal.Typography.Text.Decoration
  ( TextDecorationDescriptor, textDecorationFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias TextDecorationDescriptor = TextDecorationFactory -> Property.Value

type alias NubTextDecorationFactory rec =
  { rec | underline : Property.Value
        , overline : Property.Value
        , lineThrough : Property.Value
        , blink : Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubTextDecorationFactory : NubTextDecorationFactory {}
nubTextDecorationFactory =
  { underline = Property.stringValue "underline"
  , overline = Property.stringValue "overline"
  , lineThrough = Property.stringValue "line-through"
  , blink = Property.stringValue "blink"
  , other_ val = Common.otherValue val
  }

type alias TextDecorationFactory =
  NubTextDecorationFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.None Property.Value {}))))
  
textDecorationFactory : TextDecorationFactory
textDecorationFactory =
  let withNone = { nubTextDecorationFactory | none_ = Common.noneValue }
  in Common.addCommonValues withNone
