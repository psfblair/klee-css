module Css.Internal.Typography.Text.Indent
  ( TextIndentDescriptor, textIndentFactory
  ) where

import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias TextIndentDescriptor sz = TextIndentFactory sz -> Property.Value

type alias NubTextIndentFactory sz rec =
  { rec | textIndent : Linear.NubSizeDescriptor {} sz -> Property.Value
        , indentEachLine : Property.Value
        , hangingIndent : Property.Value
        , other_ : Property.Value -> Property.Value
  } 
  
nubTextIndentFactory : NubTextIndentFactory sz {}
nubTextIndentFactory =
  { textIndent sizeDescriptor = sizeDescriptor Linear.nubSizeFactory
  , indentEachLine = Property.stringValue "each-line"
  , hangingIndent = Property.stringValue "hanging"
  , other_ val = Common.otherValue val
  }

type alias TextIndentFactory sz =
  NubTextIndentFactory sz
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

textIndentFactory : TextIndentFactory sz
textIndentFactory =
  Common.addCommonValues nubTextIndentFactory
