module Css.Internal.Typography.Font.Size
  ( FontSizeDescriptor, fontSizeFactory
  ) where

import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property

-------------------------------------------------------------------------------

type alias FontSizeDescriptor sz = FontSizeFactory sz -> Property.Value

type alias WithFontSize = { fontSize: String -> Property.Value }
  
type alias FontSizeFactory sz = Linear.SizeFactory WithFontSize sz

fontSizeFactory : FontSizeFactory sz
fontSizeFactory = 
  let basicSizeFactory = Linear.basicSizeFactory
  in { basicSizeFactory | fontSize = \str -> Property.stringValue str }
