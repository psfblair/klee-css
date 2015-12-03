module Css.Internal.Layout.VerticalAlign
  ( VerticalAlignDescriptor, verticalAlignFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias VerticalAlignDescriptor sz = VerticalAlignFactory sz -> Property.Value

-- Since NubSizeDescriptor is parameterized by a generic type `a` rather than
-- simply by `Size a`, that means that for dimensioned sizes it just calls
-- whatever `size` function is passed to it in the record -- that function
-- doesn't need to return a `Size`. So we can pass this factory to a 
-- NubSizeDescriptor and get a `VerticalAlignValue` out instead of a `Size`.
type alias WithVerticalAlign =
  { vAlign : String -> Property.Value
  , baseline_ : Property.Value
  }

type alias VerticalAlignFactory sz = Linear.SizeFactory WithVerticalAlign sz

verticalAlignFactory : VerticalAlignFactory sz
verticalAlignFactory =
  let sizeFactory  = Linear.basicSizeFactory
      withBaseline = { sizeFactory  | baseline_ = Common.baselineValue }
      withVAlign   = { withBaseline | vAlign = \str -> Property.stringValue str }
  in withVAlign
