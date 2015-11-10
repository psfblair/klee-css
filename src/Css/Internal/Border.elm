module Css.Internal.Border 
  ( StrokeDescriptor, strokeFactory
  , OutlineColorDescriptor, outlineColorFactory
  ) where

import Css.Internal.Property exposing (Value, stringValue)

import Css.Internal.Common exposing 
  ( otherValue, initialValue, inheritValue, autoValue, noneValue )

import Css.Internal.Color exposing 
  (CssColor (..), ColorFactory, colorFactory)
-------------------------------------------------------------------------------

type alias StrokeDescriptor = StrokeFactory -> Value

type alias StrokeFactory =
  {
    stroke: String -> Value
  , none_ : Value
  , inherit_ : Value
  , auto_ : Value
  , other_ : Value -> Value
  }

strokeFactory : StrokeFactory
strokeFactory =
  {
    stroke str = stringValue str
  , none_ = noneValue
  , inherit_ = inheritValue
  , auto_ = autoValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------
-- TODO Fix along with color fixes
-- NOTE outline-color takes "invert" as well as the standard color descriptors,
-- which is one reason we need ColorDescriptor to be parameterized.

type alias OutlineColorDescriptor = OutlineColorFactory -> CssColor

type alias InvertColorFactory = { invert: CssColor }

type alias OutlineColorFactory = ColorFactory InvertColorFactory

outlineColorFactory : OutlineColorFactory
outlineColorFactory = { colorFactory | invert = OtherColor (stringValue "invert") }
  
