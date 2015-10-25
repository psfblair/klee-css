module Css.Internal.Border 
  ( StrokeDescriptor, strokeFactory, strokeValue
  , OutlineColorDescriptor, outlineColorFactory
  ) where

import Css.Internal.Property exposing (Value, stringValue)

import Css.Internal.Common exposing 
  ( otherValue, initialValue, inheritValue, autoValue, noneValue )

import Css.Internal.Color exposing 
  (CssColor (..), ColorFactory, colorFactory)
-------------------------------------------------------------------------------

type alias StrokeDescriptor = StrokeFactory -> Stroke

type Stroke
  = Stroke String
  | NoStroke
  | InheritStroke
  | AutoStroke
  | OtherStroke Value

type alias StrokeFactory =
  {
    stroke: String -> Stroke
  , none: Stroke
  , inherit: Stroke
  , auto: Stroke
  , other: Value -> Stroke
  }

strokeFactory : StrokeFactory
strokeFactory =
  {
    stroke str = Stroke str
  , none = NoStroke
  , inherit = InheritStroke
  , auto = AutoStroke
  , other val = OtherStroke val
  }

strokeValue : Stroke -> Value 
strokeValue stroke =
  case stroke of
    Stroke str -> stringValue str
    NoStroke -> noneValue
    InheritStroke -> inheritValue
    AutoStroke -> autoValue
    OtherStroke val -> otherValue val

-------------------------------------------------------------------------------
-- NOTE outline-color takes "invert" as well as the standard color descriptors,
-- which is one reason we need ColorDescriptor to be parameterized.

type alias OutlineColorDescriptor = OutlineColorFactory -> CssColor

type alias InvertColorFactory = { invert: CssColor }

type alias OutlineColorFactory = ColorFactory InvertColorFactory

outlineColorFactory : OutlineColorFactory
outlineColorFactory = { colorFactory | invert = OtherColor (stringValue "invert") }
  
