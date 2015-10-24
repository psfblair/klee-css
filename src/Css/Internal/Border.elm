module Css.Internal.Border 
  ( StrokeDescriptor, strokeFactory, strokeValueFactory
  , OutlineColorDescriptor, outlineColorFactory
  ) where

import Css.Internal.Property exposing
  ( ValueFactory, stringValueFactory
  )

import Css.Common exposing 
  ( otherValue, initialValue, inheritValue, autoValue, noneValue )

import Css.Color exposing 
  (CssColor (..), ColorFactory, colorFactory
  )
-------------------------------------------------------------------------------

type alias StrokeDescriptor = StrokeFactory -> Stroke

type Stroke
  = Stroke String
  | NoStroke
  | InheritStroke
  | AutoStroke
  | OtherStroke String

type alias StrokeFactory =
  {
    stroke: String -> Stroke
  , none: Stroke
  , inherit: Stroke
  , auto: Stroke
  , other: String -> Stroke
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

strokeValueFactory : ValueFactory Stroke
strokeValueFactory =
  { value stroke =
      case stroke of
        Stroke str -> stringValueFactory.value str
        NoStroke -> noneValue
        InheritStroke -> inheritValue
        AutoStroke -> autoValue
        OtherStroke val -> otherValue val
  }

-------------------------------------------------------------------------------
-- NOTE outline-color takes "invert" as well as the standard color descriptors,
-- which is one reason we need ColorDescriptor to be parameterized.

type alias OutlineColorDescriptor = OutlineColorFactory -> CssColor

type alias InvertColorFactory = { invert: CssColor }

type alias OutlineColorFactory = ColorFactory InvertColorFactory

outlineColorFactory : OutlineColorFactory
outlineColorFactory = { colorFactory | invert = OtherColor "invert" }
  
