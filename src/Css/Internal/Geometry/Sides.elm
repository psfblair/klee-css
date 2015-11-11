module Css.Internal.Geometry.Sides
  ( HorizontalSide, VerticalSide, horizontalSideValue, verticalSideValue

  -- * Constructing positions
    
  , sideLeft, sideCenter, sideRight
  , sideTop, sideMiddle, sideBottom

  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

type HorizontalSide 
  = LeftSide
  | CenterSide
  | RightSide
  | InitialHorizontalSide
  | InheritHorizontalSide
  | OtherHorizontalSide Property.Value

type VerticalSide
  = TopSide
  | MiddleSide
  | BottomSide
  | InitialVerticalSide
  | InheritVerticalSide
  | OtherVerticalSide Property.Value

-------------------------------------------------------------------------------

sideLeft : HorizontalSide
sideLeft = LeftSide

sideCenter : HorizontalSide
sideCenter = CenterSide

sideRight : HorizontalSide
sideRight = RightSide

sideTop : VerticalSide
sideTop = TopSide

sideMiddle : VerticalSide
sideMiddle = MiddleSide

sideBottom : VerticalSide
sideBottom = BottomSide

-------------------------------------------------------------------------------

horizontalSideValue : HorizontalSide -> Property.Value 
horizontalSideValue side = 
  case side of
    LeftSide -> Property.stringValue "left"
    CenterSide -> Property.stringValue "center"
    RightSide -> Property.stringValue "right"
    InitialHorizontalSide -> Common.initialValue
    InheritHorizontalSide -> Common.inheritValue
    OtherHorizontalSide val -> Common.otherValue val

verticalSideValue : VerticalSide -> Property.Value 
verticalSideValue side = 
  case side of
    TopSide -> Property.stringValue "top"
    MiddleSide -> Property.stringValue "center"
    BottomSide -> Property.stringValue "bottom"
    InitialVerticalSide -> Common.initialValue
    InheritVerticalSide -> Common.inheritValue
    OtherVerticalSide val -> Common.otherValue val
  
