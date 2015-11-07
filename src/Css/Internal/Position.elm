module Css.Internal.Position
  ( HorizontalSide, VerticalSide
  , sideLeft, sideCenter, sideRight
  , sideTop, sideMiddle, sideBottom
  , horizontalSideValue, verticalSideValue
  ) where
  
import Css.Internal.Common exposing (initialValue, inheritValue, otherValue)
import Css.Internal.Property exposing (Value, stringValue)

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

type HorizontalSide 
  = LeftSide
  | CenterSide
  | RightSide
  | InitialHorizontalSide
  | InheritHorizontalSide
  | OtherHorizontalSide Value

horizontalSideValue : HorizontalSide -> Value 
horizontalSideValue side = 
  case side of
    LeftSide -> stringValue "left"
    CenterSide -> stringValue "center"
    RightSide -> stringValue "right"
    InitialHorizontalSide -> initialValue
    InheritHorizontalSide -> inheritValue
    OtherHorizontalSide val -> otherValue val

type VerticalSide
  = TopSide
  | MiddleSide
  | BottomSide
  | InitialVerticalSide
  | InheritVerticalSide
  | OtherVerticalSide Value

verticalSideValue : VerticalSide -> Value 
verticalSideValue side = 
  case side of
    TopSide -> stringValue "top"
    MiddleSide -> stringValue "center"
    BottomSide -> stringValue "bottom"
    InitialVerticalSide -> initialValue
    InheritVerticalSide -> inheritValue
    OtherVerticalSide val -> otherValue val
