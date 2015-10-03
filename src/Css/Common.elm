module Css.Common where

import Css.Property exposing (PrefixedOrNot)

{-
class All      a where all      : a
class Auto     a where auto     : a
class Baseline a where baseline : a
class Center   a where center   : a
class Inherit  a where inherit  : a
class None     a where none     : a
class Normal   a where normal   : a
class Visible  a where visible  : a
class Hidden   a where hidden   : a
class Initial  a where initial  : a
class Unset    a where unset    : a

-- The Other type class is used to escape from the type safety introduced by
-- embedding CSS properties into the typed world of Clay.
-- `Other` allows you to cast any `Value` to a specific value type.

class Other   a where other   : Value -> a

instance All      Value where all      = "all"
instance Auto     Value where auto     = "auto"
instance Baseline Value where baseline = "baseline"
instance Center   Value where center   = "center"
instance Inherit  Value where inherit  = "inherit"
instance Normal   Value where normal   = "normal"
instance None     Value where none     = "none"
instance Visible  Value where visible  = "visible"
instance Hidden   Value where hidden   = "hidden"
instance Other    Value where other    = id
instance Initial  Value where initial  = "initial"
instance Unset    Value where unset    = "unset"

-}


{-| List of browser prefixes to make experimental properties work in
different browsers. -}

browsers : PrefixedOrNot
browsers =
  Css.Property.Prefixed
  [ ( "-webkit-", "" )
  , (    "-moz-", "" )
  , (     "-ms-", "" )
  , (      "-o-", "" )
  , (         "", "" )
  ]

{-| Syntax for CSS function call. -}

call : String -> String -> String
call fn arg = fn ++ "(" ++ arg ++ ")"


{-| Some auxiliary mathematical functions.  -}

floatMod : Float -> Float -> Float
floatMod dividend divisor =
  let numberOfEvenMultiples = dividend / divisor  |> truncate |> toFloat
  in dividend - (numberOfEvenMultiples * divisor)

-- TODO Integrate with fixedShow in Property.elm
decimalRound : Float -> Int -> Float
decimalRound x decimalPlaces =
  let powersOf10 = 10 ^ decimalPlaces
      shiftedAndRounded = x * powersOf10 |> round |> toFloat
  in shiftedAndRounded / powersOf10
