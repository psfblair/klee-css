module Css.Utils where
-- TODO These need to go into a library of their own

import Char
import String

{-| Some auxiliary mathematical functions.  -}

floatMod : Float -> Float -> Float
floatMod dividend divisor =
  let numberOfEvenMultiples = dividend / divisor  |> truncate |> toFloat
  in dividend - (numberOfEvenMultiples * divisor)

toFixed : Int -> Float -> Float
toFixed decimalPlaces x =
  let powersOf10 = 10 ^ decimalPlaces
      shiftedAndRounded = x * powersOf10 |> round |> toFloat
  in shiftedAndRounded / powersOf10

toHexString : Int -> String
toHexString num =
   if | num  < 10 -> toString num
      | num == 10 -> "A"
      | num == 11 -> "B"
      | num == 12 -> "C"
      | num == 13 -> "D"
      | num == 14 -> "E"
      | num == 15 -> "F"
      | otherwise ->
          let remainder = num % 16
              next = num // 16
          in toHexString next ++ toString remainder

fromHexChar : Char -> Result String Int
fromHexChar ch =
   if | Char.isDigit ch -> toString ch |> String.toInt
      | ch == 'A' -> Ok 10
      | ch == 'a' -> Ok 10
      | ch == 'B' -> Ok 11
      | ch == 'b' -> Ok 11
      | ch == 'C' -> Ok 12
      | ch == 'c' -> Ok 12
      | ch == 'D' -> Ok 13
      | ch == 'd' -> Ok 13
      | ch == 'E' -> Ok 14
      | ch == 'e' -> Ok 14
      | ch == 'F' -> Ok 15
      | ch == 'f' -> Ok 15
      | otherwise -> Err ("could not convert char '" ++ (toString ch) ++ "' to Int")

fromHex : String -> Result String Int
fromHex str =
  let hexDigits =
        (if String.startsWith "#" str then String.dropLeft 1 str else str)
        |> String.toList
      mapResultInto digitChar result =
        let convertedResult = fromHexChar digitChar
        in Result.map2 (\accum val -> (accum * 16) + val) result convertedResult
  in hexDigits |> List.foldr mapResultInto (Ok 0)