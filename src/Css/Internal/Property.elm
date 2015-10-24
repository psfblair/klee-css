module Css.Internal.Property
  ( Key (..), Value (..), PrefixedOrNot (..), Literal (..)
  , unPrefixed, plain, quote, stringKey, prefixedKeys
  , emptyValue, appendValues, concatenateValues, intersperse
  , stringValue, literalValue, intValue, floatValue
  , maybeValue, commaListValue
  , spaceListValue, spacePairValue, spaceTripleValue, spaceQuadrupleValue
  , commaQuadrupleValue, resultValue
  ) where

import Dict exposing (fromList, get)
import Regex exposing (regex, replace)

import Css.Internal.Utils exposing (Either (..), rightValue, toFixed)

-------------------------------------------------------------------------------

type Literal = Literal String

-------------------------------------------------------------------------------
{- `PrefixedOrNot` is the type for keys and values in properties. 
`Prefixed` is for properties with browser prefixes.
-}
type PrefixedOrNot
     = Prefixed (List (String, String))
     | Plain String


{- Unwrap a PrefixedOrNot into a string. -}
plain : PrefixedOrNot -> String
plain prefixedOrNot =
  case prefixedOrNot of
    Prefixed xs -> Dict.fromList xs |> Dict.get "" |> Maybe.withDefault ""
    Plain str -> str


{- Unwrap a Prefixed. -}
unPrefixed : PrefixedOrNot -> List (String, String)
unPrefixed (Prefixed inner) = inner


{- Escape quotes in a string. -}
quote : String -> String
quote str =
   let escaped = str |> replace Regex.All (regex "\"") (\_ -> "\\\"")
   in "\"" ++ escaped ++ "\""


{- Combine two keys or values with/without prefixes. -}
merge : PrefixedOrNot -> PrefixedOrNot -> PrefixedOrNot
merge prefixedOrNot1 prefixedOrNot2 =
  case (prefixedOrNot1, prefixedOrNot2) of
    ((Plain     x), (Plain     y)) -> Plain (x ++ y)
    ((Plain     x), (Prefixed ys)) -> ys |> List.map (\(k, y) -> (k, x ++ y)) |> Prefixed
    ((Prefixed xs), (Plain     y)) -> xs |> List.map (\(k, x) -> (k, x ++ y)) |> Prefixed
    ((Prefixed xs), (Prefixed ys)) ->
      let kxs = List.map fst xs
          kys = List.map fst ys
          xsWithKeysInKys = List.partition (fst >> (\x -> List.member x kys)) xs
                                       |> fst
                                       |> List.sort
          ysWithKeysInKxs = List.partition (fst >> (\y -> List.member y kxs)) ys
                                       |> fst
                                       |> List.sort
      in List.map2 (\(p, a) (_, b) -> (p, a ++ b)) xsWithKeysInKys ysWithKeysInKxs
                                      |> Prefixed

-------------------------------------------------------------------------------
{- A type that represents the name of a CSS property. The type variable keeps
the type of the key and value coordinated. -}
type Key = Key PrefixedOrNot


{- Turn a string into a key. -}
stringKey : String -> Key
stringKey str = Plain str |> Key


{- Combine a string key with a PrefixedOrNot containing browser prefixes. -}
prefixedKeys : PrefixedOrNot -> String -> PrefixedOrNot
prefixedKeys prefixes rootKey = Plain rootKey |> merge prefixes


-------------------------------------------------------------------------------
{- A type that represents the property value in a CSS property. Values can also
have prefixes, indicating that they pertain only to certain browser implementations
of the property. -}
type Value = Value PrefixedOrNot


{- A value containing an empty string. Useful primarily as the starting value for
a fold. -}
emptyValue : Value
emptyValue = Value (Plain "")


{- Concatenate two values, respecting the fact that one or the other might be
prefixed. -}
appendValues : Value -> Value -> Value
appendValues (Value v1) (Value v2) = merge v1 v2 |> Value


{- Concatenate a list of values, folding with the `appendValues` function. -}
concatenateValues : List Value -> Value
concatenateValues = List.foldr (\val accum -> appendValues val accum) emptyValue


{- Add a separator value between each of the elements of a list of values, 
and concatenate the result. -}
intersperse : String -> List Value -> Value
intersperse str values =
  let separatorValue = Plain str |> Value
      interspersed = List.intersperse separatorValue values
  in List.foldr (\val accum -> appendValues val accum) emptyValue interspersed


stringValue : String -> Value 
stringValue str = Plain str |> Value


literalValue : Literal -> Value 
literalValue (Literal x) = quote x |> Plain |> Value 


intValue : Int -> Value 
intValue num = toString num |> Plain |> Value 


floatValue : Float -> Value 
floatValue num = toFixed 5 num |> toString |> Plain |> Value 


maybeValue : (a -> Value) -> Maybe a -> Value
maybeValue innerConverter innerValue =
  case innerValue of
    Just val -> innerConverter val
    Nothing -> emptyValue


listValue : String -> (a -> Value) -> List a -> Value 
listValue separator innerConverter xs =
  List.map innerConverter xs |> intersperse separator


commaListValue : (a -> Value) -> List a -> Value 
commaListValue = listValue ","


spaceListValue : (a -> Value) -> List a -> Value 
spaceListValue = listValue " "


pairValue : String -> (a -> Value) -> (b -> Value) -> (a, b) -> Value
pairValue separator innerConverter1 innerConverter2 (x, y) =
  [ innerConverter1 x, innerConverter2 y ] |> intersperse separator


spacePairValue : (a -> Value) -> (b -> Value) -> (a, b) -> Value
spacePairValue = pairValue " "


tripleValue : String -> 
              (a -> Value) -> 
              (b -> Value) -> 
              (c -> Value) -> 
              (a, b, c) -> 
              Value
tripleValue separator innerConverter1 innerConverter2 innerConverter3 (x, y, z) =
  [ innerConverter1 x, innerConverter2 y, innerConverter3 z ]
    |> intersperse separator


spaceTripleValue : (a -> Value) -> 
                   (b -> Value) -> 
                   (c -> Value) -> 
                   (a, b, c) -> 
                   Value
spaceTripleValue = tripleValue " "


quadrupleValue : String -> 
                 (a -> Value) -> 
                 (b -> Value) -> 
                 (c -> Value) -> 
                 (d -> Value) -> 
                 (a, b, c, d) -> 
                 Value
quadrupleValue separator innerConv1 innerConv2 innerConv3 innerConv4 (x, y, z, a) =
  [ innerConv1 x, innerConv2 y, innerConv3 z, innerConv4 a ] 
    |> intersperse separator


spaceQuadrupleValue : (a -> Value) -> 
                      (b -> Value) -> 
                      (c -> Value) -> 
                      (d -> Value) -> 
                      (a, b, c, d) -> 
                      Value
spaceQuadrupleValue = quadrupleValue " "


commaQuadrupleValue : (a -> Value) -> 
                      (b -> Value) -> 
                      (c -> Value) -> 
                      (d -> Value) -> 
                      (a, b, c, d) -> 
                      Value
commaQuadrupleValue = quadrupleValue ","


resultValue : (a -> Value) -> (b -> Value) -> (Result a b) -> Value
resultValue errConverter okConverter val =
  case val of
    Err a -> errConverter a
    Ok  a -> okConverter a
