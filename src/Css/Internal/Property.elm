module Css.Internal.Property
  ( Key, Value, ValueElement, Literal (..)
  , simpleElement, unPrefixed, prefixedElements, quote
  , stringKey, prefixedKey, addPrefixes
  , emptyValue, appendValues, concatenateValues, intersperse
  , elementValue, stringValue, literalValue, intValue, floatValue
  , maybeValue, commaListValue
  , spaceListValue, spacePairValue, spaceTripleValue, spaceQuadrupleValue
  , commaQuadrupleValue, resultValue
  , managePrefixes
  ) where
  
import Dict
import Regex exposing (regex, replace)

import Css.Internal.Utils exposing (toFixed)

-------------------------------------------------------------------------------

type Literal = Literal String

-------------------------------------------------------------------------------
{- `PrefixedOrNot` is the type for keys and values in properties. 
`Prefixed` is for properties with browser prefixes.
-}
type ValueElement
     = SimpleValue String
     | Prefixed (List (String, String))

simpleElement : String -> ValueElement
simpleElement str = SimpleValue str

{- Unwrap a Prefixed. Used in rendering. -}
unPrefixed : ValueElement -> List (String, String)
unPrefixed (Prefixed inner) = inner

prefixedElements : List (String, String) -> ValueElement
prefixedElements listing = Prefixed listing

{- Combine a string key with a PrefixedOrNot containing browser prefixes. -}
addPrefixes : ValueElement -> String -> ValueElement
addPrefixes prefixes rootKey = SimpleValue rootKey |> merge prefixes

{- Escape quotes in a string. -}
quote : String -> String
quote str =
   let escaped = str |> replace Regex.All (regex "\"") (\_ -> "\\\"")
   in "\"" ++ escaped ++ "\""


{- Combine two keys or values with/without prefixes. -}
merge : ValueElement -> ValueElement -> ValueElement
merge prefixedOrNot1 prefixedOrNot2 =
  case (prefixedOrNot1, prefixedOrNot2) of
    ((SimpleValue x), (SimpleValue y)) -> SimpleValue (x ++ y)
    ((SimpleValue x), (Prefixed   ys)) -> 
      ys |> List.map (\(k, y) -> (k, x ++ y)) |> Prefixed
    ((Prefixed   xs), (SimpleValue y)) -> 
      xs |> List.map (\(k, x) -> (k, x ++ y)) |> Prefixed
    ((Prefixed   xs), (Prefixed   ys)) ->
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
type Key = Key ValueElement


{- Turn a string into a key. -}
stringKey : String -> Key
stringKey str = SimpleValue str |> Key

prefixedKey : ValueElement -> Key
prefixedKey prefixed = Key prefixed


-------------------------------------------------------------------------------
{- A type that represents the property value in a CSS property. Values can also
have prefixes, indicating that they pertain only to certain browser implementations
of the property. -}
type Value = Value ValueElement
  
{- A value containing an empty string. Useful primarily as the starting value for
a fold. -}
emptyValue : Value
emptyValue = Value (SimpleValue "")


elementValue : ValueElement -> Value
elementValue propertyValue = Value propertyValue

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
  let separatorValue = SimpleValue str |> Value
      interspersed = List.intersperse separatorValue values
  in List.foldr (\val accum -> appendValues val accum) emptyValue interspersed


stringValue : String -> Value 
stringValue str = SimpleValue str |> Value


literalValue : Literal -> Value 
literalValue (Literal x) = quote x |> SimpleValue |> Value 


intValue : Int -> Value 
intValue num = toString num |> SimpleValue |> Value 


floatValue : Float -> Value 
floatValue num = toFixed 5 num |> toString |> SimpleValue |> Value 


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


-------------------------------------------------------------------------------

{-  Returns either a pair of key,value (Ok), or a prefixed key (Err).
    The prefixes that are carried along by the Prefixed type are concatenated
    with the keys or values respectively if they pertain to one or the other.
    If both the keys and values are prefixed, for each of the key prefixes
    the corresponding value is looked up and the prefix is concatenated with
    both the key and value in the returned Right; if no value is found for
    the prefix, an Err containing the prefixed key is returned. This function is
    called by `renderProperties` for each key-value property for a rule.
    The results are fed to the properties function below which stringifies them.
 -}
managePrefixes : (Key, Value) -> List (Result String (String, String))
managePrefixes (Key ky, Value vl) =
  case (ky, vl) of
    ( SimpleValue k  , SimpleValue v  ) -> [ Ok (k, v) ]
    ( Prefixed    ks , SimpleValue v  ) -> 
        ks |> List.map (\(prefix, k) -> Ok (prefix ++ k, v))
    ( SimpleValue k  , Prefixed    vs ) -> 
        vs |> List.map (\(prefix, v) -> Ok (k, prefix ++ v))
    ( Prefixed    ks , Prefixed    vs ) ->
        ks |> List.map
                (\(prefix, k) ->
                  let default = Err (prefix ++ k)
                      okFromVal val = Ok (prefix ++ k, prefix ++ val)
                      maybeVal = Dict.get prefix (Dict.fromList vs)
                  in Maybe.map okFromVal maybeVal |> Maybe.withDefault default)
