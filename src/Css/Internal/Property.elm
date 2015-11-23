module Css.Internal.Property
  ( Prefixed, Key, Value, Literal
  , toLiteral, toPrefixed, unPrefixed
  , stringKey, prefixedKey, appendToPrefixedRoot, prependToPrefixedRoot
  , emptyValue, appendValues, concatenateValues, appendUnits, intersperse
  , stringValue, prefixedValue, literalValue, intValue, floatValue
  , maybeValue, listValue, commaListValue
  , spaceListValue, spacePairValue, spaceTripleValue, spaceQuadrupleValue
  , commaQuadrupleValue, resultValue
  , managePrefixes
  ) where
  
import Dict as Dict

import Css.Internal.Browser as Browser
import Css.Internal.Utils as Utils

-------------------------------------------------------------------------------

type Literal = Literal String

toLiteral : String -> Literal
toLiteral str = Literal str

-------------------------------------------------------------------------------
{- `Prefixed` is for properties with browser prefixes.
-}
type Prefixed = Prefixed (List (Browser.BrowserPrefix, String))

toPrefixed : List (Browser.BrowserPrefix, String) -> Prefixed
toPrefixed items = Prefixed items

{- Unwrap a Prefixed. -}
unPrefixed : Prefixed -> List (String, String)
unPrefixed (Prefixed items) = 
  let toStringPair (prefix, root) = (Browser.stringPrefix prefix, root)
  in List.map toStringPair items

type Element
     = SimpleElement String
     | PrefixedElement Prefixed

{- Combine a string with a Prefixed containing browser prefixes. -}
appendToPrefixedRoot : Prefixed -> String -> Prefixed
appendToPrefixedRoot (Prefixed xs) str = 
  xs |> List.map (\(k, x) -> (k, x ++ str)) |> Prefixed

prependToPrefixedRoot : String -> Prefixed -> Prefixed
prependToPrefixedRoot str (Prefixed ys) = 
  ys |> List.map (\(k, y) -> (k, str ++ y)) |> Prefixed 

mergePrefixed : Prefixed -> Prefixed -> Prefixed
mergePrefixed (Prefixed xs) (Prefixed ys) =
  let kxs = List.map fst xs
      kys = List.map fst ys
      -- need to make BrowserPrefix into a Comparable (string) for sorting.
      byPrefixThenProperty = \(pfx, prop) -> (Browser.stringPrefix pfx, prop)
      xsWithKeysInKys = xs |> List.partition (fst >> (\x -> List.member x kys)) 
                           |> fst
                           |> List.sortBy byPrefixThenProperty
      ysWithKeysInKxs = ys |> List.partition (fst >> (\y -> List.member y kxs)) 
                           |> fst
                           |> List.sortBy byPrefixThenProperty
  in List.map2 (\(p, a) (_, b) -> 
      (p, a ++ b)) xsWithKeysInKys ysWithKeysInKxs |> Prefixed
  
{- Combine the Elements of two keys or values with/without prefixes. -}
merge : Element -> Element -> Element
merge element1 element2 =
  case (element1, element2) of
    (SimpleElement x, SimpleElement y) -> 
      SimpleElement (x ++ y)
    (PrefixedElement prefixed, SimpleElement y) -> 
      appendToPrefixedRoot prefixed y |> PrefixedElement
    (SimpleElement x, PrefixedElement prefixed) -> 
      prependToPrefixedRoot x prefixed |> PrefixedElement
    (PrefixedElement prefixed1, PrefixedElement prefixed2) -> 
      mergePrefixed prefixed1 prefixed2 |> PrefixedElement

-------------------------------------------------------------------------------
{- A type that represents the name of a CSS property. The type variable keeps
the type of the key and value coordinated. -}
type Key = Key Element

{- Turn a string into a key. -}
stringKey : String -> Key
stringKey str = SimpleElement str |> Key

prefixedKey : Prefixed -> Key
prefixedKey prefixed = PrefixedElement prefixed |> Key


-------------------------------------------------------------------------------
{- A type that represents the property value in a CSS property. Values can also
have prefixes, indicating that they pertain only to certain browser implementations
of the property. -}
type Value = Value Element
  
{- A value containing an empty string. Useful primarily as the starting value for
a fold. -}
emptyValue : Value
emptyValue = Value (SimpleElement "")

{- Concatenate two values, respecting the fact that one or the other might be
prefixed. -}
appendValues : Value -> Value -> Value
appendValues (Value v1) (Value v2) = merge v1 v2 |> Value


{- Concatenate a list of values, folding with the `appendValues` function. -}
concatenateValues : List Value -> Value
concatenateValues = List.foldr (\val accum -> appendValues val accum) emptyValue

{- For quantitative values with units; create a value out of the quantity and
the units. -}
appendUnits : Float -> String -> Value
appendUnits qty unit =
  appendValues (floatValue qty) (stringValue unit)

{- Add a separator value between each of the Elements of a list of values, 
and concatenate the result. -}
intersperse : String -> List Value -> Value
intersperse str values =
  let separatorValue = SimpleElement str |> Value
      interspersed = List.intersperse separatorValue values
  in List.foldr (\val accum -> appendValues val accum) emptyValue interspersed


stringValue : String -> Value 
stringValue str = SimpleElement str |> Value

prefixedValue : Prefixed -> Value
prefixedValue prefixed = PrefixedElement prefixed |> Value

literalValue : Literal -> Value 
literalValue (Literal x) = Utils.quote x |> SimpleElement |> Value 


intValue : Int -> Value 
intValue num = toString num |> SimpleElement |> Value 


floatValue : Float -> Value 
floatValue num = Utils.toFixed 5 num |> toString |> SimpleElement |> Value 


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
    (SimpleElement k, SimpleElement v) -> [ Ok (k, v) ]
    (PrefixedElement ks, SimpleElement v) -> 
        ks |> unPrefixed |> List.map (\(prefix, k) -> Ok (prefix ++ k, v))
    (SimpleElement k, PrefixedElement vs) -> 
        vs |> unPrefixed |> List.map (\(prefix, v) -> Ok (k, prefix ++ v))
    (PrefixedElement ks, PrefixedElement vs) ->
        ks |> unPrefixed 
           |> List.map
                (\(prefix, k) ->
                  let default = Err (prefix ++ k)
                      okFromVal val = Ok (prefix ++ k, prefix ++ val)
                      maybeVal = Dict.get prefix (Dict.fromList (unPrefixed vs))
                  in Maybe.map okFromVal maybeVal |> Maybe.withDefault default)
