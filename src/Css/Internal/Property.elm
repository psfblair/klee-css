module Css.Internal.Property
  ( Key (..), Value (..), PrefixedOrNot (..), Literal (..)
  , unPrefixed, plain, quote, stringKey, prefixedKeys, cast
  , ValueFactory, emptyValue, appendValues, concatenateValues, intersperse
  , stringValueFactory, literalValueFactory, intValueFactory, floatValueFactory
  , valueValueFactory, maybeValueFactory, commaListValueFactory
  , spaceListValueFactory, spacePairValueFactory, spaceTripleValueFactory
  , spaceQuadrupleValueFactory, commaQuadrupleValueFactory, eitherValueFactory
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
type Key a = Key PrefixedOrNot


{- Turn a string into a key. -}
stringKey : String -> Key a
stringKey str = Plain str |> Key


{- Combine a string key with a PrefixedOrNot containing browser prefixes. -}
prefixedKeys : PrefixedOrNot -> String -> PrefixedOrNot
prefixedKeys prefixes rootKey = Plain rootKey |> merge prefixes


{- Rules store property keys parameterized with the unit type; this allows
them to be aggregated in a single collection. The `cast` method is used to
replace any other type parameter with unit. -}
cast : Key a -> Key ()
cast (Key k) = Key k


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


-------------------------------------------------------------------------------
{- The type representing something that converts a value of some type to a 
value of `Value` type. The `ValueFactory` type is a parameterized type alias for 
a record holding a function converting something of the type of the parameter to
a `Value`. `ValueFactory` allows similar polymorphic behavior to a Haskell 
typeclass, except that the record of functions needs to be passed to any 
function making use of a value of a type in that "class." -}
type alias ValueFactory a = { value : a -> Value }


{- Converts a string to a `Value`. -}
stringValueFactory : ValueFactory String
stringValueFactory = { value x = Plain x |> Value}


{- Converts a `Literal` to a `Value`. -}
literalValueFactory : ValueFactory Literal
literalValueFactory = { value (Literal x) = quote x |> Plain |> Value }


{- Converts an `Int` to a `Value`. -}
intValueFactory : ValueFactory Int
intValueFactory = { value x = toString x |> Plain |> Value }


{- Converts a `Float` to a `Value`. -}
floatValueFactory : ValueFactory Float
floatValueFactory = { value x = toFixed 5 x |> toString |> Plain |> Value }


{- Converts a `Value` to a `Value`. This is useful in cases where a `Value` 
needs to be passed to a function that may take other types as well. -}
valueValueFactory : ValueFactory Value
valueValueFactory = { value = identity }


{- Converts a `Maybe` to a `Value`. -}
maybeValueFactory : ValueFactory a -> ValueFactory (Maybe a)
maybeValueFactory innerFactory =
  { value x =
      case x of
        Just val -> innerFactory.value val
        Nothing -> emptyValue
  }


{- Converts a `List a` to a `Value`, given a separator string and a value
factory for items of type a. -}
listValueFactory : String -> ValueFactory a -> ValueFactory (List a)
listValueFactory separator innerFactory =
  let wrapList xs =
    case xs of
      [] -> []
      (h :: t) -> (innerFactory.value h) :: (wrapList t)
  in { value = wrapList >> intersperse separator }


{- Converts a `List a` to a `Value` of comma-separated items, given a value
factory for items of type a. -}
commaListValueFactory : ValueFactory a -> ValueFactory (List a)
commaListValueFactory = listValueFactory ","


{- Converts a `List a` to a `Value` of space-separated items, given a value
factory for items of type a. -}
spaceListValueFactory : ValueFactory a -> ValueFactory (List a)
spaceListValueFactory = listValueFactory " "


{- Converts a pair `(a, b)` to a `Value`, given a separator string and value
factories for items of type a and of type b. -}
pairValueFactory : String -> ValueFactory a -> ValueFactory b -> ValueFactory (a, b)
pairValueFactory separator firstInnerFactory secondInnerFactory =
  { value (x,y) =
        [ firstInnerFactory.value x, secondInnerFactory.value y ]
        |> intersperse separator
  }


{- Converts a pair `(a, b)` to a `Value` of space-separated items, given value
factories for items of type a and of type b. -}
spacePairValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory (a, b)
spacePairValueFactory = pairValueFactory " "


{- Converts a triple `(a, b, c)` to a `Value`, given a separator string and value
factories for items of type a, of type b, and of type c. -}
tripleValueFactory : String -> ValueFactory a -> ValueFactory b -> ValueFactory c -> ValueFactory (a, b, c)
tripleValueFactory separator firstInnerFactory secondInnerFactory thirdInnerFactory =
  { value (x,y,z) =
        [ firstInnerFactory.value x, secondInnerFactory.value y, thirdInnerFactory.value z ]
        |> intersperse separator
  }


{- Converts a triple `(a, b, c)` to a `Value` of space-separated items, given 
value factories for items of type a, of type b, and of type c. -}
spaceTripleValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory c -> ValueFactory (a, b, c)
spaceTripleValueFactory = tripleValueFactory " "


{- Converts a quadruple `(a, b, c, d)` to a `Value`, given a separator string and 
value factories for items of type a, of type b, of type c, and of type d. -}
quadrupleValueFactory : String -> ValueFactory a -> ValueFactory b -> ValueFactory c ->
                          ValueFactory d -> ValueFactory (a, b, c, d)
quadrupleValueFactory separator firstInnerFactory secondInnerFactory thirdInnerFactory fourthInnerFactory =
  { value (x,y,z,a) =
        [ firstInnerFactory.value x, secondInnerFactory.value y, thirdInnerFactory.value z, fourthInnerFactory.value a ]
        |> intersperse separator
  }


{- Converts a quadruple `(a, b, c, d)` to a `Value` of space-separated items, 
given value factories for items of type a, of type b, of type c, and of type d. -}
spaceQuadrupleValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory c ->
                                ValueFactory d -> ValueFactory (a, b, c, d)
spaceQuadrupleValueFactory = quadrupleValueFactory " "


{- Converts a quadruple `(a, b, c, d)` to a `Value` of comma-separated items, 
given value factories for items of type a, of type b, of type c, and of type d. -}
commaQuadrupleValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory c ->
                                ValueFactory d -> ValueFactory (a, b, c, d)
commaQuadrupleValueFactory = quadrupleValueFactory ","


{- Converts an `Either a b` to a `Value`, given value factories for items of 
type a and of type b. -}
eitherValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory (Either a b)
eitherValueFactory leftInnerFactory rightInnerFactory =
  { value x =
      case x of
        Left a -> leftInnerFactory.value a
        Right a -> rightInnerFactory.value a
  }
