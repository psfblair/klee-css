module Css.Internal.Property
  ( Key (..), Value (..), PrefixedOrNot (..)
  , unPrefixed, plain, quote, stringKey, prefixedKeys, cast
  , ValueFactory, emptyValue, appendUnits, concatenateValues
  , stringValueFactory, intValueFactory, floatValueFactory, valueValueFactory
  , maybeValueFactory, commaListValueFactory, spaceListValueFactory, spacePairValueFactory
  , spaceTripleValueFactory, spaceQuadrupleValueFactory, commaQuadrupleValueFactory
  , eitherValueFactory
  ) where

import Dict exposing (fromList, get)
import Regex exposing (regex, replace)

import Css.Internal.Utils exposing (Either (..), rightValue, toFixed)

-------------------------------------------------------------------------------

type Literal = Literal String

-------------------------------------------------------------------------------
-- Prefixed is for properties with browser prefixes.
type PrefixedOrNot
     = Prefixed (List (String, String))
     | Plain String

unPrefixed : PrefixedOrNot -> List (String, String)
unPrefixed (Prefixed inner) = inner

unPlain : PrefixedOrNot -> String
unPlain (Plain str) = str

plain : PrefixedOrNot -> String
plain prefixedOrNot =
  case prefixedOrNot of
    Prefixed xs -> Dict.fromList xs |> Dict.get "" |> Maybe.withDefault ""
    Plain str -> str

quote : String -> String
quote str =
   let escaped = str |> replace Regex.All (regex "\"") (\_ -> "\\\"")
   in "\"" ++ escaped ++ "\""

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

type Key a = Key PrefixedOrNot

stringKey : String -> Key a
stringKey str = Plain str |> Key

prefixedKeys : PrefixedOrNot -> String -> PrefixedOrNot
prefixedKeys prefixes rootKey = Plain rootKey |> merge prefixes

unKeys : Key PrefixedOrNot -> PrefixedOrNot
unKeys (Key a) = a

cast : Key a -> Key ()
cast (Key k) = Key k

-------------------------------------------------------------------------------

type Value = Value PrefixedOrNot

unValue : Value -> PrefixedOrNot
unValue (Value v) = v

emptyValue : Value
emptyValue = Value (Plain "")

appendValues : Value -> Value -> Value
appendValues (Value v1) (Value v2) = merge v1 v2 |> Value

concatenateValues : List Value -> Value
concatenateValues = List.foldr (\val accum -> appendValues val accum) emptyValue

appendUnits : Float -> String -> Value
appendUnits qty unit =
  appendValues (floatValueFactory.value qty) (stringValueFactory.value unit)

intersperse : String -> List Value -> Value
intersperse str values =
  let separatorValue = Plain str |> Value
      interspersed = List.intersperse separatorValue values
  in List.foldr (\val accum -> appendValues val accum) emptyValue interspersed

-------------------------------------------------------------------------------

type alias ValueFactory a = { value : a -> Value }

stringValueFactory : ValueFactory String
stringValueFactory = { value x = Plain x |> Value}

literalValueFactory : ValueFactory Literal
literalValueFactory = { value (Literal x) = quote x |> Plain |> Value }

intValueFactory : ValueFactory Int
intValueFactory = { value x = toString x |> Plain |> Value }

floatValueFactory : ValueFactory Float
floatValueFactory = { value x = toFixed 5 x |> toString |> Plain |> Value }

valueValueFactory : ValueFactory Value
valueValueFactory = { value = identity }

maybeValueFactory : ValueFactory a -> ValueFactory (Maybe a)
maybeValueFactory innerFactory =
  { value x =
      case x of
        Just val -> innerFactory.value val
        Nothing -> emptyValue
  }

listValueFactory : String -> ValueFactory a -> ValueFactory (List a)
listValueFactory separator innerFactory =
  let wrapList xs =
    case xs of
      [] -> []
      (h :: t) -> (innerFactory.value h) :: (wrapList t)
  in { value = wrapList >> intersperse separator }

commaListValueFactory : ValueFactory a -> ValueFactory (List a)
commaListValueFactory = listValueFactory ","

spaceListValueFactory : ValueFactory a -> ValueFactory (List a)
spaceListValueFactory = listValueFactory " "

pairValueFactory : String -> ValueFactory a -> ValueFactory b -> ValueFactory (a, b)
pairValueFactory separator firstInnerFactory secondInnerFactory =
  { value (x,y) =
        [ firstInnerFactory.value x, secondInnerFactory.value y ]
        |> intersperse separator
  }

spacePairValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory (a, b)
spacePairValueFactory = pairValueFactory " "

tripleValueFactory : String -> ValueFactory a -> ValueFactory b -> ValueFactory c -> ValueFactory (a, b, c)
tripleValueFactory separator firstInnerFactory secondInnerFactory thirdInnerFactory =
  { value (x,y,z) =
        [ firstInnerFactory.value x, secondInnerFactory.value y, thirdInnerFactory.value z ]
        |> intersperse separator
  }

spaceTripleValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory c -> ValueFactory (a, b, c)
spaceTripleValueFactory = tripleValueFactory " "

quadrupleValueFactory : String -> ValueFactory a -> ValueFactory b -> ValueFactory c ->
                          ValueFactory d -> ValueFactory (a, b, c, d)
quadrupleValueFactory separator firstInnerFactory secondInnerFactory thirdInnerFactory fourthInnerFactory =
  { value (x,y,z,a) =
        [ firstInnerFactory.value x, secondInnerFactory.value y, thirdInnerFactory.value z, fourthInnerFactory.value a ]
        |> intersperse separator
  }

spaceQuadrupleValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory c ->
                                ValueFactory d -> ValueFactory (a, b, c, d)
spaceQuadrupleValueFactory = quadrupleValueFactory " "

commaQuadrupleValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory c ->
                                ValueFactory d -> ValueFactory (a, b, c, d)
commaQuadrupleValueFactory = quadrupleValueFactory ","

eitherValueFactory : ValueFactory a -> ValueFactory b -> ValueFactory (Either a b)
eitherValueFactory leftInnerFactory rightInnerFactory =
  { value x =
      case x of
        Left a -> leftInnerFactory.value a
        Right a -> rightInnerFactory.value a
  }
