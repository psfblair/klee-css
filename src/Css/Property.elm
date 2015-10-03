module Css.Property where

import Dict exposing (fromList, get)
import Regex exposing (regex, replace)

type Literal = Literal String

type Either a b = Left a | Right b

isRight : Either a b -> Bool
isRight either =
  case either of
    (Right _) -> True
    _ -> False

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

type Key a = Key PrefixedOrNot

unKeys : Key PrefixedOrNot -> PrefixedOrNot
unKeys (Key a) = a

cast : Key a -> Key ()
cast (Key k) = Key k

type Value = Value PrefixedOrNot

unValue : Value -> PrefixedOrNot
unValue (Value v) = v

emptyValue : Value
emptyValue = Value (Plain "")

appendValues : Value -> Value -> Value
appendValues (Value v1) (Value v2) = merge v1 v2 |> Value

intersperse : String -> List Value -> Value
intersperse str values =
  let separatorValue = Plain str |> Value
      interspersed = List.intersperse separatorValue values
  in List.foldl (\val accum -> appendValues val accum) emptyValue interspersed

type alias ValueWrapper a = { value : a -> Value }

stringValueWrapper : ValueWrapper String
stringValueWrapper = { value = \x -> Plain x |> Value}

literalValueWrapper : ValueWrapper Literal
literalValueWrapper = { value = \(Literal x) -> quote x |> Plain |> Value }

intValueWrapper : ValueWrapper Int
intValueWrapper = { value = \x -> toString x |> Plain |> Value }

showFixed : Int -> Float -> String
showFixed resolution number =
  let resolutionF = toFloat resolution
  in number * resolutionF
      |> round
      |> toFloat
      |> (\x -> x / resolutionF)
      |> toString

floatValueWrapper : ValueWrapper Float
floatValueWrapper = { value = \x -> showFixed 100000 x |> Plain |> Value }

maybeValueWrapper : ValueWrapper a -> ValueWrapper (Maybe a)
maybeValueWrapper innerWrapper =
  { value = \x ->
      case x of
        Just val -> innerWrapper.value val
        Nothing -> emptyValue
  }

listValueWrapper : String -> ValueWrapper a -> ValueWrapper (List a)
listValueWrapper separator innerWrapper =
  let wrapList xs =
    case xs of
      [] -> []
      (h :: t) -> (innerWrapper.value h) :: (wrapList t)
  in { value = wrapList >> intersperse separator }

commaListValueWrapper : ValueWrapper a -> ValueWrapper (List a)
commaListValueWrapper = listValueWrapper ","

noCommasListValueWrapper : ValueWrapper a -> ValueWrapper (List a)
noCommasListValueWrapper = listValueWrapper " "

pairValueWrapper : ValueWrapper a -> ValueWrapper b -> ValueWrapper (a, b)
pairValueWrapper firstInnerWrapper secondInnerWrapper =
  { value =
      \(x,y) ->
        [firstInnerWrapper.value x, secondInnerWrapper.value y]
        |> intersperse " "
  }

eitherValueWrapper : ValueWrapper a -> ValueWrapper b -> ValueWrapper (Either a b)
eitherValueWrapper leftInnerWrapper rightInnerWrapper =
  { value = \x ->
      case x of
        Left a -> leftInnerWrapper.value a
        Right a -> rightInnerWrapper.value a
  }

-- TODO right-associative tupling operator -- why?
(!) : a -> b -> (a, b)
(!) = (,)
