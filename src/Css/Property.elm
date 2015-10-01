module Css.Property where

type Prefixed
     = WithPrefix (List (String, String))
     | Plain String

fromString : String -> Prefixed
fromString str = Plain str
                 
unPlain : Prefixed -> String
unPlain prefixed =
  case prefixed of
    Plain str -> str
                 
unPrefixed : Prefixed -> List (String, String)
unPrefixed prefixed =
  case prefixed of
    WithPrefix xs -> xs

plain : Prefixed -> String
plain prefixed =
  case prefixed of
    WithPrefix xs -> "" `fromMaybe` lookup "" xs
    Plain str -> str

quote : String -> String
quote str = "\"" ++ (replace quote in string with escape) + "\""

merge : Prefixed -> Prefixed -> Prefixed
merge prefixed1 prefixed2 =
  case (prefixed1, prefixed2) of
    ((Plain       x), (Plain       y)) -> Plain (x ++ y)
    ((Plain       x), (WithPrefix ys)) -> ys |> List.map (\(k, y) -> (k, x ++ y)) |> WithPrefix
    ((WithPrefix xs), (Plain       y)) -> xs |> List.map (\(k, x) -> (k, x ++ y)) |> WithPrefix
    ((WithPrefix xs), (WithPrefix ys)) ->
      let kxs = List.map fst xs
          kys = List.map fst ys
          xsWithKeysInKys = List.partition (fst >> (\x -> List.member x kys)) xs
                                       |> fst
                                       |> List.sort
          ysWithKeysInKxs = List.partition (fst >> (\y -> List.member y kxs)) ys
                                       |> fst
                                       |> List.sort
      in List.map2 (\(p, a) (_, b) -> (p, a ++ b)) xsWithKeysInKys ysWithKeysInKxs
                                      |> WithPrefix

type Key a = Key a
           
unKeys : Key a -> Prefixed
unKeys key =
  case Key val -> val

-- Huh?
cast : Key a -> Key ()
cast (Key k) = Key k

type Value = Value Prefixed
           
type Literal = Literal String

type alias ValueWrappable a =
  { _obj: a
  , _value : a -> Value
  }

value : ValueWrappable a -> Value
value val = val._value val._obj

valueWrappableString : String -> ValueWrappable String
valueWrappableString str =
  { _obj = str, _value = \x -> Value (Plain x) }

valueWrappableLiteral : Literal -> ValueWrappable Literal
valueWrappableLiteral literal =
  { _obj = literal, _value = \x -> quote x |> Plain |> Value }

valueWrappableInteger : Integer -> ValueWrappable Integer
valueWrappableInteger int =
  { _obj = int, _value = \x -> toString x |> Value }

valueWrappableFloat : Float -> ValueWrappable Float
valueWrappableFloat num =
  { _obj = num, _value = \x -> showFixed x |> toString |> Value } -- TODO Data.Fixed to resolution of 100000

valueWrappableValue : Value -> ValueWrappable Value
valueWrappableValue val =
  { _obj = val, _value = \x -> x }

valueWrappableMaybe : Maybe a -> ValueWrappable Maybe a
valueWrappableMaybe maybe =
  { _obj = maybe
  , _value = \x ->
      case x of
        Just a -> value (wrap a)  -- TODO How to get this polymorphism?
        Nothing -> Value ""
  }

valueWrappablePair : (a, b) -> ValueWrappable (a, b)
valueWrappablePair pair =
  { _obj = pair
  , _value \(x,y) -> value (wrap x) ++ " " ++ value (wrap y) -- TODO How to get this polymorphism?
  }

valueWrappableEither : Either a b -> ValueWrappable Either a b
valueWrappableEither : either =
  { _obj = either
  , _value \x ->   -- TODO How to get this polymorphism?
      case x of
        Left a -> value (wrap a)
        Right a -> value (wrap a)
  }

valueWrappableList : List a -> ValueWrappable List a
valueWrappableList list =
  { _obj = list
  , _value = list |> List.map wrap |> List.map value |> intersperse "," -- TODO polymorphism
  }


intersperse :: Monoid a => a -> [a] -> a -- TODO polymorphism
intersperse _ []     = mempty
intersperse s (x:xs) = foldl (\a b -> a <> s <> b) x xs

noCommas : List a -> Value
noCommas xs = intersperse " " (map value xs)

-- right-associative tupling operator
(!) : a -> b -> (a, b)
(!) = (,)
