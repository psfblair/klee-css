module Css.Selector (
  Selector (..), Refinement (..), Path (..), Predicate (..)
  , emptySelector, sortPredicate, star, deep, child, adjacent, with, byId, byClass
  , pseudo, func, withAttr, withAttrValue, withAttrValueBeginning, withAttrValueEnding
  , withAttrValueEnding, withAttrValueContaining, withAttrValueInSpacedList
  , withAttrValueInHyphenatedList, text, filterFromString
  ) where

import String exposing (uncons)

type Path
  = Star
  | Elem      String
  | Child     Selector Selector
  | Deep      Selector Selector
  | Adjacent  Selector Selector
  | Combined  Selector Selector

type Selector = Selector Refinement Path
-- Haskell uses overloaded strings to allow obtaining a Selector from a string.
-- We can't do that.

emptySelector : Selector
emptySelector = text ""

text : String -> Selector
text str =
  case String.uncons str of
    Just ('#', idPart) -> Selector (Refinement [Id idPart]) Star
    Just ('.', classPart) -> Selector (Refinement [Class classPart]) Star
    _ -> Selector (Refinement []) (Elem str)

combine : Selector -> Selector -> Selector
combine selector1 selector2 = Selector (Refinement []) (Combined selector1 selector2)

-------------------------------------------------------------------------------
type Predicate
  = Id           String
  | Class        String
  | Attr         String
  | AttrVal      String String
  | AttrBegins   String String
  | AttrEnds     String String
  | AttrContains String String
  | AttrSpace    String String
  | AttrHyph     String String
  | Pseudo       String
  | PseudoFunc   String (List String)

sortPredicate : Predicate -> Predicate -> Order
sortPredicate pred1 pred2 =
  let rank pred =
    case pred of
      Id              _ -> 1
      Class           _ -> 2
      Attr            _ -> 3
      AttrVal       _ _ -> 4
      AttrBegins    _ _ -> 5
      AttrEnds      _ _ -> 6
      AttrContains  _ _ -> 7
      AttrSpace     _ _ -> 8
      AttrHyph      _ _ -> 9
      Pseudo          _ -> 10
      PseudoFunc    _ _ -> 11
  in compare (rank pred1) (rank pred2)

type Refinement = Refinement (List Predicate)
-- Haskell uses overloaded strings to allow creating a Refinement from a String.
-- We can't do that.

filterFromString : String -> Refinement
filterFromString str = Refinement <|
  case String.uncons str of
    Just ('#', s) -> [Id     s]
    Just ('.', s) -> [Class  s]
    Just (':', s) -> [Pseudo s]
    Just ('@', s) -> [Attr   s]
    _             -> [Attr str]

-------------------------------------------------------------------------------
-- ** Element selectors

{-| The star selector applies to all elements. Maps to @*@ in CSS.
-}
star : Selector
star = Selector (Refinement []) Star

-------------------------------------------------------------------------------
-- ** Composing and refining selectors

{-| The deep selector composer. Maps to @sel1 sel2@ in CSS.
-}
deep : Selector -> Selector -> Selector
deep selector1 selector2 =
  Selector (Refinement []) (Deep selector1 selector2)

{-| The child selector composer. Maps to @sel1 > sel2@ in CSS.
-}
child : Selector -> Selector -> Selector
child selector1 selector2 =
  Selector (Refinement []) (Child selector1 selector2)

{-| The adjacent selector composer. Maps to @sel1 + sel2@ in CSS.
-}
adjacent : Selector -> Selector -> Selector
adjacent selector1 selector2 =
  Selector (Refinement []) (Adjacent selector1 selector2)

{-| The filter selector composer, which adds a filter to a selector. Maps to
something like @sel#filter@ or @sel.filter@ in CSS, depending on the filter.
-}
with : Selector -> Refinement -> Selector
with (Selector (Refinement filters) path) (Refinement moreFilters) =
  Selector (Refinement (filters ++ moreFilters)) path

{-| Given an id and a selector, add an id filter to the selector.
-}
byId : String -> Selector -> Selector
byId idString sel =  Refinement [ Id idString ] |> with sel

{-| Given a class name and a selector, add a class filter to the selector.
-}
byClass : String -> Selector -> Selector
byClass className sel = Refinement [ Class className ] |>  with sel

{-| Filter elements of the given selector by pseudo selector or pseudo class.
The preferred syntax is to use one of the predefined pseudo selectors from "Css.Pseudo".
-}
pseudo : String -> Selector -> Selector
pseudo pseudoSelector outerSelector =
  Refinement [ Pseudo pseudoSelector ] |> with outerSelector

{-| Filter elements of the given selector by pseudo selector functions. The
preferred syntax is to use one of the predefined functions from "Css.Pseudo".
-}
func : String -> (List String) -> Selector -> Selector
func pseudoFunc args outerSelector =
  Refinement [ PseudoFunc pseudoFunc args ] |> with outerSelector

-- ** Attribute-based refining.

{-| Given an attribute name and a selector, filter elements based on the presence
of that attribute. The preferred syntax is use one of the predefined attribute
Refinements from "Css.Attributes".
-}
withAttr : String -> Selector -> Selector
withAttr attrName outerSelector =
  Refinement [ Attr attrName ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute with the
specified value.
-}
withAttrValue : String -> String -> Selector -> Selector
withAttrValue attrName attrValue outerSelector =
  Refinement [ AttrVal attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that begins
with the specified value.
-}
withAttrValueBeginning : String -> String -> Selector -> Selector
withAttrValueBeginning attrName attrValue outerSelector =
  Refinement [ AttrBegins attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that ends
with the specified value.
-}
withAttrValueEnding : String -> String -> Selector -> Selector
withAttrValueEnding attrName attrValue outerSelector =
  Refinement [ AttrEnds attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that contains
the specified value as a substring.
-}
withAttrValueContaining : String -> String -> Selector -> Selector
withAttrValueContaining attrName attrValue outerSelector =
  Refinement [ AttrContains attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a space separated list.
-}
withAttrValueInSpacedList : String -> String -> Selector -> Selector
withAttrValueInSpacedList attrName attrValue outerSelector =
  Refinement [ AttrSpace attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a hyphen separated list.
-}
withAttrValueInHyphenatedList : String -> String -> Selector -> Selector
withAttrValueInHyphenatedList attrName attrValue outerSelector =
  Refinement [ AttrHyph attrName attrValue ] |> with outerSelector
