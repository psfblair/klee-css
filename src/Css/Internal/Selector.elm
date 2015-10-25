module Css.Internal.Selector (
    SelectorData (..), Refinement (..), Path (..), Predicate (..)
  , selectorDataFromString, emptySelectorData, star, sortPredicate, filterFromString
  ) where

import String exposing (uncons)

type SelectorData = SelectorData Refinement Path

type Path
  = Star
  | Elem       String
  | Child      SelectorData SelectorData
  | Descendant SelectorData SelectorData
  | Adjacent   SelectorData SelectorData
  | Combined   SelectorData SelectorData

selectorDataFromString : String -> SelectorData
selectorDataFromString str =
  case String.uncons str of
    Just ('#', idPart) -> SelectorData (Refinement [Id idPart]) Star
    Just ('.', classPart) -> SelectorData (Refinement [Class classPart]) Star
    _ -> SelectorData (Refinement []) (Elem str)

-- Used only as a null case in rendering.
emptySelectorData : SelectorData
emptySelectorData = selectorDataFromString ""

-- The star selector applies to all vals.
-- This is used by `star` in `Css.vals` which is what clients should use.
star : SelectorData
star = SelectorData (Refinement []) Star

-------------------------------------------------------------------------------

type Refinement = Refinement (List Predicate)

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
