module Css.Selector where

import String (uncons)

type Fix f = In SelectorF f
-- TODO out to dig the embedded value out?
-- { out : f (Fix f) }
-- instance Show (f (Fix f)) => Show (Fix f)

type Path f
  = Star
  | Elem      String
  | Child     f f
  | Deep      f f
  | Adjacent  f f
  | Combined  f f

type SelectorF a = SelectorF Refinement (Path a)

type Selector = Fix SelectorF
-- Haskell uses overloaded strings to allow obtaining a Selector from a string.
-- We can't do that.

emptySelector : Selector
emptySelector = text ""

text : String -> Selector
text str = In <|
  case String.uncons str of
    Just ('#', idPart) -> SelectorF (Refinement [Id idPart]) Star
    Just ('.', classPart) -> SelectorF (Refinement [Class classPart]) Star
    _ -> SelectorF (Refinement []) (Elem str)

combine : Selector -> Selector -> Selector
combine selector1 selector2 = In (SelectorF (Refinement []) (Combined a b))

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

type Refinement = Refinement List Predicate
-- Haskell uses overloaded strings to allow creating a Refinement from a String.
-- We can't do that.
-- TODO unFilter to dig the embedded value out?

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
star = In (SelectorF (Refinement []) Star)

{-| Select elements by name. The preferred syntax is to just use one of
the predefined elements from "Css.Elements".
-}
element : String -> Selector
element name = In (SelectorF (Refinement []) (Elem name))

-------------------------------------------------------------------------------
-- ** Composing and refining selectors

{-| The deep selector composer. Maps to @sel1 sel2@ in CSS.
-}
deep : Selector -> Selector -> Selector
deep selector1 selector2 =
  In (SelectorF (Refinement []) (Deep selector1 selector2))

{-| The child selector composer. Maps to @sel1 > sel2@ in CSS.
-}
child : Selector -> Selector -> Selector
child selector1 selector2 =
  In (SelectorF (Refinement []) (Child selector1 selector2))

{-| The adjacent selector composer. Maps to @sel1 + sel2@ in CSS.
-}
adjacent : Selector -> Selector -> Selector
adjacent selector1 selector2 =
  In (SelectorF (Refinement []) (Adjacent selector1 selector2))

{-| The filter selector composer, which adds a filter to a selector. Maps to
something like @sel#filter@ or @sel.filter@ in CSS, depending on the filter.
-}
with : Selector -> Refinement -> Selector
with (In (SelectorF (Refinement filters) path)) (Refinement moreFilters) =
  In (SelectorF (Refinement (filters ++ moreFilters)) path)

{-| Given an id and a selector, add an id filter to the selector.
-}
byId : String -> Selector -> Selector
byId idString sel = [ Id idString ] |> Refinement |> (sel with)

{-| Given a class name and a selector, add a class filter to the selector.
-}
byClass : String -> Selector -> Selector
byClass className sel = [ Class className ] |> Refinement |> (sel with)

{-| Filter elements of the given selector by pseudo selector or pseudo class.
The preferred syntax is to use one of the predefined pseudo selectors from "Css.Pseudo".
-}
pseudo : String -> Selector -> Selector
pseudo pseudoSelector outerSelector =
  [ Pseudo pseudoSelector ] |> Refinement |> (outerSelector with)

{-| Filter elements of the given selector by pseudo selector functions. The
preferred syntax is to use one of the predefined functions from "Css.Pseudo".
-}
func : String -> (List String) -> Selector -> Selector
func pseudoFunc args outerSelector =
  [ PseudoFunc pseudoFunc args ] |> Refinement |> (outerSelector with)

-- ** Attribute-based refining.

{-| Given an attribute name and a selector, filter elements based on the presence
of that attribute. The preferred syntax is use one of the predefined attribute
Refinements from "Css.Attributes".
-}
withAttr : String -> Selector -> Selector
withAttr attrName outerSelector =
  [ Attr attrName ] |> Refinement |> (outerSelector with)

{-| Filter elements based on the presence of a certain attribute with the
specified value.
-}
withAttrValue : String -> String -> Selector -> Selector
withAttrValue attrName attrValue outerSelector =
  [ AttrVal attrName attrValue ] |> Refinement |> (outerSelector with)

{-| Filter elements based on the presence of a certain attribute that begins
with the specified value.
-}
withAttrValueBeginning : String -> String -> Selector -> Selector
withAttrValueBeginning attrName attrValue outerSelector =
  [ AttrBegins attrName attrValue ] |> Refinement |> (outerSelector with)

{-| Filter elements based on the presence of a certain attribute that ends
with the specified value.
-}
withAttrValueEnding : String -> String -> Selector -> Selector
withAttrValueEnding attrName attrValue outerSelector =
  [ AttrEnds attrName attrValue ] |> Refinement |> (outerSelector with)

{-| Filter elements based on the presence of a certain attribute that contains
the specified value as a substring.
-}
withAttrValueContaining : String -> String -> Selector -> Selector
withAttrValueContaining attrName attrValue outerSelector =
  [ AttrContains attrName attrValue ] |> Refinement |> (outerSelector with)

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a space separated list.
-}
withAttrValueInSpacedList : String -> String -> Selector -> Selector
withAttrValueInSpacedList attrName attrValue outerSelector =
  [ AttrSpace attrName attrValue ] |> Refinement |> (outerSelector with)

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a hyphen separated list.
-}
withAttrValueInHyphenatedList : String -> String -> Selector -> Selector
withAttrValueInHyphenatedList attrName attrValue outerSelector =
  [ AttrHyph attrName attrValue ] |> Refinement |> (outerSelector with)
