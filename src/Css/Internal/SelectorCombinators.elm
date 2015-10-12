module Css.Internal.SelectorCombinators (
    Selector, element, descendant, child, sibling, with, byId, byClass
  , pseudo, func, withAttr, withAttrValue, withAttrValueBeginning, withAttrValueEnding
  , withAttrValueEnding, withAttrValueContaining, withAttrValueInSpacedList
  , withAttrValueInHyphenatedList
  -- used by other modules
  , createSelector
  ) where

import Css.Internal.Selector exposing
  ( SelectorData (..), Refinement (..), Path (..), Predicate (..)
  , selectorDataFromString, filterFromString
  )
import Css.Internal.Stylesheet exposing
  ( CssAppender, PropertyRuleAppender, SelectorRuleAppender, RuleData (..)
  , addRule, extractRuleData
  )

{-
The DSL's concept of a selector is a function that takes a list of a certain
subtype of `CssGenerator`, namely, functions that when given a `CssRule` add
a `RuleTree` and returns `CssRule`. The selector function returns a function
that when given a `CssRule` adds a `NestedRule` to it and returns that.

We would like to parameterize the types of the functions CssGenerator -> CssGenerator
without wrapping them.
`List (RuleTree -> CssRule) -> (NestedRule -> CssRule)` where `RuleTree`
and `NestedRule` are records made to subtype `CssRule` -- `RuleTree` has
a `ruleTree` field which is intended to take rules only using the constructors
`Property` or `Nested`, and `NestedRule` has an additional `selector` field
and is intended to take only rules using the `Nested` constructor.
You can pass a `NestedRule` any time you need a `RuleTree`, because it has
a `ruleTree` field on it.

These constraints are not enforced by the type system, but by construction; however,
they enforce type constraints in the DSL itself by allowing Selectors to be
combined only with other Selectors and not with MediaQuery or other rules.
Since `RuleTree` and `NestedRule` both contain a `rule` field, they can be
used wherever `CssRule` is used, and so a `Selector` is also of type
`List (CssRule -> CssRule) -> (CssRule -> CssRule)`, i.e., of type
`List CssGenerator -> CssGenerator`.
-}
-- A selector is a function that takes a List of RuleTree to CssRule functions
-- and when given a CssRule adds a NestedRule to it and returns that.
type alias Selector = List PropertyRuleAppender -> SelectorRuleAppender

-------------------------------------------------------------------------------
-- ** Creating selectors

{-| Create a new selector by name. The preferred syntax is to
just use one of the predefined elements from "Css.Elements".
-}
element : String -> Selector
element name = selectorDataFromString name |> createSelector

{-| Create a new filter by name. The preferred syntax is to
use `byId` and `byClass` to filter with ids and classes, to use one of the
predefined attributes from "Css.Attributes" for attributes, or one
of the predefined pseudo-classes and pseudo functions from `Css.Pseudo`.
-}
filter : String -> Refinement
filter name = filterFromString name
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ** Composing and refining selectors.

-- Maps to `sel1, sel2` in CSS
combine : Selector -> Selector -> Selector
combine selector1 selector2 = combineSelectors selector1 selector2 Combined

{-| The descendant selector composer. Maps to `sel1 sel2` in CSS.
-}
descendant : Selector -> Selector -> Selector
descendant selector1 selector2 = combineSelectors selector1 selector2 Descendant

{-| The child selector composer. Maps to `sel1 > sel2` in CSS.
-}
child : Selector -> Selector -> Selector
child selector1 selector2 = combineSelectors selector1 selector2 Css.Internal.Selector.Child

{-| The next-sibling selector composer. Maps to `sel1 + sel2` in CSS.
-}
sibling : Selector -> Selector -> Selector
sibling selector1 selector2 = combineSelectors selector1 selector2 Adjacent

{-| The filter selector composer, which adds a filter to a selector. Maps to
something like `sel#filter` or `sel.filter` in CSS, depending on the filter.
-}
with : Selector -> Refinement -> Selector
with selector refinement = addRefinementToSelector selector refinement

{-| Given an id and a selector, add an id filter to the selector.
-}
byId : Selector -> String -> Selector
byId sel idString =  Refinement [ Id idString ] |> with sel

{-| Given a class name and a selector, add a class filter to the selector.
-}
byClass : Selector -> String -> Selector
byClass sel className = Refinement [ Class className ] |>  with sel

{-| Filter elements of the given selector by pseudo selector or pseudo class.
The preferred syntax is to use one of the predefined pseudo selectors from "Css.Pseudo".
-}
pseudo : Selector -> String -> Selector
pseudo outerSelector pseudoSelector =
  Refinement [ Pseudo pseudoSelector ] |> with outerSelector

{-| Filter elements of the given selector by pseudo selector functions. The
preferred syntax is to use one of the predefined functions from "Css.Pseudo".
-}
func : Selector -> String -> (List String) -> Selector
func outerSelector pseudoFunc args =
  Refinement [ PseudoFunc pseudoFunc args ] |> with outerSelector

-- ** Attribute-based refining.

{-| Given an attribute name and a selector, filter elements based on the presence
of that attribute. The preferred syntax is use one of the predefined attribute
Refinements from "Css.Attributes".
-}
withAttr : Selector -> String -> Selector
withAttr outerSelector attrName =
  Refinement [ Attr attrName ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute with the
specified value.
-}
withAttrValue : Selector -> String -> String -> Selector
withAttrValue outerSelector attrName attrValue =
  Refinement [ AttrVal attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that begins
with the specified value.
-}
withAttrValueBeginning : Selector -> String -> String -> Selector
withAttrValueBeginning outerSelector attrName attrValue =
  Refinement [ AttrBegins attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that ends
with the specified value.
-}
withAttrValueEnding : Selector -> String -> String -> Selector
withAttrValueEnding outerSelector attrName attrValue =
  Refinement [ AttrEnds attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that contains
the specified value as a substring.
-}
withAttrValueContaining : Selector -> String -> String -> Selector
withAttrValueContaining outerSelector attrName attrValue =
  Refinement [ AttrContains attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a space separated list.
-}
withAttrValueInSpacedList : Selector -> String -> String -> Selector
withAttrValueInSpacedList outerSelector attrName attrValue =
  Refinement [ AttrSpace attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a hyphen separated list.
-}
withAttrValueInHyphenatedList : Selector -> String -> String -> Selector
withAttrValueInHyphenatedList outerSelector attrName attrValue =
  Refinement [ AttrHyph attrName attrValue ] |> with outerSelector

-------------------------------------------------------------------------------
-- * Ancillary functions used for implementation.

-- ** Assigning rules to selectors

{- Create a selector from a SelectorData. When the selector is nested
inside an outer scope it will be composed with `descendant`, which maps to
`sel1 sel2` in CSS. This function is not exposed by the outer Css module; it is
intended to be called only when creating the various embedded element functions;
the `element` function in `Css.Element` also calls it when creating custom
selectors.

Note that `List PropertyRuleAppender -> SelectorRuleAppender` is the same as
`Selector`, but we leave it expanded for clarity.
-}
createSelector : SelectorData -> List PropertyRuleAppender -> SelectorRuleAppender
createSelector selectorData propertyRuleAppenders =
  let nestedRule = Nested selectorData (extractRuleData propertyRuleAppenders)
  in { addCss = addRule nestedRule
     , propertyRule = nestedRule
     , selector = selectorData
     }

-- ** Combining selectors

type alias BinaryPathConstructor = SelectorData -> SelectorData -> Path

{- Implementation notes: Combining two selectors means that the paths of the two
selectors will be combined using the `BinaryPathConstructor` and the combination
will be applied to the same `List PropertyRuleAppender` to return a
`SelectorRuleAppender.` Since `Selector` is the same as
`List PropertyRuleAppender -> SelectorRuleAppender`, this function could
also be seen as a selector combinator with signature
`Selector -> Selector -> BinaryPathConstructor -> Selector`, but expanding it
makes the implementation clearer.
-}
combineSelectors : Selector ->
                   Selector ->
                   BinaryPathConstructor ->
                   List PropertyRuleAppender ->
                   SelectorRuleAppender
combineSelectors selector1 selector2 pathConstructor propertyRuleAppenders =
  -- each selector is a List PropertyRuleAppender -> SelectorRuleAppender
  let selector1Appender = selector1 []
      selector1Data = selector1Appender.selector
      selector2Appender = selector2 []
      selector2Data = selector2Appender.selector
      combinedSelectorData =
        SelectorData (Refinement []) (pathConstructor selector1Data selector2Data)
      combinedRule =
        Nested combinedSelectorData (extractRuleData propertyRuleAppenders)
  in { addCss = addRule combinedRule
     , propertyRule = combinedRule
     , selector = combinedSelectorData
     }


-- ** Refining selectors

{- Implementation notes:  Since `Selector` is the same as
`List PropertyRuleAppender -> SelectorRuleAppender`, this function has the same
signature as `with` above (which just calls it), but expanding it makes the
implementation clearer.
-}
addRefinementToSelector : Selector ->
                          Refinement ->
                          List PropertyRuleAppender ->
                          SelectorRuleAppender
addRefinementToSelector sel (Refinement filtersToAdd) propertyRuleAppenders =
  -- each selector is a List PropertyRuleAppender -> SelectorRuleAppender
  let selectorAppender = sel []
      selectorData = selectorAppender.selector
      (selectorFilters, selectorPath) = case selectorData of
        SelectorData (Refinement filters) path -> (filters, path)
      newSelectorData =
        SelectorData (Refinement (selectorFilters ++ filtersToAdd)) selectorPath
      refinedRule =
        Nested newSelectorData (extractRuleData propertyRuleAppenders)
  in { addCss = addRule refinedRule
     , propertyRule = refinedRule
     , selector = newSelectorData
     }
