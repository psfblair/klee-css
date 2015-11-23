module Css.Internal.SelectorCombinators (
    Selector, element, filter, group, descendant, child, sibling, with, byId, byClass
  , pseudo, func, withAttr, withAttrValue, withAttrValueBeginning, withAttrValueEnding
  , withAttrValueEnding, withAttrValueContaining, withAttrValueInSpacedList
  , withAttrValueInHyphenatedList
  -- used by other modules
  , createSelector
  ) where

-- TODO - Stop importing SelectorData, Combined, Adjacent, Child, Descendant, Refinement, 
-- Id, Class, Pseudo, PseudoFunc, Attr, AttrXXX constructors from Selector (..)  
import Css.Internal.Selector as Selector
  
-- TODO - Stop importing Nested constructor from RuleData (..)  
import Css.Internal.Stylesheet as Stylesheet

{-|  A `Selector` represents the selector in a CSS rule. `Selector` is
implemented as a function that takes twp list of rules (one of `StyleProperty`
and one of nested `PropertyStylesheet` rules) and returns a `PropertyStylesheet`.
Selectors may be combined using the combinators defined in this module. Most of
the selectors one would need are predefined in the `Css.Elements` module, though
if necessary new custom selectors can be defined using the `element` function in
that module.
-}
type alias Selector = List Stylesheet.PropertyRuleAppender ->
                      List Stylesheet.SelectorRuleAppender ->
                      Stylesheet.SelectorRuleAppender

-------------------------------------------------------------------------------
-- ** Creating selectors

{-| Create a new selector by name. The preferred syntax is to
just use one of the predefined elements from "Css.Elements".
-}
element : String -> Selector
element name = Selector.selectorDataFromString name |> createSelector

{-| Create a new filter by name. The preferred syntax is to
use `byId` and `byClass` to filter with ids and classes, to use one of the
predefined attributes from "Css.Attributes" for attributes, or one
of the predefined pseudo-classes and pseudo functions from `Css.Pseudo`.
-}
filter : String -> Selector.Refinement
filter name = Selector.filterFromString name


-------------------------------------------------------------------------------
-- ** Composing and refining selectors.

{-| The group selector composer. aps to `sel1, sel2` in CSS.
-}
group : Selector -> Selector -> Selector
group selector1 selector2 = combineSelectors selector1 selector2 Selector.Combined

{-| The descendant selector composer. Maps to `sel1 sel2` in CSS.
-}
descendant : Selector -> Selector -> Selector
descendant selector1 selector2 = combineSelectors selector1 selector2 Selector.Descendant

{-| The child selector composer. Maps to `sel1 > sel2` in CSS.
-}
child : Selector -> Selector -> Selector
child selector1 selector2 = combineSelectors selector1 selector2 Selector.Child

{-| The next-sibling selector composer. Maps to `sel1 + sel2` in CSS.
-}
sibling : Selector -> Selector -> Selector
sibling selector1 selector2 = combineSelectors selector1 selector2 Selector.Adjacent

{-| The filter selector composer, which adds a filter to a selector. Maps to
something like `sel#filter` or `sel.filter` in CSS, depending on the filter.
-}
with : Selector -> Selector.Refinement -> Selector
with selector refinement = addRefinementToSelector selector refinement

{-| Given an id and a selector, add an id filter to the selector.
-}
byId : Selector -> String -> Selector
byId sel idString =  Selector.Refinement [ Selector.Id idString ] |> with sel

{-| Given a class name and a selector, add a class filter to the selector.
-}
byClass : Selector -> String -> Selector
byClass sel className = Selector.Refinement [ Selector.Class className ] |>  with sel

{-| Filter elements of the given selector by pseudo selector or pseudo class.
The preferred syntax is to use one of the predefined pseudo selectors from "Css.Pseudo".
-}
pseudo : Selector -> String -> Selector
pseudo outerSelector pseudoSelector =
  Selector.Refinement [ Selector.Pseudo pseudoSelector ] |> with outerSelector

{-| Filter elements of the given selector by pseudo selector functions. The
preferred syntax is to use one of the predefined functions from "Css.Pseudo".
-}
func : String -> (List String) -> Selector.Refinement
func pseudoFunc args =
  Selector.Refinement [ Selector.PseudoFunc pseudoFunc args ]

-- ** Attribute-based refining.

{-| Given an attribute name and a selector, filter elements based on the presence
of that attribute. The preferred syntax is use one of the predefined attribute
Refinements from "Css.Attributes".
-}
withAttr : Selector -> String -> Selector
withAttr outerSelector attrName =
  Selector.Refinement [ Selector.Attr attrName ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute with the
specified value.
-}
withAttrValue : Selector -> String -> String -> Selector
withAttrValue outerSelector attrName attrValue =
  Selector.Refinement [ Selector.AttrVal attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that begins
with the specified value.
-}
withAttrValueBeginning : Selector -> String -> String -> Selector
withAttrValueBeginning outerSelector attrName attrValue =
  Selector.Refinement [ Selector.AttrBegins attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that ends
with the specified value.
-}
withAttrValueEnding : Selector -> String -> String -> Selector
withAttrValueEnding outerSelector attrName attrValue =
  Selector.Refinement [ Selector.AttrEnds attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that contains
the specified value as a substring.
-}
withAttrValueContaining : Selector -> String -> String -> Selector
withAttrValueContaining outerSelector attrName attrValue =
  Selector.Refinement [ Selector.AttrContains attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a space separated list.
-}
withAttrValueInSpacedList : Selector -> String -> String -> Selector
withAttrValueInSpacedList outerSelector attrName attrValue =
  Selector.Refinement [ Selector.AttrSpace attrName attrValue ] |> with outerSelector

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a hyphen separated list.
-}
withAttrValueInHyphenatedList : Selector -> String -> String -> Selector
withAttrValueInHyphenatedList outerSelector attrName attrValue =
  Selector.Refinement [ Selector.AttrHyph attrName attrValue ] |> with outerSelector

-------------------------------------------------------------------------------
-- * Ancillary functions used for implementation.

-- ** Assigning rules to selectors

{- Create a selector from a SelectorData. When the selector is nested
inside an outer scope it will be composed with `descendant`, which maps to
`sel1 sel2` in CSS. This function is not exposed by the outer Css module; it is
intended to be called only when creating the various embedded element functions;
the `element` function in `Css.Element` also calls it when creating custom
selectors.

Note that `List Stylesheet.PropertyRuleAppender -> Stylesheet.SelectorRuleAppender` is the same as
`Selector`, but we leave it expanded for clarity.
-}
createSelector : Selector.SelectorData ->
                 List Stylesheet.PropertyRuleAppender ->
                 List Stylesheet.SelectorRuleAppender ->
                 Stylesheet.SelectorRuleAppender
createSelector selectorData propertyRuleAppenders nestedRuleAppenders =
  let propertyRules = Stylesheet.extractRuleData propertyRuleAppenders
      nestedRules = Stylesheet.extractRuleData nestedRuleAppenders
      nestedRule = Stylesheet.Nested selectorData (propertyRules ++ nestedRules)
  in { addCss = Stylesheet.addRule nestedRule
     , propertyRule = nestedRule
     , selector = selectorData
     }

-- ** Combining selectors

type alias BinaryPathConstructor = 
  Selector.SelectorData -> Selector.SelectorData -> Selector.Path

{- Implementation notes: Combining two selectors means that the paths of the two
selectors will be combined using the `BinaryPathConstructor` and the combination
will be applied to the same `List Stylesheet.PropertyRuleAppender` to return a
`Stylesheet.SelectorRuleAppender.` Since `Selector` is the same as
`List Stylesheet.PropertyRuleAppender -> Stylesheet.SelectorRuleAppender`, this function could
also be seen as a selector combinator with signature
`Selector -> Selector -> BinaryPathConstructor -> Selector`, but expanding it
makes the implementation clearer.
-}
combineSelectors : Selector ->
                   Selector ->
                   BinaryPathConstructor ->
                   List Stylesheet.PropertyRuleAppender ->
                   List Stylesheet.SelectorRuleAppender ->
                   Stylesheet.SelectorRuleAppender
combineSelectors selector1
                 selector2
                 pathConstructor
                 propertyRuleAppenders
                 nestedRuleAppenders =
  -- each selector is a List Stylesheet.PropertyRuleAppender -> Stylesheet.SelectorRuleAppender
  let selector1Appender = selector1 [] []
      selector1Data = selector1Appender.selector
      selector2Appender = selector2 [] []
      selector2Data = selector2Appender.selector
      combinedSelectorData =
        Selector.SelectorData (Selector.Refinement []) (pathConstructor selector1Data selector2Data)
      propertyRules = Stylesheet.extractRuleData propertyRuleAppenders
      nestedRules = Stylesheet.extractRuleData nestedRuleAppenders
      combinedRule =
        Stylesheet.Nested combinedSelectorData (propertyRules ++ nestedRules)
  in { addCss = Stylesheet.addRule combinedRule
     , propertyRule = combinedRule
     , selector = combinedSelectorData
     }


-- ** Refining selectors

{- Implementation notes:  Since `Selector` is the same as
`List Stylesheet.PropertyRuleAppender -> Stylesheet.SelectorRuleAppender`, this function has the same
signature as `with` above (which just calls it), but expanding it makes the
implementation clearer.
-}
addRefinementToSelector : Selector ->
                          Selector.Refinement ->
                          List Stylesheet.PropertyRuleAppender ->
                          List Stylesheet.SelectorRuleAppender ->
                          Stylesheet.SelectorRuleAppender
addRefinementToSelector sel
                        (Selector.Refinement filtersToAdd)
                        propertyRuleAppenders
                        nestedRuleAppenders =
  -- each selector is a List Stylesheet.PropertyRuleAppender -> Stylesheet.SelectorRuleAppender
  let selectorAppender = sel [] []
      selectorData = selectorAppender.selector
      (selectorFilters, selectorPath) = case selectorData of
        Selector.SelectorData (Selector.Refinement filters) path -> (filters, path)
      newSelectorData =
        Selector.SelectorData (Selector.Refinement (selectorFilters ++ filtersToAdd)) selectorPath
      propertyRules = Stylesheet.extractRuleData propertyRuleAppenders
      nestedRules = Stylesheet.extractRuleData nestedRuleAppenders
      refinedRule =
        Stylesheet.Nested newSelectorData (propertyRules ++ nestedRules)
  in { addCss = Stylesheet.addRule refinedRule
     , propertyRule = refinedRule
     , selector = newSelectorData
     }
