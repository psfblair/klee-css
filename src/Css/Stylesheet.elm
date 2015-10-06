module Css.Stylesheet (
  Css, CssGenerator, SelectorScope (..), Rule (..), emptyCss, extractRules, custom
  , assignSelector, addStylesAsChild, addFilteredStyles, root
  , MediaQuery (..), MediaType (..), NotOrOnly (..), Feature (..), query, queryNot, queryOnly
  , Keyframes (..), keyframes, keyframesFromTo, fontFace, importUrl
  ) where

import Css.Property exposing (Key, Value, ValueWrapper, PrefixedOrNot
  , cast, stringKey, stringValueWrapper)
import Css.Selector exposing (Selector, Refinement)

-------------------------------------------------------------------------------

{-| The `Css` type is used to collect style rules which are mappings
from selectors to style properties.
-}
type Css = Css (List Rule)

{-| An alias for a function of Css -> Css, which is the type allowing for the
css combinators to be composed.
-}
type alias CssGenerator a = Css -> Css

emptyCss : Css
emptyCss = Css []

compose : List (a -> a) -> a -> a
compose = List.foldl (>>) identity

extractRules : List (CssGenerator a) -> (List Rule)
extractRules cssGenerators =
  let extract (Css rules) = rules
  in  emptyCss |> compose cssGenerators |> extract

addRule : Rule -> Css -> Css
addRule ruleToAdd (Css rules) = rules ++ [ ruleToAdd ] |> Css

type Rule
  = Property (Key ()) Value
  | Nested   SelectorScope (List Rule)
  | Query    MediaQuery (List Rule)
  | Face     (List Rule)
  | Keyframe Keyframes
  | Import   String

{-| A type allowing various selectors to be composed together to indicate the
scope of a set of rules.
-}
type SelectorScope
  = Self   Refinement
  | Root   Selector
  | Child  Selector
  | Sub    Selector

{-| Types for the @media rule: @media not|only mediatype and (media feature) { rules }
    The MediaQuery type does not contain the media rules themselves.
-}
type MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType (List Feature)
type MediaType = MediaType Value
type NotOrOnly = Not | Only
type Feature = Feature String (Maybe Value)

{-| A @keyframes rule: @keyframes animation-name {keyframes-selector {css-styles;}}
    where keyframes-selector is the percentage of the animation duration.
-}
type Keyframes = Keyframes String (List (Float, (List Rule)))

-------------------------------------------------------------------------------

{- Add a new style property to the stylesheet with the specified `Key` and value.
The value can be any type that that can be converted to a `Value` using the
a record of functions of type `ValueWrapper`.
-}
key : Key a -> a -> ValueWrapper a -> CssGenerator ()
key k v wrapper = Property (cast k) (wrapper.value v) |> addRule

{- Add a new style property to the stylesheet with the specified `Key` and value
the same way `key` does, but uses a `PrefixedOrNot` key.
-}
prefixed : PrefixedOrNot -> a -> ValueWrapper a -> CssGenerator ()
prefixed prefixedOrNot = key (Css.Property.Key prefixedOrNot)

{-| The custom function can be used to add style rules to the current context
for which there is no typed version available. Both the key and the value
are plain text values and rendered as is to the output CSS.
-}
custom : String -> String -> CssGenerator ()
custom k v  = key (stringKey k) v stringValueWrapper

{-| Assign a group of style rules to a selector. When the selector is nested inside an
outer scope it will be composed with `deep`, which maps to @sel1 sel2@ in CSS. This
function is not exposed by the outer Css module; it is intended to be called only
when creating the various embedded element functions; the `element` function in
`Css.Element` also calls it when creating custom selectors.
-}
assignSelector : Selector -> List (CssGenerator a) -> CssGenerator Selector
assignSelector sel rulesToAdd = addRule <| Nested (Sub sel) (extractRules rulesToAdd)

{-| Assign a group of style rules to a selector. When the selector is nested inside
an outer scope it will be composed with `child`, which maps to @sel1 > sel2@ in CSS.
-}
{-
 Implementation notes: This function takes as its first parameter a function that
 takes a list of Css Generators and returns a `CssGenerator` parameterized by
 `Selector`. These functions are created by assigning selectors using
 `assignSelector` above (e.g., as done in in `Css.Elements`). But `assignSelector`
 returns a `CssGenerator` that assigns rules directly to the selector or to the
 selector composed with an outer scope selector using `deep`. Here we want to
 change the assignment so that the resulting `CssGenerator Selector` instead
 composes with the outer scope selector using `child`. To do so we have to supply
 an empty list of generators to the function to get the `CssGenerator Selector`,
 then extract the selector rule, and finally extract the selector from that. We
 then replace the selector rule with a new rule composing the selector with the
 outer selector using `child`.

 By construction, we expect that the passed-in selector function will generate
 only one selector rule, with no rules embedded in it. However, we have to account
 for the possibility that there are some embedded rules. So we will nest as a child
 every selector for which there are rules, but we will not keep any rules from the
 original list that contained no embedded sub-rules.
-}
addStylesAsChild : (List (CssGenerator a) -> CssGenerator Selector) -> List (CssGenerator a) -> CssGenerator Selector
addStylesAsChild selectorFunction rulesToEmbed =
  let emptyListOfCssGenerators = []
      bareSelectorCssGenerator = selectorFunction emptyListOfCssGenerators
      rules = extractRules [ bareSelectorCssGenerator ]
      extractSelector rule = case rule of
        Nested (Root sel) _ -> Just sel
        Nested (Sub sel) _ -> Just sel
        Nested (Child sel) _ -> Just sel
        _ -> Nothing
      selectors = rules |> List.filterMap extractSelector
      rulesToAdd = selectors |> List.map (\sel -> Nested (Child sel) (extractRules rulesToEmbed))
      containsEmbeddedRules rule = case rule of
        Nested _ [] -> False
        _ -> True
      rulesToKeep = rules |> List.filter containsEmbeddedRules
  in \css -> List.foldl addRule css (rulesToKeep ++ rulesToAdd)

{-| Assign a group of style rules to a filter selector. When the selector is nested
inside an outer scope it will be composed with the `with` selector, which maps
to something like @sel#filter@ or @sel.filter@ in CSS depending on the filter.
-}
addFilteredStyles : Refinement -> List (CssGenerator a) -> CssGenerator ()
addFilteredStyles refinemt rulesToAdd = addRule <| Nested (Self refinemt) (extractRules rulesToAdd)

{-| `root` is used to add style rules to the top stylesheet scope. Note that this
is not the CSS :root selector, which refers to the root of the document, and which
is defined in `Css.Pseudo`.
-}
root : Selector -> List (CssGenerator a) -> CssGenerator ()
root sel rulesToAdd = addRule <| Nested (Root sel) (extractRules rulesToAdd)

-------------------------------------------------------------------------------

{-| A @media rule to apply a set of style rules when a media type and
feature queries apply.
-}
query : MediaType -> (List Feature) -> List (CssGenerator a) -> CssGenerator ()
query typeOfMedia mediaFeatures mediaRules =
  extractRules mediaRules
  |> Query (MediaQuery Nothing typeOfMedia mediaFeatures)
  |> addRule

{-| A  @media rule to apply a set of style rules when the media type and
feature queries do not apply.
-}
queryNot : MediaType -> (List Feature) -> List (CssGenerator a) -> CssGenerator ()
queryNot typeOfMedia mediaFeatures mediaRules =
  extractRules mediaRules
  |> Query (MediaQuery (Just Not) typeOfMedia mediaFeatures)
  |> addRule

{-| A  @media rule to apply a set of style rules only when the media type and
feature queries apply.
-}
queryOnly : MediaType -> (List Feature) -> List (CssGenerator a) -> CssGenerator ()
queryOnly typeOfMedia mediaFeatures mediaRules =
  extractRules mediaRules
  |> Query (MediaQuery (Just Only) typeOfMedia mediaFeatures)
  |> addRule

-------------------------------------------------------------------------------
{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs.
-}
keyframes : String -> (List (Float, List (CssGenerator a))) -> CssGenerator ()
keyframes animationName frames =
  frames
    |> List.map (\(percentage, css) -> (percentage, (extractRules css)))
    |> Keyframes animationName
    |> Keyframe
    |> addRule

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules.
-}
keyframesFromTo : String -> List (CssGenerator a) -> List (CssGenerator a) -> CssGenerator ()
keyframesFromTo animationName fromRules toRules =
  keyframes animationName [(0, fromRules), (100, toRules)]

-------------------------------------------------------------------------------

{-| Create a CSS @font-face rule from some css specifying font properties.
-}
fontFace : List (CssGenerator a) -> CssGenerator ()
fontFace fontProperties = extractRules fontProperties |> Face |> addRule

-------------------------------------------------------------------------------

{-| Create a CSS @import rule to import a CSS file from a URL.
-}
importUrl : String -> CssGenerator ()
importUrl url = Import url |> addRule
