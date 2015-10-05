module Css.Stylesheet (
  Css, Scope (..), Rule (..), custom, addStyles, addChildStyles, addFilteredStyles, root, pop
  , MediaQuery (..), MediaType (..), NotOrOnly (..), Feature (..), query, queryNot, queryOnly
  , Keyframes (..), keyframes, keyframesFromTo, fontFace, importUrl
  , emptyCss, runS) where

import Css.Property exposing (Key, Value, ValueWrapper, PrefixedOrNot
  , cast, stringKey, stringValueWrapper)
import Css.Selector exposing (Selector, Refinement)

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

{-| A type allowing various selectors to be composed together to indicate the
scope of a set of rules.
-}
type Scope
  = Self   Refinement
  | Root   Selector
  | Pop    Int
  | Child  Selector
  | Sub    Selector

type Rule
  = Property (Key ()) Value
  | Nested   Scope (List Rule)
  | Query    MediaQuery (List Rule)
  | Face     (List Rule)
  | Keyframe Keyframes
  | Import   String

{-| In Clay, the `Css` context is used to collect style rules which are mappings
from selectors to style properties. The `Css` type is a computation in the
`StyleM` monad that just collects and doesn't return anything.
-}
type Css = Css (List Rule)

emptyCss : Css
emptyCss = Css []

runS : Css -> (List Rule)
runS (Css rules) = rules

addRule : Rule -> Css -> Css
addRule ruleToAdd (Css rules) = rules ++ [ ruleToAdd ] |> Css

{- Add a new style property to the stylesheet with the specified `Key` and value.
The value can be any type that is in the `Val' typeclass; i.e., that can be
converted to a `Value`. No typeclasses in the Elm version!
-}
key : Key a -> a -> ValueWrapper a -> Css -> Css
key k v wrapper = Property (cast k) (wrapper.value v) |> addRule

{- In Clay, the prefixed function adds a new style property to the stylesheet
with the specified `Key` and value in the same way `key` does, but uses a
`PrefixedOrNot` key. Again, not available in Elm.
-}
prefixed : PrefixedOrNot -> a -> ValueWrapper a -> Css -> Css
prefixed prefixedOrNot = key (Css.Property.Key prefixedOrNot)

{-| The custom function can be used to add style rules to the current context
for which there is no typed version available. Both the key and the value
are plain text values and rendered as is to the output CSS.
-}
custom : String -> String -> Css -> Css
custom k v  = key (stringKey k) v stringValueWrapper

{-| Assign a group of style rules to a selector. When the selector is nested inside an
outer scope it will be composed with `deep`, which maps to @sel1 sel2@ in CSS.
-}
addStyles : Selector -> Css -> Css -> Css
addStyles sel rulesToAdd = addRule <| Nested (Sub sel) (runS rulesToAdd)

{-| Assign a group of style rules to a selector. When the selector is nested inside
an outer scope it will be composed with `child`, which maps to @sel1 > sel2@ in CSS.
-}
addChildStyles : Selector -> Css -> Css -> Css
addChildStyles sel rulesToAdd = addRule <| Nested (Child sel) (runS rulesToAdd)

{-| Assign a group of style rules to a filter selector. When the selector is nested
inside an outer scope it will be composed with the `with` selector, which maps
to something like @sel#filter@ or @sel.filter@ in CSS depending on the filter.
-}
addFilteredStyles : Refinement -> Css -> Css -> Css
addFilteredStyles refinemt rulesToAdd = addRule <| Nested (Self refinemt) (runS rulesToAdd)

{-| `root` is used to add style rules to the top scope.
-}
root : Selector -> Css -> Css -> Css
root sel rulesToAdd = addRule <| Nested (Root sel) (runS rulesToAdd)

{-| `pop` is used to add style rules to selectors defined in an outer scope. The
counter specifies how far up the scope stack we want to add the rules.
-}
pop : Int -> Css -> Css -> Css
pop numLevels rulesToAdd = addRule <| Nested (Pop numLevels) (runS rulesToAdd)

-------------------------------------------------------------------------------

{-| A @media rule to apply a set of style rules when a media type and
feature queries apply.
-}
query : MediaType -> (List Feature) -> Css -> Css -> Css
query typeOfMedia mediaFeatures mediaRules =
  runS mediaRules
  |> Query (MediaQuery Nothing typeOfMedia mediaFeatures)
  |> addRule

{-| A  @media rule to apply a set of style rules when the media type and
feature queries do not apply.
-}
queryNot : MediaType -> (List Feature) -> Css -> Css -> Css
queryNot typeOfMedia mediaFeatures mediaRules =
  runS mediaRules
  |> Query (MediaQuery (Just Not) typeOfMedia mediaFeatures)
  |> addRule

{-| A  @media rule to apply a set of style rules only when the media type and
feature queries apply.
-}
queryOnly : MediaType -> (List Feature) -> Css -> Css -> Css
queryOnly typeOfMedia mediaFeatures mediaRules =
  runS mediaRules
  |> Query (MediaQuery (Just Only) typeOfMedia mediaFeatures)
  |> addRule

-------------------------------------------------------------------------------
{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs.
-}
keyframes : String -> (List (Float, Css)) -> Css -> Css
keyframes animationName frames =
  frames
    |> List.map (\(percentage, css) -> (percentage, (runS css)))
    |> Keyframes animationName
    |> Keyframe
    |> addRule

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules.
-}
keyframesFromTo : String -> Css -> Css -> Css -> Css
keyframesFromTo animationName fromRules toRules =
  keyframes animationName [(0, fromRules), (100, toRules)]

-------------------------------------------------------------------------------

{-| Create a CSS @font-face rule from some css specifying font properties.
-}
fontFace : Css -> Css -> Css
fontFace fontProperties = runS fontProperties |> Face |> addRule

-------------------------------------------------------------------------------

{-| Create a CSS @import rule to import a CSS file from a URL.
-}
importUrl : String -> Css -> Css
importUrl url = Import url |> addRule
