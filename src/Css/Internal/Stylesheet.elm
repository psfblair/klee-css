module Css.Internal.Stylesheet
  ( Css, CssAppender, PropertyRuleAppender, SelectorRuleAppender
  , MediaQueryRuleAppender, KeyframesRuleAppender, FontFaceRuleAppender
  , ImportRuleAppender
  , RuleData (..), emptyCss, addRule, extractRuleData
  , simpleProperty, prefixed, custom
  , MediaQuery (..), MediaType (..), NotOrOnly (..), Feature (..)
  , query, queryNot, queryOnly
  , Keyframes (..), keyframes, keyframesFromTo, fontFace, importUrl
  ) where

import Css.Internal.Property exposing
  (Key, Value, Prefixed, stringKey, prefixedKey, stringValue)
import Css.Internal.Selector exposing (SelectorData, Refinement)
import Css.Internal.Utils exposing (compose)

-------------------------------------------------------------------------------

{-| The `Css` type is used to collect style rules which are mappings
from selectors to style properties.
-}
type alias Css = List RuleData

emptyCss : Css
emptyCss = []

{- A container for a function of Css -> Css, which is the type allowing for the
css combinators to be composed. This is held in a record so that we can use
structural typing to enforce some type constraints.
-}
type alias CssAppender a = { a | addCss: Css -> Css }

{- These rules enforce type safety but we can't use the type system to enforce
that they themselves are used properly when building the DSL. I.e., since
records are not recursive, there is no way to enforce that propertyRule always
contains a property rule. This all has to be observed by the functions
constructing the records. We don't want to phantom type `RuleData` because then
Css could not contain a list of heterogeneous types of `RuleData`. It might be
thought that `addCss` could be of type `Css a -> Css ()` with the type
enforcement on the level of `addPropertyRule` which might e.g., take something
like `Css PropertyRule`. However, records cannot call themselves recursively. In
addition, the `Css` that is the type of the parameter to `addCss` is not the type
of rule that is added by addCss, but the type of Css that is accumulated as it
is passed through the composition of all the rules. As a result, even
`propertyRule` would have to be of type `Css ()`, which would defeat the purpose
of introducing the phantom type in the first place.
-}
type alias PropertyRuleAppender = CssAppender (WithPropertyRule {})
type alias WithPropertyRule a = { a | propertyRule: RuleData }

type alias SelectorRuleAppender = CssAppender (WithSelector (WithPropertyRule {}))
type alias WithSelector a = { a | selector : SelectorData }

type alias MediaQueryRuleAppender =  CssAppender (WithMediaQueryRule {})
type alias WithMediaQueryRule a = { a | mediaQueryRule: RuleData }

type alias KeyframesRuleAppender = CssAppender (WithKeyframesRule {})
type alias WithKeyframesRule a = { a | keyframesRule: RuleData }

type alias FontFaceRuleAppender =  CssAppender (WithFontFaceRule {})
type alias WithFontFaceRule a = { a | fontFaceRule: RuleData }

type alias ImportRuleAppender = CssAppender (WithImportRule {})
type alias WithImportRule a = { a | importRule: RuleData }

type RuleData
  = Property Key Value
  | Nested   SelectorData (List RuleData)
  | Query    MediaQuery (List RuleData)
  | Face     (List RuleData)
  | Keyframe Keyframes
  | Import   String

addRule : RuleData -> Css -> Css
addRule ruleDataToAdd rules = rules ++ [ ruleDataToAdd ]

{-| Types for the @media rule: @media not|only mediatype and (media feature) { rules }
    The MediaQuery type does not contain the media rules themselves.
-}
type MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType (List Feature)
type MediaType = MediaType String
type NotOrOnly = Not | Only
type Feature = Feature String (Maybe String)

{-| A @keyframes rule: @keyframes animation-name {keyframes-selector {css-styles;}}
    where keyframes-selector is the percentage of the animation duration.
-}
type Keyframes = Keyframes String (List (Float, (List RuleData)))

-------------------------------------------------------------------------------

{- Add a new style property to the stylesheet with the specified `Key` and value.
The value can be any type that that can be converted to a `Value` using the
a record of functions of type `Value`.
-}
addProperty : Key -> Value -> PropertyRuleAppender
addProperty key val =
  let ruleData = Property key val
  in { addCss = addRule ruleData
     , propertyRule = ruleData
     }

simpleProperty : String -> Value -> PropertyRuleAppender
simpleProperty keyName val = addProperty (stringKey keyName) val
  
{- Add a new style property to the stylesheet with the specified `Key` and value
the same way `key` does, but uses a `PrefixedOrNot` key.
-}
prefixed : Prefixed -> Value -> PropertyRuleAppender
prefixed prefxd = addProperty (prefixedKey prefxd)

{-| The custom function can be used to create property-value style rules for
which there is no typed version available. Both the key and the value
are plain text values and rendered as is to the output CSS.
-}
custom : String -> String -> PropertyRuleAppender
custom keyName val = addProperty (stringKey keyName) (stringValue val)

-------------------------------------------------------------------------------

{-| A @media rule to apply a set of style rules when a media type and
feature queries apply.
-}
query : MediaType ->
        List Feature ->
        List PropertyRuleAppender ->
        MediaQueryRuleAppender
query mediaType mediaFeatures mediaRules =
  createMediaQueryRuleAppender mediaType mediaFeatures Nothing mediaRules

{-| A  @media rule to apply a set of style rules when the media type and
feature queries do not apply.
-}
queryNot : MediaType ->
           List Feature ->
           List PropertyRuleAppender ->
           MediaQueryRuleAppender
queryNot mediaType mediaFeatures mediaRules =
  createMediaQueryRuleAppender mediaType mediaFeatures (Just Not) mediaRules

{-| A  @media rule to apply a set of style rules only when the media type and
feature queries apply.
-}
queryOnly : MediaType ->
            List Feature ->
            List PropertyRuleAppender ->
            MediaQueryRuleAppender
queryOnly mediaType mediaFeatures mediaRules =
  createMediaQueryRuleAppender mediaType mediaFeatures (Just Only) mediaRules

-------------------------------------------------------------------------------
{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs.
-}
keyframes : String ->
            (List (Float, List PropertyRuleAppender)) ->
            KeyframesRuleAppender
keyframes animationName frames =
  let ruleToAdd
      = frames
      |> List.map (\(percentage, css) -> (percentage, (extractRuleData css)))
      |> Keyframes animationName
      |> Keyframe
  in { addCss = addRule ruleToAdd
     , keyframesRule = ruleToAdd
     }

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules.
-}
keyframesFromTo : String ->
                  List PropertyRuleAppender ->
                  List PropertyRuleAppender ->
                  KeyframesRuleAppender
keyframesFromTo animationName fromRules toRules =
  keyframes animationName [(0, fromRules), (100, toRules)]

-------------------------------------------------------------------------------

{-| Create a CSS @font-face rule from some css specifying font properties.
-}

fontFace : List PropertyRuleAppender -> FontFaceRuleAppender
fontFace fontProperties =
  let ruleToAdd = fontProperties |> extractRuleData |> Face
  in { addCss = addRule ruleToAdd
     , fontFaceRule = ruleToAdd
     }

-------------------------------------------------------------------------------
-- TODO Media-dependent import rules
{-| Create a CSS @import rule to import a CSS file from a URL.
-}
importUrl : String -> ImportRuleAppender
importUrl url =
  let ruleToAdd = Import url
  in { addCss = addRule ruleToAdd
     , importRule = ruleToAdd
     }

-------------------------------------------------------------------------------
-- * Ancillary functions used for implementation.

createMediaQueryRuleAppender : MediaType ->
                               List Feature ->
                               Maybe NotOrOnly ->
                               List PropertyRuleAppender ->
                               MediaQueryRuleAppender
createMediaQueryRuleAppender mediaType mediaFeatures maybeNotOrOnly mediaRules =
  let ruleToAdd
      = mediaRules
      |> extractRuleData
      |> Query (MediaQuery maybeNotOrOnly mediaType mediaFeatures)
  in { addCss = addRule ruleToAdd
     , mediaQueryRule = ruleToAdd
     }

extractRuleData : List (CssAppender a) -> List RuleData
extractRuleData cssAppenders =
  let wrappedFunctions = cssAppenders |> List.map (\appender -> appender.addCss)
  in compose wrappedFunctions <| emptyCss
