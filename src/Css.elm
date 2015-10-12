module Css
  ( Stylesheet, StyleProperty, PropertyStylesheet, MediaStylesheet
  , KeyframeStylesheet, FontFaceStylesheet, ImportStylesheet
  , Selector, Refinement
  , group, (:..), descend, (:~), child, (:>), sibling, (:+), with, (:|)
  , byId, (:#), byClass, (:.), pseudo, func
  , attr, (:@), attrBegin, (:@^), attrEnd, (:@$), attrHas, (:@*)
  , attrInSpaceList, (:@~), attrInHyphenList, (:@-)
  , Config, render, renderCompact, renderProperties, renderWith
  , MediaType, Feature, query, queryNot, queryOnly
  , keyframes, keyframesFromTo, fontFace, importUrl
  , custom, element, filter, withAttr
  )  where

{-| A module for constructing Css in a typesafe way, and rendering the result
to a string (either pretty-printed or compact). This library is based on the
Haskell Clay CSS preprocessing framework, though some of the operators in the
selector DSL have been changed in order to avoid clashing with standard Elm
operators and for various other reasons.

# Principal Types
@docs Stylesheet, StyleProperty, PropertyStylesheet, MediaStylesheet,
      KeyframeStylesheet, FontFaceStylesheet, ImportStylesheet,
      Selector, Refinement

# The selector language.
For predefined element selectors, see `Css.Elements`, for pseudo-selectors see
`Css.Pseudo`, and for predefined attributes, see `Css.Attributes`.
@docs group, (:..), descend, (:~), child, (:>), sibling, (:+), with, (:|),
      byId, (:#), byClass, (:.), pseudo, func,
      attr, (:@), attrBegin, (:@^), attrEnd, (:@$),attrHas, (:@*),
      attrInSpaceList, (:@~), attrInHyphenList, (:@-)

# Rendering stylesheets to CSS strings
@docs Config, render, renderCompact, renderProperties, renderWith

# Special rules
@docs MediaType, Feature, query, queryNot, queryOnly, keyframes, keyframesFromTo,
      fontFace, importUrl

# Creating custom properties and selectors
@docs custom, element, filter, withAttr
-}

import Css.Internal.Stylesheet exposing (Css, MediaType, Feature)
import Css.Internal.Selector exposing (Refinement)
import Css.Internal.SelectorCombinators exposing (Selector)
import Css.Internal.Render exposing (pretty, compact)

-------------------------------------------------------------------------------
-- * Principal types

{-| The `Stylesheet` type is the generic type for style rules which may be of
several different varieties: Mappings of values to properties, assigned to
selectors (`PropertyStylesheet`), Font-face rules (`FontFaceStylesheet`),
Media rules (`MediaStylesheet`), Keyframe rules (`KeyframeStylesheet`), and
imported rules (`ImportStyleshet`).
-}
type alias Stylesheet a = Css.Internal.Stylesheet.CssAppender a

{-| A `StyleProperty` represents a mapping from a property to a value. Lists of
`StyleProperty` may be assigned to selectors, or used in font-face, media, and
keyframe rules.
-}
type alias StyleProperty = Css.Internal.Stylesheet.PropertyRuleAppender

{-| A `PropertyStylesheet` represents an assignment of a list of rules to a
selector. These rules may be `StyleProperty` mappings, or other nested
`PropertyStylesheet` rules.
-}
type alias PropertyStylesheet = Css.Internal.Stylesheet.SelectorRuleAppender

{-| A `FontFaceStylesheet` represents a Css @font-face rule.
-}
type alias FontFaceStylesheet = Css.Internal.Stylesheet.FontFaceRuleAppender

{-| A `MediaStylesheet` represents a Css @media rule.
-}
type alias MediaStylesheet = Css.Internal.Stylesheet.MediaQueryRuleAppender

{-| A `KeyframeStylesheet` represents a Css @keyframes rule.
-}
type alias KeyframeStylesheet = Css.Internal.Stylesheet.KeyframesRuleAppender

{-| An `ImportStylesheet` represents a Css @import rule.
-}
type alias ImportStylesheet = Css.Internal.Stylesheet.ImportRuleAppender

{-|  A `Selector` represents the selector in a CSS rule. `Selector` is implemented
as a function that takes a list of rules (either `StyleProperty` or nested
`PropertyStylesheet` rules) and returns a `PropertyStylesheet`. Selectors may
be combined using the combinators defined in this module. Most of the selectors
one would need are predefined in the `Css.Elements` module, though if necessary
new custom selectors can be defined using the `element` function in that module.
-}
type alias Selector = Css.Internal.SelectorCombinators.Selector

{-|  A `Refinement` is a type that represents refinements on a selector; i.e.,
CSS classes, ids, attributes etc. that may be applied to a selector at a
given level as filters. Attributes are predefined in the `Css.Attributes` module;
CSS pseudo-selectors and pseudo-functions are predefined in the `Css.Pseudo`
module.
-}
type alias Refinement = Css.Internal.Selector.Refinement

-------------------------------------------------------------------------------
-- * The selector language.
-- ** Composing and refining selectors

{-| The group selector composer. aps to `sel1, sel2` in CSS, but unfortunately
we can't use commas in an operator.
-}
group : Selector -> Selector -> Selector
group = Css.Internal.SelectorCombinators.group

{-| Operator alias for group.
-}
(:..) : Selector -> Selector -> Selector
(:..) = Css.Internal.SelectorCombinators.group

{-| The descendant selector composer. Maps to `sel1 sel2` in CSS.
-}
descend : Selector -> Selector -> Selector
descend = Css.Internal.SelectorCombinators.descendant

{-| Operator alias for descend.
-}
(:~) : Selector -> Selector -> Selector
(:~) = Css.Internal.SelectorCombinators.descendant

{-| The child selector composer. Maps to `sel1 > sel2` in CSS.
-}
child : Selector -> Selector -> Selector
child = Css.Internal.SelectorCombinators.child

{-| Operator alias for child.
-}
(:>) : Selector -> Selector -> Selector
(:>) = Css.Internal.SelectorCombinators.child

{-| The next-sibling selector composer. Maps to `sel1 + sel2` in CSS.
-}
sibling : Selector -> Selector -> Selector
sibling = Css.Internal.SelectorCombinators.sibling

{-| Operator alias for sibling.
-}
(:+) : Selector -> Selector -> Selector
(:+) = Css.Internal.SelectorCombinators.sibling

{-| The filter selector composer, which adds a filter to a selector. Maps to
something like `sel#filter` or `sel.filter` in CSS, depending on the filter.
-}
with : Selector -> Refinement -> Selector
with = Css.Internal.SelectorCombinators.with

{-| Operator alias for with.
-}
(:|) : Selector -> Refinement -> Selector
(:|) = Css.Internal.SelectorCombinators.with

{-| Given an id and a selector, add an id filter to the selector.
-}
byId : Selector -> String -> Selector
byId = Css.Internal.SelectorCombinators.byId

{-| Operator alias for byId.
-}
(:#) : Selector -> String -> Selector
(:#) = Css.Internal.SelectorCombinators.byId

{-| Given a class name and a selector, add a class filter to the selector.
-}
byClass : Selector -> String -> Selector
byClass = Css.Internal.SelectorCombinators.byClass

{-| Operator alias for byClass.
-}
(:.) : Selector -> String -> Selector
(:.) = Css.Internal.SelectorCombinators.byClass

{-| Filter elements of the given selector by pseudo selector or pseudo class.
The preferred syntax is to use one of the predefined pseudo selectors from "Css.Pseudo".
-}
pseudo : Selector -> String -> Selector
pseudo = Css.Internal.SelectorCombinators.pseudo

{-| Filter elements of the given selector by pseudo selector functions. The
preferred syntax is to use one of the predefined functions from "Css.Pseudo".
-}
func : Selector -> String -> (List String) -> Selector
func = Css.Internal.SelectorCombinators.func

-- ** Attribute-based refining.

{-| Filter elements based on the presence of a certain attribute with the
specified name and value.
-}
attr : Selector -> String -> String -> Selector
attr = Css.Internal.SelectorCombinators.withAttrValue

{-| Operator alias for attr.
-}
(:@) : Selector -> String -> String -> Selector
(:@) = Css.Internal.SelectorCombinators.withAttrValue

{-| Filter elements based on the presence of a certain attribute that begins
with the specified value.
-}
attrBegin : Selector -> String -> String -> Selector
attrBegin = Css.Internal.SelectorCombinators.withAttrValueBeginning

{-| Operator alias for attrBegin.
-}
(:@^) : Selector -> String -> String -> Selector
(:@^) = Css.Internal.SelectorCombinators.withAttrValueBeginning

{-| Filter elements based on the presence of a certain attribute that ends
with the specified value.
-}
attrEnd : Selector -> String -> String -> Selector
attrEnd = Css.Internal.SelectorCombinators.withAttrValueEnding

{-| Operator alias for attrEnd.
-}
(:@$) : Selector -> String -> String -> Selector
(:@$) = Css.Internal.SelectorCombinators.withAttrValueEnding

{-| Filter elements based on the presence of a certain attribute that contains
the specified value as a substring.
-}
attrHas : Selector -> String -> String -> Selector
attrHas = Css.Internal.SelectorCombinators.withAttrValueContaining

{-| Operator alias for attrHas.
-}
(:@*) : Selector -> String -> String -> Selector
(:@*) = Css.Internal.SelectorCombinators.withAttrValueContaining

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a space separated list.
-}
attrInSpaceList : Selector -> String -> String -> Selector
attrInSpaceList = Css.Internal.SelectorCombinators.withAttrValueInSpacedList

{-| Operator alias for attrInSpaceList.
-}
(:@~) : Selector -> String -> String -> Selector
(:@~) = Css.Internal.SelectorCombinators.withAttrValueInSpacedList

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a hyphen separated list.
-}
attrInHyphenList : Selector -> String -> String -> Selector
attrInHyphenList = Css.Internal.SelectorCombinators.withAttrValueInHyphenatedList

{-| Operator alias for attrInHyphenList.
-}
(:@-) : Selector -> String -> String -> Selector
(:@-) = Css.Internal.SelectorCombinators.withAttrValueInHyphenatedList

-------------------------------------------------------------------------------
-- * Rendering stylesheets to CSS strings.

{-| `Config` is a type that allows configuring print output of separators,
newlines, etc.
-}
type alias Config = Css.Internal.Render.Config

{-| Render a stylesheet with the default configuration, using the pretty printer.
-}
render : List ( {a | addCss: Css -> Css } ) -> String
render = Css.Internal.Render.renderWith pretty

{-| Render a stylesheet in compact format.
-}
renderCompact : List (Stylesheet a) -> String
renderCompact = Css.Internal.Render.renderWith compact

{-| Render a list of properties t in compact format, in a style suitable for an
HTML style attribute.
-}
renderProperties : List StyleProperty -> String
renderProperties styleProperties
  = styleProperties
  |> List.map (\property -> property.propertyRule)
  |> Css.Internal.Render.ruleProperties
  |> Css.Internal.Render.renderProperties compact

{-| Render a stylesheet with a custom print configuration.
-}
renderWith : Config -> List ( {a | addCss: Css -> Css } ) -> String
renderWith = Css.Internal.Render.renderWith

-------------------------------------------------------------------------------
-- * Special rules
-- ** The @font-face rule.

{-| Create a CSS @font-face rule from some Css specifying font properties.
-}
fontFace : List StyleProperty -> FontFaceStylesheet
fontFace = Css.Internal.Stylesheet.fontFace

-------------------------------------------------------------------------------
-- ** The @media rule.

{-| `MediaType` represents the media type portion of a Css @media rule.
-}
type alias MediaType = Css.Internal.Stylesheet.MediaType

{-| `Feature` represents the media features portion of a Css @media rule.
-}
type alias Feature = Css.Internal.Stylesheet.Feature

{-| A @media rule to apply a set of style rules whenever a media type and
feature queries apply.
-}
query : MediaType ->
        List Feature ->
        List StyleProperty ->
        MediaStylesheet
query = Css.Internal.Stylesheet.query

{-| A  @media rule to apply a set of style rules when the media type and
feature queries do not apply.
-}
queryNot : MediaType ->
           List Feature ->
           List StyleProperty ->
           MediaStylesheet
queryNot = Css.Internal.Stylesheet.queryNot

{-| A  @media rule to apply a set of style rules only when the media type and
feature queries apply.
-}
queryOnly : MediaType ->
            List Feature ->
            List StyleProperty ->
            MediaStylesheet
queryOnly = Css.Internal.Stylesheet.queryOnly

-------------------------------------------------------------------------------
-- ** The @keyframes rule.

{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs.
-}
keyframes : String -> (List (Float, List StyleProperty)) -> KeyframeStylesheet
keyframes = Css.Internal.Stylesheet.keyframes

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules.
-}
keyframesFromTo : String ->
                  List StyleProperty ->
                  List StyleProperty ->
                  KeyframeStylesheet
keyframesFromTo = Css.Internal.Stylesheet.keyframesFromTo

-------------------------------------------------------------------------------
-- ** The @import rule.

{-| Create a CSS @import rule to import a CSS file from a URL.
-}
importUrl : String -> ImportStylesheet
importUrl = Css.Internal.Stylesheet.importUrl

-------------------------------------------------------------------------------
-- * Creating custom properties, selectors, and refinements

{-| The `custom` function can be used to add style rules to the current context
for which there is no typed version available. Both the key and the value
are plain text values and rendered as-is to the output CSS. The preferred
approach is to use the type-safe selectors and values provided by the
various Css sub-modules, since this function provides no type-safety.
-}
custom : String -> String -> StyleProperty
custom = Css.Internal.Stylesheet.custom

{-| Create a new selector by name. The preferred syntax is to
just use one of the predefined elements from "Css.Elements".
-}
element : String -> Selector
element = Css.Internal.SelectorCombinators.element

{-| Create a new filter by name. The preferred syntax is to
use `byId` and `byClass` to filter with ids and classes, to use one of the
predefined attributes from "Css.Attributes" for attributes, or one
of the predefined pseudo-classes and pseudo functions from `Css.Pseudo`.
-}
filter : String -> Refinement
filter = Css.Internal.SelectorCombinators.filter

{-| Given an attribute name and a selector, filter elements based on the presence
of that attribute. The preferred syntax is use one of the predefined attribute
Refinements from "Css.Attributes".
-}
withAttr : Selector -> String -> Selector
withAttr = Css.Internal.SelectorCombinators.withAttr
