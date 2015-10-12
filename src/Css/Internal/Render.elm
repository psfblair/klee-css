module Css.Internal.Render where

import String
import Dict

import Css.Internal.Stylesheet exposing
  ( Css, CssAppender, RuleData (..)
  , MediaQuery (..), MediaType (..), NotOrOnly (..), Feature (..)
  , Keyframes (..), emptyCss, extractRuleData
  )
import Css.Common exposing (browsers)
import Css.Internal.Property exposing
  ( Key (..), Value (..), PrefixedOrNot (..)
  , Either (..)
  , plain, unPrefixed, rightValue
  )
import Css.Internal.Selector exposing
  ( SelectorData (..), Refinement (..), Path (..), Predicate (..)
  , emptySelectorData, sortPredicate
  )
import Css.Internal.SelectorCombinators exposing
  ( Selector, descendant, child, with
  )
import Css.Internal.Utils exposing (mapPairwise)
-------------------------------------------------------------------------------

type alias Config =
  { indentation    : String
  , newline        : String
  , sep            : String
  , finalSemicolon : Bool
  , warn           : Bool
  , align          : Bool
  , banner         : Bool
  }

{-| Configuration to print to a pretty human readable CSS output.
-}
pretty : Config
pretty =
  { indentation    = "  "
  , newline        = "\n"
  , sep            = " "
  , finalSemicolon = True
  , warn           = True
  , align          = True
  , banner         = True
  }

{-| Configuration to print to a compacted unreadable CSS output.
-}
compact : Config
compact =
  { indentation    = ""
  , newline        = ""
  , sep            = ""
  , finalSemicolon = False
  , warn           = False
  , align          = False
  , banner         = False
  }

{-| Render a stylesheet with a custom configuration. The `CssAppender` argument
is a stylesheet, represented as a function of `Css -> Css`, which `renderWith`
will supply with an empty `Css` as an accumulator.
-}
renderWith : Config -> List (CssAppender a) -> String
renderWith cfg stylesheets
  = extractRuleData stylesheets
  |> renderRules cfg emptySelectorData
  |> renderBanner cfg

-------------------------------------------------------------------------------

{-| Render a single CSS `Selector`.
 -}
renderSelector : Selector -> String
renderSelector selector =
  let selectorAppender = selector []
      selectorData = selectorAppender.selector
  in renderSelectorWithConfig compact selectorData

{-| Adds a "Generated with elm-css" comment to the bottom of the rendered Css.
-}
renderBanner : Config -> String -> String
renderBanner cfg =
  if | cfg.banner -> (\x -> x ++ "\n/* Generated with elm-css */")
     | otherwise  -> identity

-------------------------------------------------------------------------------

{-| Render a list of CSS rules inside a given scope. The scope is specified
by a listing of Scope objects that specifies how the scope is composed of
its various nested levels.
-}
renderRules : Config -> SelectorData -> (List RuleData) -> String
renderRules cfg selector ruleList =
  let property prop =
        case prop of
          (Property k v) -> Just (k, v)
          _  -> Nothing
      nested n =
        case n of
          (Nested selectorData nestedRules) -> Just (selectorData, nestedRules)
          _  -> Nothing
      queries qs =
        case qs of
          (Query q nestedRules) -> Just (q, nestedRules)
          _ -> Nothing
      kframes kfs =
        case kfs of
          (Keyframe nestedRules) -> Just nestedRules
          _ -> Nothing
      faces fcs =
        case fcs of
          (Face nestedRules) -> Just nestedRules
          _ -> Nothing
      imports imp =
        case imp of
          (Import i    ) -> Just i
          _ -> Nothing
  in concat
      [ renderRule cfg selector (List.filterMap property ruleList)
      , cfg.newline
      , List.filterMap imports ruleList |> List.map (renderImportRule cfg) |> concat
      , List.filterMap kframes ruleList |> List.map (renderKeyframes cfg) |> concat
      , List.filterMap faces   ruleList |> List.map (renderFontFace cfg) |> concat
      , List.filterMap nested  ruleList
            |> List.map (\(selectorData, nestedRules) -> renderRules cfg selectorData nestedRules)
            |> concat
      , List.filterMap queries ruleList
            |> List.map (\(qry, nestedRules) -> renderMedia cfg qry selector nestedRules)
            |> concat
      ]

{-| Renders to a string a set of property/value rules inside a given scope. (The
property/value rules do not include nested rules or rules for media, keyframes,
font-face, or imports. The scope is specified by a listing of Scope objects that
specifies how the scope is composed of its various nested levels.
-}
renderRule : Config -> SelectorData -> (List (Key (), Value)) -> String
renderRule cfg selectorData propertyRules =
  case propertyRules of
    [] -> ""
    (h::t) ->
      concat
        [ renderSelectorWithConfig cfg selectorData
        , cfg.newline
        , "{"
        , cfg.newline
        , (List.map toEithers propertyRules
            |> List.map (renderProperty cfg)
            |> concat)
        , "}"
        , cfg.newline
        ]

{- Render a selector to a string, given a `Config` for specifying the
newline behavior. -}
renderSelectorWithConfig : Config -> SelectorData -> String
renderSelectorWithConfig cfg =
  let expandSelectorIntoStringList (SelectorData (Refinement preds) selectorPath) =
        let insertSeparator separator str1 str2 = str1 ++ separator ++ str2
            predString = List.sortWith sortPredicate preds |> List.map renderPredicate |> concat
            appendPredStringToPathComponent predStr component = component ++ predStr
            pathComponents =
              case selectorPath of
                Star           -> if List.isEmpty preds then ["*"] else [""]
                Elem t         -> [t]
                Css.Internal.Selector.Child sel1 sel2 ->
                  mapPairwise (insertSeparator " > ")
                              (expandSelectorIntoStringList sel1)
                              (expandSelectorIntoStringList sel2)
                Descendant sel1 sel2 ->
                  mapPairwise (insertSeparator " ")
                              (expandSelectorIntoStringList sel1)
                              (expandSelectorIntoStringList sel2)
                Adjacent sel1 sel2 ->
                  mapPairwise (insertSeparator " + ")
                              (expandSelectorIntoStringList sel1)
                              (expandSelectorIntoStringList sel2)
                Combined sel1 sel2 ->
                  expandSelectorIntoStringList sel1 ++
                    expandSelectorIntoStringList sel2
        in List.map (appendPredStringToPathComponent predString) pathComponents
  in expandSelectorIntoStringList >> String.join ("," ++ cfg.newline)


{- Render to a string a predicate that refines a selector.
-}
renderPredicate : Predicate -> String
renderPredicate pred =
  case pred of
    Id           a   -> "#" ++ a
    Class        a   -> "." ++ a
    Attr         a   -> "[" ++ a ++                "]"
    AttrVal      a v -> "[" ++ a ++  "='" ++ v ++ "']"
    AttrBegins   a v -> "[" ++ a ++ "^='" ++ v ++ "']"
    AttrEnds     a v -> "[" ++ a ++ "$='" ++ v ++ "']"
    AttrContains a v -> "[" ++ a ++ "*='" ++ v ++ "']"
    AttrSpace    a v -> "[" ++ a ++ "~='" ++ v ++ "']"
    AttrHyph     a v -> "[" ++ a ++ "|='" ++ v ++ "']"
    Pseudo       a   -> ":" ++ a
    PseudoFunc   a args -> ":" ++ a ++ "(" ++ (String.join "," args) ++ ")"

{-  Returns either a pair of key,value (Right), or a prefixed key (Left).
    The prefixes that are carried along by the Prefixed type are concatenated
    with the keys or values respectively if they pertain to one or the other.
    If both the keys and values are prefixed, for each of the key prefixes
    the corresponding value is looked up and the prefix is concatenated with
    both the key and value in the returned Right; if no value is found for
    the prefix, a Left containing the prefixed key is returned. This function
    is called by `renderRule` for each of the key-value properties for the rule.
    The results are fed to the properties function below which stringifies them.
 -}
toEithers : (Key (), Value) -> List (Either String (String, String))
toEithers (Key ky, Value vl) =
  case (ky, vl) of
    ( Plain    k  , Plain    v  ) -> [ Right(k, v) ]
    ( Prefixed ks , Plain    v  ) -> ks |> List.map (\(prefix, k) -> Right(prefix ++ k, v))
    ( Plain    k  , Prefixed vs ) -> vs |> List.map (\(prefix, v) -> Right(k, prefix ++ v))
    ( Prefixed ks , Prefixed vs ) ->
        ks |> List.map
                (\(prefix, k) ->
                  let default = Left (prefix ++ k)
                      rightFromVal val = Right(prefix ++ k, prefix ++ val)
                      maybeVal = Dict.get prefix (Dict.fromList vs)
                  in Maybe.map rightFromVal maybeVal |> Maybe.withDefault default
                )

{-  Takes a key-value property for a rule in the form of either a pair of
    key,value (Right), or a prefixed key (Left). Returns a string representation
    of the property.
 -}
renderProperty : Config -> List (Either String (String, String)) -> String
renderProperty cfg eithers =
  let finalSemi  = if cfg.finalSemicolon then ";" else ""
      indent     = cfg.indentation
      endline    = cfg.newline
      goodKeys   = eithers |> List.filterMap rightValue |> List.map fst
      alignWidth = List.map String.length goodKeys
                      |> List.maximum
                      |> Maybe.withDefault (String.length indent)
                      |> ((+) 1)
      paddingFor theKey =
        if cfg.align
        then " " |> String.repeat (alignWidth - String.length theKey) --empty if length is negative
        else ""
      stringify either =
        case either of
          Right (k, val) -> concat [indent, k, (paddingFor k), ":", cfg.sep, val]
          Left orphan ->
            if cfg.warn
            then indent ++ "/* no value for " ++ orphan ++ " */" ++ endline
            else ""
  in List.map stringify eithers
      |> String.join (";" ++ endline)
      |> \x -> x ++ finalSemi ++ endline

-------------------------------------------------------------------------------

{-| Render a CSS @keyframes rule.
-}
renderKeyframes : Config -> Keyframes -> String
renderKeyframes cfg (Keyframes animationName listOfFrames) =
  unPrefixed browsers
  |> List.map
    ( \(browser, _) ->
      concat [ "@" ++ browser ++ "keyframes "
              , animationName
              , cfg.newline
              , "{"
              , cfg.newline
              , (listOfFrames |> List.map (renderKeyframe cfg) |> concat)
              , "}"
              , cfg.newline
              , cfg.newline
              ]
      )
  |> concat

{- Render one frame in a CSS @keyframes rule.
-}
renderKeyframe : Config -> (Float, (List RuleData)) -> String
renderKeyframe cfg (percentage, mediaRules) =
  concat
    [ percentage |> toString
    , "% "
    , renderRules cfg emptySelectorData mediaRules
    ]

{-| Render a CSS @media rule.
-}
renderMedia : Config -> MediaQuery -> SelectorData -> (List RuleData) -> String
renderMedia cfg query sel mediaRules =
  concat
    [ renderMediaQuery query
    , cfg.newline
    , "{"
    , cfg.newline
    , renderRules cfg sel mediaRules
    , "}"
    , cfg.newline
    ]

{- Render the media query part of a CSS @media rule.
-}
renderMediaQuery : MediaQuery -> String
renderMediaQuery (MediaQuery notOrOnly typeOfMedia mediaFeatures) =
  concat
    [ "@media "
    , case notOrOnly of
        Nothing   -> ""
        Just Not  -> "not "
        Just Only -> "only "
    , renderMediaType typeOfMedia
    , mediaFeatures
        |> List.map renderMediaFeature
        |> List.map (\feature -> " and " ++ feature)
        |> concat
    ]

{- Render the media type in the media query part of a CSS @media rule.
-}
renderMediaType : MediaType -> String
renderMediaType (MediaType (Value v)) = plain v

{- Render a media feature in the media query part of a CSS @media rule.
-}
renderMediaFeature : Feature -> String
renderMediaFeature (Feature featureName maybeFeatureValue) =
  case maybeFeatureValue of
    Nothing        -> featureName
    Just (Value v) ->
      concat [ "(" , featureName , ": " , (plain v) , ")" ]

{-| Render a CSS @font-face rule.
-}
renderFontFace : Config -> (List RuleData) -> String
renderFontFace cfg faceRules =
  concat
    [ "@font-face"
    , renderRules cfg emptySelectorData faceRules
    ]

{-| Render a CSS import rule.
-}
renderImportRule : Config -> String -> String
renderImportRule cfg url =
  concat
    [ "@import url("
    , url
    , ");"
    , cfg.newline
    ]

concat : List String -> String
concat = String.join ""
