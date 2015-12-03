module Css.Internal.Render where

import String

import Css.Common as Common
import Css.Internal.Property as Property
-- TODO Remove calls to constructors in Selector
import Css.Internal.Selector as Selector
-- TODO Remove calls to constructors in Stylesheet
import Css.Internal.Stylesheet as Stylesheet 
import Css.Internal.Utils as Utils

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

{- Configuration to print to a pretty human readable CSS output.
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

{- Configuration to print to a compacted unreadable CSS output.
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

{- Render a stylesheet with a custom configuration. The `Stylesheet.CssAppender` 
argument is a stylesheet, represented as a function of `Css -> Css`, which 
`renderWith` will supply with an empty `Css` as an accumulator.
-}
renderWith : Config -> List (Stylesheet.CssAppender a) -> String
renderWith cfg stylesheets
  = Stylesheet.extractRuleData stylesheets
  |> renderRules cfg []
  |> renderBanner cfg

{- Adds a "Generated with klee" comment to the bottom of the rendered Css.
-}
renderBanner : Config -> String -> String
renderBanner cfg =
  if | cfg.banner -> (\x -> x ++ "\n/* Generated with klee */")
     | otherwise  -> identity

-------------------------------------------------------------------------------

{- Render a list of CSS rules inside a given scope. The scope is specified
by a listing of Scope objects that specifies how the scope is composed of
its various nested levels.
-}
renderRules : Config -> 
              List Selector.SelectorData -> 
              List Stylesheet.RuleData -> 
              String
renderRules cfg selectors ruleList =
  let nested n =
        case n of
          (Stylesheet.Nested selectorData nestedRules) -> Just (selectorData, nestedRules)
          _  -> Nothing
      queries qs =
        case qs of
          (Stylesheet.Query q nestedRules) -> Just (q, nestedRules)
          _ -> Nothing
      kframes kfs =
        case kfs of
          (Stylesheet.Keyframe nestedRules) -> Just nestedRules
          _ -> Nothing
      faces fcs =
        case fcs of
          (Stylesheet.Face nestedRules) -> Just nestedRules
          _ -> Nothing
      imports imp =
        case imp of
          (Stylesheet.Import i    ) -> Just i
          _ -> Nothing
  in concat
      [ renderRule cfg selectors (ruleProperties ruleList)
      , cfg.newline
      , List.filterMap imports ruleList |> List.map (renderImportRule cfg) |> concat
      , List.filterMap kframes ruleList |> List.map (renderKeyframes cfg) |> concat
      , List.filterMap faces   ruleList |> List.map (renderFontFace cfg) |> concat
      , List.filterMap nested  ruleList
            |> List.map (\(selectorData, nestedRules) ->
                          renderRules cfg (selectorData :: selectors) nestedRules)
            |> concat
      , List.filterMap queries ruleList
            |> List.map (\(qry, nestedRules) ->
                            renderMedia cfg qry selectors nestedRules)
            |> concat
      ]

ruleProperties : List Stylesheet.RuleData -> List (Property.Key, Property.Value)
ruleProperties ruleList =
  let property prop =
      case prop of
        (Stylesheet.Property k v) -> Just (k, v)
        _  -> Nothing
  in List.filterMap property ruleList

{- Render to a string a set of property/value rules inside a given scope. (The
property/value rules do not include nested rules or rules for media, keyframes,
font-face, or imports.
-}
renderRule : Config -> 
             List Selector.SelectorData ->
             List (Property.Key, Property.Value) -> 
             String
renderRule cfg selectorDatas propertyRules =
  case propertyRules of
    [] -> ""
    (h::t) ->
      concat
        [ merge selectorDatas |> renderSelectorWithConfig cfg
        , cfg.newline
        , "{"
        , cfg.newline
        , renderProperties cfg propertyRules
        , "}"
        , cfg.newline
        ]

{- Merge selector data from nested scopes into one SelectorData.
Unlike Clay, here the only merge we need is to compose all selectors with the
"descendant" relationship. Any other composition is done by the combinators
before we get here.
Note that the way renderRules constructs the list of selector data, the outer
scopes are added to the list before the inner ones. So we traverse the list
from the left, with the innermost scopes first.
-}
merge : List Selector.SelectorData -> Selector.SelectorData
merge selectorDatas =
  let combineSelectorData selector1Data selector2Data =
        Selector.SelectorData (Selector.Refinement []) (Selector.Descendant selector1Data selector2Data)
  in List.foldl combineSelectorData Selector.emptySelectorData selectorDatas

{- Render a selector to a string, given a `Config` for specifying the
newline behavior. -}
renderSelectorWithConfig : Config -> Selector.SelectorData -> String
renderSelectorWithConfig cfg =
  let expandSelectorIntoStringList (Selector.SelectorData (Selector.Refinement preds) selectorPath) =
        let insertSeparator separator str1 str2 = str1 ++ separator ++ str2
            predString = List.sortWith Selector.sortPredicate preds |> List.map renderPredicate |> concat
            appendPredStringToPathComponent predStr component = component ++ predStr
            pathComponents =
              case selectorPath of
                Selector.Star -> if List.isEmpty preds then ["*"] else [""]
                Selector.Elem t -> [t]
                Selector.Child sel1 sel2 ->
                  Utils.mapPairwise (insertSeparator " > ")
                              (expandSelectorIntoStringList sel1)
                              (expandSelectorIntoStringList sel2)
                Selector.Descendant sel1 sel2 ->
                  Utils.mapPairwise (insertSeparator " ")
                              (expandSelectorIntoStringList sel1)
                              (expandSelectorIntoStringList sel2)
                Selector.Adjacent sel1 sel2 ->
                  Utils.mapPairwise (insertSeparator " + ")
                              (expandSelectorIntoStringList sel1)
                              (expandSelectorIntoStringList sel2)
                Selector.Combined sel1 sel2 ->
                  expandSelectorIntoStringList sel1 ++
                    expandSelectorIntoStringList sel2
        in List.map (appendPredStringToPathComponent predString) pathComponents
  in expandSelectorIntoStringList >> String.join ("," ++ cfg.newline)


{- Render to a string a predicate that refines a selector.
-}
renderPredicate : Selector.Predicate -> String
renderPredicate pred =
  case pred of
    Selector.Id           a   -> "#" ++ a
    Selector.Class        a   -> "." ++ a
    Selector.Attr         a   -> "[" ++ a ++                "]"
    Selector.AttrVal      a v -> "[" ++ a ++  "='" ++ v ++ "']"
    Selector.AttrBegins   a v -> "[" ++ a ++ "^='" ++ v ++ "']"
    Selector.AttrEnds     a v -> "[" ++ a ++ "$='" ++ v ++ "']"
    Selector.AttrContains a v -> "[" ++ a ++ "*='" ++ v ++ "']"
    Selector.AttrSpace    a v -> "[" ++ a ++ "~='" ++ v ++ "']"
    Selector.AttrHyph     a v -> "[" ++ a ++ "|='" ++ v ++ "']"
    Selector.Pseudo       a   -> ":" ++ a
    Selector.PseudoFunc   a args -> ":" ++ a ++ "(" ++ (String.join "," args) ++ ")"

{- Render a list of key-value mappings.
-}
renderProperties : Config -> List (Property.Key, Property.Value) -> String
renderProperties cfg propertyRules =
  propertyRules
    |> List.concatMap Property.managePrefixes -- each of the rules generates a `List Result`
    |> renderProperty cfg

{-  Takes a key-value property for a rule in the form of either a pair of
    key,value (Ok), or a prefixed key (Err). Returns a string representation
    of the property.
 -}
renderProperty : Config -> List (Result String (String, String)) -> String
renderProperty cfg results =
  let finalSemi  = if cfg.finalSemicolon then ";" else ""
      indent     = cfg.indentation
      endline    = cfg.newline
      goodKeys   = results |> List.filterMap Result.toMaybe |> List.map fst
      alignWidth = List.map String.length goodKeys
                      |> List.maximum
                      |> Maybe.withDefault (String.length indent)
                      |> ((+) 1)
      paddingFor theKey =
        if cfg.align
        then " " |> String.repeat (alignWidth - String.length theKey) --empty if length is negative
        else ""
      stringify result =
        case result of
          Ok (k, val) -> concat [indent, k, (paddingFor k), ":", cfg.sep, val]
          Err orphan ->
            if cfg.warn
            then indent ++ "/* no value for " ++ orphan ++ " */" ++ endline
            else ""
  in List.map stringify results
      |> String.join (";" ++ endline)
      |> \x -> x ++ finalSemi ++ endline

-------------------------------------------------------------------------------

{- Render a CSS @keyframes rule.
-}
renderKeyframes : Config -> Stylesheet.Keyframes -> String
renderKeyframes cfg (Stylesheet.Keyframes animationName listOfFrames) =
  Property.unPrefixed Common.browsers
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
renderKeyframe : Config -> (Float, (List Stylesheet.RuleData)) -> String
renderKeyframe cfg (percentage, keyframeRules) =
  concat
    [ percentage |> toString
    , "% "
    , renderRules cfg [] keyframeRules
    ]

{- Render a CSS @media rule.
-}
renderMedia : Config -> 
              Stylesheet.MediaQuery -> 
              List Selector.SelectorData -> 
              List Stylesheet.RuleData -> 
              String
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
renderMediaQuery : Stylesheet.MediaQuery -> String
renderMediaQuery (Stylesheet.MediaQuery notOrOnly typeOfMedia mediaFeatures) =
  concat
    [ "@media "
    , case notOrOnly of
        Nothing   -> ""
        Just (Stylesheet.Not)  -> "not "
        Just (Stylesheet.Only) -> "only "
    , renderMediaType typeOfMedia
    , mediaFeatures
        |> List.map renderMediaFeature
        |> List.map (\feature -> " and " ++ feature)
        |> concat
    ]

{- Render the media type in the media query part of a CSS @media rule.
-}
renderMediaType : Stylesheet.MediaType -> String
renderMediaType (Stylesheet.MediaType str) = str

{- Render a media feature in the media query part of a CSS @media rule.
-}
renderMediaFeature : Stylesheet.Feature -> String
renderMediaFeature (Stylesheet.Feature featureName maybeFeatureValue) =
  case maybeFeatureValue of
    Nothing        -> featureName
    Just str ->
      concat [ "(" , featureName , ": " , str , ")" ]

{- Render a CSS @font-face rule.
-}
renderFontFace : Config -> List Stylesheet.RuleData -> String
renderFontFace cfg faceRules =
  concat
    [ "@font-face"
    , renderRules cfg [] faceRules
    ]

{- Render a CSS import rule.
-}
renderImportRule : Config -> String -> String
renderImportRule cfg url =
  concat
    [ "@import url(\""
    , url
    , "\");"
    , cfg.newline
    ]

concat : List String -> String
concat = String.join ""
