module Css.Render where

import Css.Stylesheet hiding (Child, query, rule)
import Css.Common (browsers)
import Css.Property
import Css.Selector

import qualified Clay.Stylesheet as Rule

type alias Config = 
  { indentation    : String
  , newline        : String
  , sep            : String
  , finalSemicolon : Bool
  , warn           : Bool
  , align          : Bool
  , banner         : Bool
  }

{-| Configuration to print to a pretty human readable CSS output. -}

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

{-| Configuration to print to a compacted unreadable CSS output. -}

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

{-| Render a stylesheet with the default configuration. The pretty printer is
used by default. -}

render : Css -> String
render = renderWith pretty []
         
{-| Render a stylesheet with a custom configuration and an optional outer scope. -}

renderWith : Config -> [App] -> Css -> String
renderWith cfg outerScope css
  = runS css
  |> rules cfg outerScope
  |> renderBanner cfg

-------------------------------------------------------------------------------

{-| Render a single CSS `Selector`. -}

renderSelector : Selector -> String
renderSelector = selector compact

renderBanner : Config -> String -> String
renderBanner cfg =
  if | banner cfg -> (++ "\n/* Generated with elm-css */")
     | otherwise  -> id

{-| Render a CSS @keyframes rule. -}

kframe : Config -> Keyframes -> String
kframe cfg (Keyframes animationName listOfFrames) =
  unPrefixed browsers
  |> List.map
    ( \(browser, _) ->
      concat [ "@" ++ browser ++ "keyframes "
              , animationName
              , newline cfg
              , "{"
              , newline cfg
              , (listOfFrames |> List.map (frame cfg) |> concat)
              , "}"
              , newline cfg
              , newline cfg
              ]
      )
  |> concat

{-| Render one frame in a CSS @keyframes rule. -}

frame : Config -> (Float, [Rule]) -> String
frame cfg (percentage, mediaRules) =
  concat
    [ percentage |> toString
    , "% "
    , rules cfg [] mediaRules
    ]

{-| Render a CSS @media rule. -}

query : Config -> MediaQuery -> [App] -> [Rule] -> String
query cfg query sel mediaRules =
  concat
    [ mediaQuery query
    , newline cfg
    , "{"
    , newline cfg
    , rules cfg sel mediaRules
    , "}"
    , newline cfg
    ]

{-| Render the media query part of a CSS @media rule. -}

mediaQuery : MediaQuery -> String
mediaQuery (MediaQuery notOrOnly typeOfMedia mediaFeatures) = mconcat
  [ "@media "
  , case notOrOnly of
      Nothing   -> ""
      Just Not  -> "not "
      Just Only -> "only "
  , mediaType typeOfMedia
  , mediaFeatures
      |> List.map feature
      |> List.map (" and " ++)
      |> concat
  ]

{-| Render the media type in the media query part of a CSS @media rule. -}

mediaType : MediaType -> String
mediaType (MediaType (Value v)) = plain v

{-| Render a media feature in the media query part of a CSS @media rule. -}

feature : Feature -> String
feature (Feature featureName maybeFeatureValue) =
  case maybeFeatureValue of
    Nothing        -> featureName
    Just (Value v) ->
      concat [ "(" , featureName , ": " , (plain v) , ")" ]

{-| Render a CSS @font-face rule. -}

face : Config -> [Rule] -> String
face cfg faceRules =
  concat
    [ "@font-face"
    , rules cfg [] faceRules
    ]

rules : Config -> [App] -> [Rule] -> String
rules cfg sel rs = mconcat
  [ rule cfg sel (mapMaybe property rs)
  , newline cfg
  ,             imp    cfg              `foldMap` mapMaybe imports rs
  ,             kframe cfg              `foldMap` mapMaybe kframes rs
  ,             face   cfg              `foldMap` mapMaybe faces   rs
  , (\(a, b) -> rules  cfg (a : sel) b) `foldMap` mapMaybe nested  rs
  , (\(a, b) -> query  cfg  a   sel  b) `foldMap` mapMaybe queries rs
  ]
  where property (Property k v) = Just (k, v)
        property _              = Nothing
        nested   (Nested a ns ) = Just (a, ns)
        nested   _              = Nothing
        queries  (Query q ns  ) = Just (q, ns)
        queries  _              = Nothing
        kframes  (Keyframe fs ) = Just fs;
        kframes  _              = Nothing
        faces    (Face ns     ) = Just ns
        faces    _              = Nothing
        imports  (Import i    ) = Just i
        imports  _              = Nothing

imp : Config -> String -> String
imp cfg t =
  mconcat
    [ "@import url("
    , fromText t
    , ");"
    , newline cfg ]


rule : Config -> [App] -> [(Key (), Value)] -> String
rule _   _   []    = mempty
rule cfg sel props =
  let xs = collect =<< props
   in mconcat
      [ selector cfg (merger sel)
      , newline cfg
      , "{"
      , newline cfg
      , properties cfg xs
      , "}"
      , newline cfg
      ]

merger : [App] -> Selector
merger []     = "" -- error "this should be fixed!"
merger (x:xs) =
  case x of
    Rule.Child s -> case xs of [] -> s; _  -> merger xs |> s
    Sub        s -> case xs of [] -> s; _  -> merger xs ** s
    Root       s -> s ** merger xs
    Pop        i -> merger (drop i (x:xs))
    Self       f -> case xs of [] -> star `with` f; _ -> merger xs `with` f

collect : (Key (), Value) -> [Either String (String, String)]
collect (Key ky, Value vl) =
  case (ky, vl) of
    ( Plain    k  , Plain    v  ) -> [prop k v]
    ( Prefixed ks , Plain    v  ) -> flip map ks $ \(p, k) -> prop (p <> k) v
    ( Plain    k  , Prefixed vs ) -> flip map vs $ \(p, v) -> prop k (p <> v)
    ( Prefixed ks , Prefixed vs ) -> flip map ks $ \(p, k) -> (Left (p <> k) `maybe` (prop (p <> k) . mappend p)) (lookup p vs)
  where prop k v = Right (k, v)

properties : Config -> [Either String (String, String)] -> String
properties cfg xs =
  let width     = 1 + maximum (String.length . fst <$> rights xs)
      ind       = indentation cfg
      new       = newline cfg
      finalSemi = if finalSemicolon cfg then ";" else ""
   in (<> new) $ (<> finalSemi) $ intersperse (";" <> new) $ flip map xs $ \p ->
        case p of
          Left w -> if warn cfg
                    then ind <> "/* no value for " <> fromText w <> " */" <> new
                    else mempty
          Right (k, v) ->
            let pad = if align cfg
                      then fromText (String.replicate (width - String.length k) " ")
                      else ""
             in mconcat [ind, fromText k, pad, ":", sep cfg, fromText v]

selector : Config -> Selector -> String
selector cfg = intersperse ("," <> newline cfg) . rec
  where rec (In (SelectorF (Refinement ft) p)) = (<> foldMap predicate (sort ft)) <$>
          case p of
            Star           -> if null ft then ["*"] else [""]
            Elem t         -> [fromText t]
            Child      a b -> ins " > " <$> rec a <*> rec b
            Deep       a b -> ins " "   <$> rec a <*> rec b
            Adjacent   a b -> ins " + " <$> rec a <*> rec b
            Combined   a b -> rec a ++ rec b
          where ins s a b = a <> s <> b

predicate : Predicate -> String
predicate ft = mconcat $
  case ft of
    Id           a   -> [ "#", fromText a                                             ]
    Class        a   -> [ ".", fromText a                                             ]
    Attr         a   -> [ "[", fromText a,                     "]"                    ]
    AttrVal      a v -> [ "[", fromText a,  "='", fromText v, "']"                    ]
    AttrBegins   a v -> [ "[", fromText a, "^='", fromText v, "']"                    ]
    AttrEnds     a v -> [ "[", fromText a, "$='", fromText v, "']"                    ]
    AttrContains a v -> [ "[", fromText a, "*='", fromText v, "']"                    ]
    AttrSpace    a v -> [ "[", fromText a, "~='", fromText v, "']"                    ]
    AttrHyph     a v -> [ "[", fromText a, "|='", fromText v, "']"                    ]
    Pseudo       a   -> [ ":", fromText a                                             ]
    PseudoFunc   a p -> [ ":", fromText a, "(", intersperse "," (map fromText p), ")" ]


