module Css.FontFace
  ( FontFaceFormat (..)
  , FontFaceSrc (..)
  , fontFaceSrc
  ) where

import Css.Internal.Property exposing
  (PrefixedOrNot, Value, quote, stringValue, commaListValue)
import Css.Internal.Stylesheet exposing (CssGenerator, simpleProperty)

import Css.Common exposing (call)

-------------------------------------------------------------------------------

type FontFaceSrc
  = FontFaceSrcLocal String
  | FontFaceSrcUrl String (Maybe FontFaceFormat)

type FontFaceFormat = WOFF | TrueType | OpenType | EmbeddedOpenType | SVG

fontFaceSrc : List FontFaceSrc -> PropertyRuleAppender
fontFaceSrc fontFaceSrcList =
  key (stringKey "src") (commaListValue fontFaceValue fontFaceSrcList)

-------------------------------------------------------------------------------

fontFaceValue : Value FontFaceSrc
fontFaceValue =
  { value fontFaceSrc =
      let srcString = case fontFaceSrc of
        FontFaceSrcLocal name -> call "local" (quote name)
        FontFaceSrcUrl url maybeFormat ->
          let toFormatString format = formatName format |> quote |> call "format"
              formatString =
                maybeFormat |> Maybe.map toFormatString |> Maybe.withDefault ""
              urlString = call "url" (quote url)
          in urlString ++ formatString
      in stringValue.value srcString
  }

-- | name of format according to CSS specification
formatName : FontFaceFormat -> String
formatName format = case format of
  WOFF             -> "woff"
  TrueType         -> "truetype"
  OpenType         -> "opentype"
  EmbeddedOpenType -> "embedded-opentype"
  SVG              -> "svg"
