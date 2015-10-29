module Css.FontFace
  ( FontFaceFormat (..)
  , fontFaceSrc, localFontFaceSrc, urlFontFaceSrc
  ) where

import Css.Internal.Property exposing
  (Value, quote, stringValue, listValue)
import Css.Internal.Stylesheet exposing (PropertyRuleAppender, simpleProperty)

import Css.Common exposing (call)

-------------------------------------------------------------------------------
{- Example:
@font-face {
  font-family: 'MyWebFont';
  src: url('webfont.eot'); /* IE9 Compat Modes */
  src: url('webfont.eot?#iefix') format('embedded-opentype'), /* IE6-IE8 */
       url('webfont.woff2') format('woff2'), /* Super Modern Browsers */
       url('webfont.woff') format('woff'), /* Pretty Modern Browsers */
       url('webfont.ttf')  format('truetype'), /* Safari, Android, iOS */
       url('webfont.svg#svgFontName') format('svg'); /* Legacy iOS */
}
-}
type FontFaceFormat = WOFF | TrueType | OpenType | EmbeddedOpenType | SVG

fontFaceSrc : List FontFaceSrc -> PropertyRuleAppender
fontFaceSrc fontFaceSrcList =
  simpleProperty "src" (listValue ",\n      " fontFaceValue fontFaceSrcList)

urlFontFaceSrc : String -> Maybe FontFaceFormat -> FontFaceSrc
urlFontFaceSrc url maybeFormat = FontFaceSrcUrl url maybeFormat

localFontFaceSrc : String -> FontFaceSrc
localFontFaceSrc fontName = FontFaceSrcLocal fontName

-- TODO only allow font-stretch, font-style, font-weight, unicode-range, font-feature-settings
-------------------------------------------------------------------------------

type FontFaceSrc
  = FontFaceSrcLocal String
  | FontFaceSrcUrl String (Maybe FontFaceFormat)

fontFaceValue : FontFaceSrc -> Value 
fontFaceValue fontFaceSrc =
  let srcString = case fontFaceSrc of
    FontFaceSrcLocal name -> call "local" (quote name)
    FontFaceSrcUrl url maybeFormat ->
      let toFormatString format = formatName format |> quote |> call "format"
          formatString =
            maybeFormat |> Maybe.map toFormatString |> Maybe.withDefault ""
          urlString = call "url" (quote url)
      in urlString ++ " " ++ formatString
  in stringValue srcString

-- | name of format according to CSS specification
formatName : FontFaceFormat -> String
formatName format = case format of
  WOFF             -> "woff"
  TrueType         -> "truetype"
  OpenType         -> "opentype"
  EmbeddedOpenType -> "embedded-opentype"
  SVG              -> "svg"
