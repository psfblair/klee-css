module Css.FontFace
  ( FontFaceFormat (..)
  , fontFaceSrc, localFontFaceSrc, urlFontFaceSrc
  ) where

import Css.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Stylesheet as Stylesheet
import Css.Internal.Utils as Utils

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

fontFaceSrc : List FontFaceSrc -> Stylesheet.PropertyRuleAppender
fontFaceSrc fontFaceSrcList =
  let srcListValue = Property.listValue ",\n      " fontFaceValue fontFaceSrcList
  in Stylesheet.simpleProperty "src" srcListValue

urlFontFaceSrc : String -> Maybe FontFaceFormat -> FontFaceSrc
urlFontFaceSrc url maybeFormat = FontFaceSrcUrl url maybeFormat

localFontFaceSrc : String -> FontFaceSrc
localFontFaceSrc fontName = FontFaceSrcLocal fontName

-- TODO only allow font-stretch, font-style, font-weight, unicode-range, font-feature-settings
-------------------------------------------------------------------------------

type FontFaceSrc
  = FontFaceSrcLocal String
  | FontFaceSrcUrl String (Maybe FontFaceFormat)

fontFaceValue : FontFaceSrc -> Property.Value 
fontFaceValue fontFaceSrc =
  let srcString = case fontFaceSrc of
    FontFaceSrcLocal name -> Common.call "local" (Utils.quote name)
    FontFaceSrcUrl url maybeFormat ->
      let toFormatString format = formatName format |> Utils.quote |> Common.call "format"
          formatString =
            maybeFormat |> Maybe.map toFormatString |> Maybe.withDefault ""
          urlString = Common.call "url" (Utils.quote url)
      in urlString ++ " " ++ formatString
  in Property.stringValue srcString

-- | name of format according to CSS specification
formatName : FontFaceFormat -> String
formatName format = case format of
  WOFF             -> "woff"
  TrueType         -> "truetype"
  OpenType         -> "opentype"
  EmbeddedOpenType -> "embedded-opentype"
  SVG              -> "svg"
