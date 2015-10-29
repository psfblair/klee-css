module MyStyleConstants where

import Css exposing (..)
import Css.Border exposing (..)
import Css.Elements exposing (..)
import Css.Display exposing (..)
import Css.Color exposing (..)

-------------------------------------------------------------------------------

{-| This module contains constants and styles that might be shared by styles 
used to attach style attributes to elements within an Elm program as well as 
in stylesheets generated on the command line by the preprocessor. 
-}
siteFontFamily = "Bitstream Vera Serif Bold"

paragraphBorderStyle = solid

paragraphBorderColor = green

shinyLink = a [ custom "-ms-lens-flare-style" "really-shiny" ] []

errorParagraph =
  (p `byClass` "error") 
      [ float floatLeft ] 
      [ (a `byId` "fred") [ clear both ] [] ] 
