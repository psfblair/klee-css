module Main where

import IO.IO exposing (IO)
import IO.Runner

import Css exposing (..)
import Css.Border exposing (..)
import Css.Elements exposing (..)
import Css.FontFace exposing (..)
import Css.Font exposing (..)

import MyStyleConstants exposing (..)

-------------------------------------------------------------------------------

{-| To render this stylesheet from the command line, run the following command
from the root directory of this project:

    ./klee example/MyStylesheet/Main.elm example/MyStyleConstants.elm > MyStylesheet.css

Note that for IO to take place, the module must be named Main. Therefore, if
we have multiple stylesheets to render we have multiple Main modules in different
subdirectories named for the stylesheet name. 
-}
imports = 
  [ importUrl "http://some.stylesheet.com" 
  , importUrl "http://some.stylesheet.org" 
  ]
  
fontFaces = 
  [ fontFace 
    [ fontFamily [ siteFontFamily ] []
    , fontFaceSrc 
      [ localFontFaceSrc "VeraBold.woff"
      , urlFontFaceSrc "http://font.url" (Just TrueType)
      ]
    ]
  ]

properties =
    [ p [ borderStyle paragraphBorderStyle, borderColor paragraphBorderColor  ]
        [ shinyLink ]
    , errorParagraph   
    ]

stylesheet = imports |> append fontFaces |> append properties

-------------------------------------------------------------------------------

generate : IO ()
generate = render stylesheet |> IO.IO.putStr

port requests : Signal IO.Runner.Request
port requests = IO.Runner.run responses generate

port responses : Signal IO.Runner.Response
