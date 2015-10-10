module Css.ColorTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Internal.Property exposing (stringValueFactory)

import Css.Color exposing (..)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.ColorTests"
  [ describe "parse"
    [ parse "#FF6666" `shouldEqual` Ok (Rgba 255 102 102 1.0)
    , parse "444" `shouldEqual` Ok (Rgba 68 68 68 1.0)
    , parse "440220" `shouldEqual` Ok (Rgba 68 2 32 1.0)
    ]
  , describe "toRgba"
    [ it "gives back the same Rgba"
        [  toRgba (Rgba 255 255 255 1.0) `shouldEqual` Rgba 255 255 255 1.0 ]
    , it "will not accept an Other"
        [  toRgba (Other <| stringValueFactory.value "auto") `shouldEqual` toRgba (Other <| stringValueFactory.value "auto") ]
    , it "translates a whiteRgba Hsla"
        [  toRgba (Hsla 360 1.0 1.0 1.0) `shouldEqual` Rgba 255 255 255 1.0 ]
    , it "translates a blackRgba Hsla"
        [  toRgba (Hsla 0 0.0 0.0 1.0) `shouldEqual` Rgba 0 0 0 1.0 ]
    , it "translates a red Hsla"
        [  toRgba (Hsla 0 1.0 0.5 1.0) `shouldEqual` Rgba 255 0 0 1.0 ]
    , it "translates a green Hsla"
        [  toRgba (Hsla 120 1.0 0.5 1.0) `shouldEqual` Rgba 0 255 0 1.0 ]
    , it "translates a blue Hsla"
        [  toRgba (Hsla 240 1.0 0.5 1.0) `shouldEqual` Rgba 0 0 255 1.0 ]
    , it "translates a random Hsla color"
        [  toRgba (Hsla 99 0.61 0.82 0.478) `shouldEqual` Rgba 200 237 181 0.478
        ,  toRgba (Hsla 154 0.79 0.46 1.0) `shouldEqual` Rgba 24 209 129 1.0
        ]
    ]
  , describe "toHsla"
    [ it "gives back the same Hsla"
        [  toHsla (Hsla 0 0.0 0.0 0) `shouldEqual` Hsla 0 0.0 0.0 0 ]
    , it "will not accept an Other"
        [  toHsla (Other <| stringValueFactory.value "auto") `shouldEqual`
              toHsla (Other <| stringValueFactory.value "auto") ]
    , it "translates a whiteRgba Rgba"
        [  toHsla (Rgba 255 255 255 1.0) `shouldEqual` Hsla 0 0.0 1.0 1.0 ]
    , it "translates a blackRgba Rgba"
        [  toHsla (Rgba 0 0 0 1.0) `shouldEqual` Hsla 0 0.0 0.0 1.0 ]
    , it "translates a red Rgba"
        [  toHsla (Rgba 255 0 0 1.0) `shouldEqual` Hsla 0 1.0 0.5 1.0 ]
    , it "translates a green Rgba"
        [  toHsla (Rgba 0 255 0 1.0) `shouldEqual` Hsla 120 1.0 0.5 1.0 ]
    , it "translates a blue Rgba"
        [  toHsla (Rgba 0 0 255 1.0) `shouldEqual` Hsla 240 1.0 0.5 1.0 ]
    , it "translates a random Rgba color"
        [  toHsla (Rgba 201 237 181 0.478) `shouldEqual` Hsla 98 0.609 0.82 0.478
        ,  toHsla (Rgba 20 168 104 1.0) `shouldEqual` Hsla 154 0.787 0.369 1.0
        ]
    ]
  , let whiteRgba = Rgba 255 255 255 1.0
        blackRgba = Rgba 0 0 0 1.0
        transparentRgba = Rgba 255 255 255 0
        transparentBlack = Rgba 0 0 0 0
        halfTransparent = Rgba 128 128 128 0.5

        whiteHsla = Hsla 0 0 1.0 1.0
        blackHsla = Hsla 0 0 0 1.0
        transparentHsla = Hsla 0 0 1.0 0
        transparentBlackHsla = Hsla 0 0 0 0
    in describe "lerp"
        [ it "gives the same HSL color back"
            [  lerp 0.5 (Hsla 100 0.5 0.5 1.0) (Hsla 100 0.5 0.5 1.0)
                  `shouldEqual` Hsla 99 0.504 0.498 1.0 ]
        , it "gives the same RGB color back"
            [  lerp 0.2 (Rgba 201 237 181 0.478) (Rgba 201 237 181 0.478)
                  `shouldEqual` Rgba 201 237 181 0.48 ]
        , it "gives the same color back at zero"
            [ lerp 0 transparentRgba whiteRgba `shouldEqual` transparentRgba
            , lerp 0.001 transparentRgba whiteRgba `shouldEqual` transparentRgba
            , lerp (-0.001) transparentRgba whiteRgba `shouldEqual` transparentRgba
            , lerp 0 transparentHsla whiteHsla `shouldEqual` transparentHsla
            , lerp 0.001 transparentHsla whiteHsla `shouldEqual` transparentHsla
            , lerp (-0.001) transparentHsla whiteHsla `shouldEqual` transparentHsla
            ]
        , it "gives the same color back at negative 1 if the color is lower than the bound"
            [ lerp (-1.0) transparentRgba whiteRgba `shouldEqual` transparentRgba
            , lerp (-1.0) transparentHsla whiteHsla `shouldEqual` transparentHsla
            ]
        , it "gives the bound color at 100%"
            [ lerp 1.0 transparentRgba whiteRgba `shouldEqual` whiteRgba
            , lerp 1.001 transparentRgba whiteRgba `shouldEqual` whiteRgba
            , lerp 2.0 transparentRgba whiteRgba `shouldEqual` whiteRgba
            , lerp 0.999 transparentRgba whiteRgba `shouldEqual` Rgba 255 255 255 1.0
            , lerp 1.0 transparentHsla whiteHsla `shouldEqual` whiteHsla
            , lerp 1.001 transparentHsla whiteHsla `shouldEqual` whiteHsla
            , lerp 2.0 transparentHsla whiteHsla `shouldEqual` whiteHsla
            , lerp 0.999 transparentHsla whiteHsla `shouldEqual` Hsla 0 0.0 1.0 1.0
            ]
        , it "gives the right halfway color"
            [ lerp 0.5 blackRgba whiteRgba `shouldEqual` Rgba 128 128 128 1.0
            , lerp 0.5 transparentBlack whiteRgba `shouldEqual` halfTransparent
            , lerp 0.501 blackRgba whiteRgba `shouldEqual` Rgba 128 128 128 1.0
            , lerp 0.499 blackRgba whiteRgba `shouldEqual` Rgba 127 127 127 1.0
            , lerp 0.5 blackHsla whiteHsla `shouldEqual` Hsla 0 0.0 0.502 1.0
            , lerp 0.5 transparentBlackHsla whiteHsla `shouldEqual` Hsla 0 0.0 0.502 0.5
            , lerp 0.501 blackHsla whiteHsla `shouldEqual` Hsla 0 0.0 0.502 1.0
            , lerp 0.499 blackHsla whiteHsla `shouldEqual` Hsla 0 0.0 0.498 1.0
            ]
        , it "should lerp backwards"
            [ lerp 0 whiteRgba blackRgba `shouldEqual` whiteRgba
            , lerp 1.0 whiteRgba blackRgba `shouldEqual` blackRgba
            , lerp 0.5 whiteRgba blackRgba `shouldEqual` Rgba 128 128 128 1.0
            , lerp 0 whiteHsla blackHsla `shouldEqual` whiteHsla
            , lerp 1.0 whiteHsla blackHsla `shouldEqual` blackHsla
            , lerp 0.5 whiteHsla blackHsla`shouldEqual` Hsla 0 0.0 0.502 1.0
            ]
        , it "should lerp down"
            [ lerp (-1.0) blackRgba whiteRgba `shouldEqual` blackRgba
            , lerp (-0.5) blackRgba whiteRgba `shouldEqual` blackRgba
            , lerp (-0.5) (Rgba 127 127 127 1.0) whiteRgba `shouldEqual` Rgba 63 63 63 1.0
            , lerp (-1.0) blackHsla whiteHsla  `shouldEqual` blackHsla
            , lerp (-0.5) blackHsla whiteHsla `shouldEqual` blackHsla
            , lerp (-0.5) (Hsla 0 0.0 0.5 0.5) whiteHsla `shouldEqual` Hsla 0 0.0 0.247 0.25
            ]
        , it "should lerp a red color"
            [ lerp 0.5 (Rgba 255 0 0 1.0) whiteRgba `shouldEqual` Rgba 255 128 128 1.0
            , lerp (-0.5) (Rgba 255 0 0 1.0) whiteRgba `shouldEqual` Rgba 255 0 0 1.0
            , lerp 0.5 (Rgba 255 0 0 1.0) blackRgba `shouldEqual` Rgba 128 0 0 1.0
            , lerp (-0.5) (Rgba 255 0 0 1.0) blackRgba `shouldEqual` Rgba 255 0 0 1.0

            , lerp 0.5 (Hsla 0 1.0 0.5 1.0) whiteRgba `shouldEqual` Hsla 0 1.0 0.751 1.0
            , lerp 0.5 (Hsla 0 1.0 0.5 1.0) blackRgba `shouldEqual` Hsla 0 1.0 0.251 1.0
            ]
        , it "should lerp a green color"
            [ lerp 0.5 (Rgba 0 255 0 1.0) whiteRgba `shouldEqual` Rgba 128 255 128 1.0
            , lerp (-0.5) (Rgba 0 255 0 1.0) whiteRgba `shouldEqual` Rgba 0 255 0 1.0
            , lerp 0.5 (Rgba 0 255 0 1.0) blackRgba `shouldEqual` Rgba 0 128 0 1.0

            , lerp 0.5 (Hsla 120 1.0 0.5 1.0) whiteRgba `shouldEqual` Hsla 120 1.0 0.751 1.0
            , lerp 0.5 (Hsla 120 1.0 0.5 1.0) blackRgba `shouldEqual` Hsla 120 1.0 0.251 1.0
            ]
        , it "should lerp a blue color"
            [ lerp 0.5 (Rgba 0 0 255 1.0) whiteRgba `shouldEqual` Rgba 128 128 255 1.0
            , lerp (-0.5) (Rgba 0 0 255 1.0) whiteRgba `shouldEqual` Rgba 0 0 255 1.0
            , lerp 0.5 (Rgba 0 0 255 1.0) blackRgba `shouldEqual` Rgba 0 0 128 1.0

            , lerp 0.5 (Hsla 240 1.0 0.5 1.0) whiteRgba `shouldEqual` Hsla 240 1.0 0.751 1.0
            , lerp 0.5 (Hsla 240 1.0 0.5 1.0) blackRgba `shouldEqual` Hsla 240 1.0 0.251 1.0
            ]
        , it "should lerp a random color"
            [  lerp 0.5 (Rgba 201 237 181 0.478) whiteRgba `shouldEqual` Rgba 228 246 218 0.74
            ,  lerp 0.5 (Rgba 201 237 181 0.478) blackRgba `shouldEqual` Rgba 101 119 91 0.74

            , lerp 0.5 (Hsla 99 0.61 0.82 0.478) whiteRgba `shouldEqual` Hsla 98 0.609 0.91 0.74
            , lerp 0.5 (Hsla 99 0.61 0.82 0.478) blackRgba `shouldEqual` Hsla 100 0.133 0.412 0.74
            ]
        ]
  , describe "lighten"
    [ it "lightens an HSL color by a factor"
        [  lighten 0.2 (Hsla 154 0.79 0.46 1.0) `shouldEqual` Hsla 154 0.667 0.565 1.0 ]
    , it "lightens an RGB color by a factor"
        [  lighten 0.2 (Rgba 25 210 130 1.0) `shouldEqual` Rgba 71 219 155 1.0 ]
    , it "does not lighten an HSL color past white"
        [  lighten 0.8 (Hsla 154 0.79 0.46 1.0) `shouldEqual` Hsla 154 0.673 0.892 1.0 ]
    , it "does not lighten an RGB color past white"
        [  lighten 0.8 (Rgba 25 210 130 1.0) `shouldEqual` Rgba 209 246 230 1.0 ]
    ]
  , describe "darken"
    [  it "darken an HSL color by a factor"
        [  darken 0.2 (Hsla 154 0.79 0.46 1.0) `shouldEqual` Hsla 154 0.796 0.365 1.0 ]
    , it "darken an RGB color by a factor"
        [  darken 0.2 (Rgba 25 210 130 1.0) `shouldEqual` Rgba 20 168 104 1.0 ]
    , it "does not darken an HSL color past black"
        [  darken 0.8 (Hsla 154 0.79 0.46 1.0) `shouldEqual` Hsla 154 0.787 0.092 1.0 ]
    , it "does not darken an RGB color past black"
        [  darken 0.8 (Rgba 25 210 130 1.0) `shouldEqual` Rgba 5 42 26 1.0 ]
    ]
  ]
