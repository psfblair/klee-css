#!/bin/bash

rm elm.js
elm-make test/Main.elm

node <(./elm-stuff/packages/maxsnew/IO/1.0.1/elm-io.sh elm.js >(cat))

