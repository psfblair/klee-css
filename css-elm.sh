#!/bin/bash

rm -f elm.js
elm-make $@ 1>&2

node <(./elm-stuff/packages/maxsnew/IO/1.0.1/elm-io.sh elm.js >(cat))
