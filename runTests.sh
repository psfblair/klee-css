#!/bin/sh

rm tmp/out.js
rm elm.js
elm-make test/Main.elm
./elm-stuff/packages/maxsnew/IO/1.0.1/elm-io.sh elm.js tmp/out.js
node tmp/out.js 
