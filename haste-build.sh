#!/bin/sh

pushd compiler

pushd JsMain
cat extern.js > all.js

printf "\nvar __elmDocs__ = " >> all.js
cat docs.json >> all.js
popd

hastec --debug JsMain/ElmJsMain.hs -iTransform:Gen:Model:HastePlatform -hide-package monads-tf-0.1.0.1 --with-js=JsMain/all.js

popd