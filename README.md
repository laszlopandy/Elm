Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).


## Elm-Haste (Elm compiler in JavaScript)

This branch contains a modified version of the [Elm compiler](http://github.com/evancz/Elm), which can be compiled by [Haste](http://github.com/valderman/haste-compiler) and run in the browser.

### Limitations

This is only a proof of concept. Running the Elm compiler in JavaScript is much slower than native, and much less flexible.

Current limitations:
 * There is no way to compile multiple Elm modules together (all source code must be a single string)
 * Elm's markdown syntax (`[markdown| ... |]`) is not converted, because [Pandoc](http://hackage.haskell.org/package/pandoc) is a monolithic package which depends heavily on file-system APIs
 * It is not possible to compile Elm programs longer than a few hundred characters ([haste bug #77][bug])

[bug]: https://github.com/valderman/haste-compiler/issues/77

### Try it out

Online demo coming soon, once [haste bug #77][bug] is fixed.

### Compiling

You will need the git version of the Haste compiler.
```
$ git clone https://github.com/valderman/haste-compiler.git
$ cd haste-compiler
$ cabal install
$ haste-boot --force --local
```

Once you have `hastec`, we need to install Elm's dependencies.
```
$ haste-inst install parsec indents
```

Finally, you can compile Elm.
```
$ git clone http://github.com/laszlopandy/Elm.git
$ cd Elm
$ git checkout elm-haste
$ sh haste-build.sh
```

You can find the compiled JavaScript file at: `compiler/JsMain/ElmJsMain.js`
