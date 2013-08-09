Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).


## Elm-Haste (Elm compiler in JavaScript)

This branch contains a modified version of the [Elm compiler](http://github.com/evancz/Elm), which can be compiled by [Haste](http://github.com/valderman/haste-compiler) and run in the browser.

### Proof of concept

This is only a proof of concept. Running the Elm compiler in JavaScript is much slower than native, and much less flexible.

Features currently not supported:
 * Compiling multiple Elm modules together (all source code must be a single string)
 * Markdown syntax (ie. Elm's `[markdown| ... |]`). This is because [PanDoc for Haskell](http://hackage.haskell.org/package/pandoc) is a huge package that depend on file-system APIs.

### Try it out

Online demo coming soon.

### Compiling

You will need the git version of the Haste compiler.
```
$ git clone https://github.com/valderman/haste-compiler.git
$ cd haste-compiler
$ cabal install
$ haste-boot --force --local
```

You can check that the correct version of the compiler is available by looking for the --full-unicode option. To compile Elm we need this option.
```
$ hastec --help | grep full-unicode
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

You can find the compiled JavaScript in: `compiler/JsMain/ElmJsMain.js`
