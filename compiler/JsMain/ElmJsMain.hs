module Main where

import Initialize (buildFromSource)
import CompileToJS (showErr, jsModule)
import Haste (toJSString, fromJSString, JSString, mkCallback)
import Haste.Prim (Ptr)

foreign import ccall consoleLog :: JSString -> IO ()

main = do
  consoleLog $ elmCompile (toJSString "import Mouse\nmain = asText <~ Mouse.position")

elmCompile :: JSString -> JSString
elmCompile src = 
  case fromJSString src of
    Nothing -> toJSString "Error converting JSString to Haskell"
    Just s -> toJSString (compileToHtml s) 

compileToHtml :: String -> String
compileToHtml source = either errToStr modToStr modul
    where
      errToStr = elmToHtmlError . showErr
      modToStr = elmToHtmlJs . jsModule 
      modul = buildFromSource True source

elmToHtmlError err = baseHtml $
  "<span style\"font-family: monospace;\">" ++
  (concatMap (++"<br>") (lines err)) ++ 
  "</span>"

elmToHtmlJs jsSrc =
  "<script type=\"text/javascript\" src=\"/elm-mini.js\"></script>" ++
  "<script type=\"text/javascript\">" ++ 
  --"window.onload = function() {" ++
  jsSrc ++
  --"; nowInit(); };" ++
  "</script>" ++
  "<script type=\"text/javascript\">var runningElmModule = Elm.fullscreen(Elm.Main)</script>"

baseHtml content =
  "<!DOCTYPE html>" ++
  "<html>" ++
  "<head>" ++
  "<meta charset=\"UTF-8\">" ++
  "<title>Compiled Elm</title>" ++
  "</head>" ++
  "<body>" ++
  content ++
  "</body>" ++
  "</html>"
