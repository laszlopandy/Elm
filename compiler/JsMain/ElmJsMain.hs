module Main where

import Build (buildFromSource)
import CompileToJS (showErr, jsModule)
import Haste (toJSString, fromJSString, JSString, mkCallback)
import Haste.Prim (Ptr, toPtr)

foreign import ccall consoleLog :: JSString -> IO ()
foreign import ccall installCallback :: JSString -> (Ptr (JSString -> JSString)) -> IO ()

main = do
  installCallback (toJSString "elmCompile") (toPtr elmCompile)

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

elmToHtmlError = elmToHtmlJs

elmToHtmlJs jsSrc = baseHtml $
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
