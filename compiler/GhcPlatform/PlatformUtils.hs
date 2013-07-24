module PlatformUtils where

import LoadLibraries as Libs
import System.IO.Unsafe (unsafePerformIO)

loadDocs :: String
loadDocs = Libs.docs

printWarning :: String -> ()
printWarning msg = unsafePerformIO (putStrLn msg) `seq` ()