-- Build .app bundles with the cabal-macosx package
-- Modeled after the excellent documentation on 
-- https://github.com/gimbo/cabal-macosx/tree/master/examples

import Distribution.MacOSX as Mac 
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postBuild = myPostBuild -- no-op if not MacOS X
    }   

myPostBuild = Mac.appBundleBuildHook apps 

apps = map (mkApp [ "hsChess.sh"
                  , "icons/pieces/small/WhiteK.png" 
                  , "icons/pieces/small/WhiteQ.png"
                  , "icons/pieces/small/WhiteB.png"
                  , "icons/pieces/small/WhiteR.png"
                  , "icons/pieces/small/WhiteP.png"
                  , "icons/pieces/small/WhiteN.png"
                  , "icons/pieces/small/BlackK.png"
                  , "icons/pieces/small/BlackQ.png"
                  , "icons/pieces/small/BlackB.png"
                  , "icons/pieces/small/BlackR.png"
                  , "icons/pieces/small/BlackP.png"
                  , "icons/pieces/small/BlackN.png" ] ) $ words "hsChess"

mkApp files name = MacApp name (Just "icons/hsChess.icns") (Just "Info.plist") files [] ChaseWithDefaults