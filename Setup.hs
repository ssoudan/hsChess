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
                  , "icons/pieces/small/WK.png" 
                  , "icons/pieces/small/WQ.png"
                  , "icons/pieces/small/WB.png"
                  , "icons/pieces/small/WR.png"
                  , "icons/pieces/small/WP.png"
                  , "icons/pieces/small/WN.png"
                  , "icons/pieces/small/BK.png"
                  , "icons/pieces/small/BQ.png"
                  , "icons/pieces/small/BB.png"
                  , "icons/pieces/small/BR.png"
                  , "icons/pieces/small/BP.png"
                  , "icons/pieces/small/BN.png" ] ) $ words "hsChess"

mkApp files name = MacApp name (Just "icons/hsChess.icns") (Just "Info.plist") files [] DoNotChase