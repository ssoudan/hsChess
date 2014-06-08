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

apps = map (mkApp ["hsChess.sh"] ) $ words "hsChess"

mkApp files name = MacApp name (Just "icons/hsChess.icns") (Just "Info.plist") files [] ChaseWithDefaults