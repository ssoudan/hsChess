{-
 GUIUtils.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module GUIUtils where

import System.FilePath
import System.IO.Unsafe

import System.Environment.Executable

getDataDir :: IO FilePath
getDataDir = fmap (\x -> takeDirectory x </> ".." </> "Resources") getExecutablePath 
    
getDataFile :: FilePath -> FilePath
getDataFile x = unsafePerformIO $ fmap (</> x) getDataDir
