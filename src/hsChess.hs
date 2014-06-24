{-
 hsChess.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module Main where


import           Data.Maybe (fromMaybe)
import           GUI
import           Options
import           TUI
import           Utils


-- | Main function!
main :: IO ()
main = do
        banner
        --options <- getOptions
        let options = (Just NotAssisted, Just AB, Just GUI)
        case fromMaybe TextUI (thrd3 options) of TextUI -> tui options
                                                 GUI -> gui options

