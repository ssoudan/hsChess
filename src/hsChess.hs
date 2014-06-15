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


-- TODO:
--
--    [√] add manual strategy
--    [√] improve UX
--    [√] Mac packaging
--    [√] store the history in the State rather than the latest state (using DList)
--    [√] add castling moves
--    [√] web or GUI
--    [ ] check detection and move selection
--    [ ] mate detection
--    [ ] stalemate detection
--    [ ] pawn conversion
--    [ ] parallelize the move evalution
--    [ ] start from predefined states
--    [ ] gamification: Achievements to learn chess


-- | Main method !
main :: IO ()
main = do
        banner
        --options <- getOptions
        let options = (Just NotAssisted, Just AB, Just GUI)
        case fromMaybe TextUI (thrd3 options) of TextUI -> tui options
                                                 GUI -> gui options

