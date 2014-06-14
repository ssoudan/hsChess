{-
 hsChess.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module Main where

import           Board            (PieceColor (..))
import           Data.Maybe       (fromMaybe)
import           InteractiveGame  as Paul
import           Minimax          as M
import           MinimaxAlphaBeta as AB
import           MinimaxLazy      as ML
import           State


-- TODO:
--
--    [√] add manual strategy
--    [√] improve UX
--    [√] Mac packaging
--    [√] store the history in the State rather than the latest state (using DList)
--    [√] add castling moves
--    [ ] check detection and move selection
--    [ ] mate detection
--    [ ] stalemate detection
--    [ ] pawn conversion
--    [ ] parallelize the move evalution
--    [ ] web or GUI
--    [ ] Start from predefined states
--    [ ] Gamification: Achievements to learn chess

-- | Play a turn based on the options that have been provided.
--
-- Basically, White player is a human player
-- Black player is an AI. You can select the one that will be used.
-- And as a human player, you can also have an AI that tell you what it would have played after you did it - yeah that's evil.
playATurn :: Options -> State -> IO State
playATurn options state = case getPlayer state of White -> do
                                                              s' <- whiteStrategy state
                                                              assistantStrategy state
                                                              putStr $ show s'
                                                              return s'
                                                  Black -> do
                                                              let s' = blackStrategy state
                                                              putStr $ show s'
                                                              return s'
                  where (assistantOption, opponentOption) = (fromMaybe NotAssisted $ fst options, fromMaybe AB $ snd options)
                        whiteStrategy = Paul.doMove
                        blackStrategy = case opponentOption of ML -> ML.doMove
                                                               AB -> AB.doMove
                                                               M -> M.doMove
                        assistantStrategy s = case assistantOption of AssistedML -> do
                                                                                      putStrLn "[ML] Thinking to what I would have played..."
                                                                                      (print . ML.doMove) s
                                                                                      putStrLn "[ML] But you played:"
                                                                      AssistedAB -> do
                                                                                      putStrLn "[AB] Thinking to what I would have played..."
                                                                                      (print . AB.doMove) s
                                                                                      putStrLn "[AB] But you played:"
                                                                      AssistedM -> do
                                                                                     putStrLn "[M] Thinking to what I would have played..."
                                                                                     (print . M.doMove) s
                                                                                     putStrLn "[M] But you played:"
                                                                      NotAssisted -> return ()


-- | Play the game for ever
--
-- TODO need to consider the end of game...
--
playForEver :: Options -> State -> IO State
playForEver options state = do
                              s <- playATurn options state
                              playForEver options s

-- | Play a predefined number of turns.
--
-- Essentially used for benchmarking
--
playForNTurns :: Int -> Options -> State -> IO State
playForNTurns 0 _ state = return state
playForNTurns n options state = do
                                 s <- playATurn options state
                                 playForNTurns (n-1) options s


-- | Banner display function
banner :: IO ()
banner = do
           putStrLn "                                                  "
           putStrLn "88  88 .dP\"Y8  dP\"\"b8 88  88 888888 .dP\"Y8 .dP\"Y8 "
           putStrLn "88  88 `Ybo.\" dP   `\" 88  88 88__   `Ybo.\" `Ybo.\" "
           putStrLn "888888 o.`Y8b Yb      888888 88\"\"   o.`Y8b o.`Y8b "
           putStrLn "88  88 8bodP'  YboodP 88  88 888888 8bodP' 8bodP' "
           putStrLn "                                       - @ssoudan "
           putStrLn "                                                  "


data AssistantOption = NotAssisted | AssistedM | AssistedML | AssistedAB deriving (Eq, Show, Read)
data OpponentOption = M | ML | AB deriving (Eq, Show, Read)

type Options = (Maybe AssistantOption, Maybe OpponentOption)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

-- | Interactive function to get the 'Option's.
getOptions :: IO Options
getOptions = do
               putStrLn "Assistant options are: "
               putStrLn " * NotAssisted: play like a grown up"
               putStrLn " * AssistedM: show the the M strategy would have played"
               putStrLn " * AssistedML: show the the ML strategy would have played"
               putStrLn " * AssistedAB: show the the AB strategy would have played"
               putStrLn "Which one do you want?"
               assistantOption <- getLine
               putStrLn " * M: play against the Minimax strategy"
               putStrLn " * ML: play against the MinimaxLazy strategy"
               putStrLn " * AB: play against the AlphaBeta strategy"
               putStrLn "Which one do you want to be defeated by?"
               opponentOption <- getLine
               return (readMaybe assistantOption :: Maybe AssistantOption, readMaybe opponentOption :: Maybe OpponentOption)

-- | Main method !
main :: IO ()
main = do
        banner
        options <- getOptions
        --let options = (NotAssisted, AB)
        putStr $ show newState
        --endState <- playForNTurns 4 options initState
        endState <- playForEver options newState
        putStr $ show endState

