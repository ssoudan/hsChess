{-
 Minimax.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module Minimax where

import           Board
import           Data.List
import           Debug.Trace
import           Move

data GameTree = GameTree {state::State, gameTree::[GameTree]} deriving Show

play :: GameTree->Int
play gt@(GameTree (State _ _ p) _) = play_aux (p == Black) gt where
                  play_aux _ (GameTree (State c _ _) []) = evalBoard c
                  play_aux True (GameTree _ xs) = maximum (map (play_aux False) xs)
                  play_aux False (GameTree _ xs) = minimum (map (play_aux True) xs)

buildGameTree :: Int -> State -> GameTree
buildGameTree 0 s = GameTree s []
buildGameTree depth s = let states = nextStates s
                             in GameTree s (map (buildGameTree (depth - 1)) states)

-- buildGameTree 2 (State initialBoard White)

compareGT :: GameTree -> GameTree -> Ordering
compareGT s1 s2 = play s1 `compare` play s2


compareOption :: (Int, GameTree) -> (Int, GameTree) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

doMove :: State -> State
doMove s@(State _ _ p) = case p of Black -> state (snd (maximumBy compareOption options))
                                   White -> state (snd (minimumBy compareOption options))
                   where gt = gameTree $ buildGameTree 4 s
                         doPlay = \g -> (play g, g)
                         options = map doPlay gt

-- doMove (State initialBoard White)
