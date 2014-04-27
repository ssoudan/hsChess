module Minimax where

import Board
import Move
import Data.List


data GameTree = GameTree {state::State, gameTree::[GameTree]} deriving Show

play :: GameTree->Int
play gt = play_aux True gt where
                  play_aux _ (GameTree (State cur _) []) = evalBoard cur
                  play_aux maximizingPlayer (GameTree _ xs) = case maximizingPlayer of True  -> maximum (map (play_aux False) xs)
                                                                                       False -> minimum (map (play_aux True) xs)

buildGameTree :: Int -> State -> GameTree
buildGameTree 0 s = GameTree s [] 
buildGameTree depth s = let states = nextStates s 
                             in GameTree s (map (\ls -> buildGameTree (depth - 1) ls) states)

-- buildGameTree 2 (State initialBoard White)

compareGT :: GameTree -> GameTree -> Ordering
compareGT s1 s2 = (play s1) `compare` (play s2)


doMove :: State -> State
doMove s = let gt = buildGameTree 1 s
            in state (head $ sortBy compareGT (gameTree gt))

-- doMove (State initialBoard White)