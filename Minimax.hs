module Minimax where

import Board
import Move
import Data.List
import Debug.Trace

data GameTree = GameTree {state::State, gameTree::[GameTree]} deriving Show

play :: GameTree->Int
play gt@(GameTree (State _ _ p) _) = play_aux (p == Black) gt where
                  play_aux _ (GameTree (State c _ _) []) = evalBoard c
                  play_aux True (GameTree _ xs) = maximum (map (play_aux False) xs)
                  play_aux False (GameTree _ xs) = minimum (map (play_aux True) xs)

buildGameTree :: Int -> State -> GameTree
buildGameTree 0 s = GameTree s [] 
buildGameTree depth s = let states = nextStates s 
                             in GameTree s (map (\ls -> buildGameTree (depth - 1) ls) states)

-- buildGameTree 2 (State initialBoard White)

compareGT :: GameTree -> GameTree -> Ordering
compareGT s1 s2 = (play s1) `compare` (play s2)


compareOption :: (Int, GameTree) -> (Int, GameTree) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

second :: (a, GameTree) -> GameTree
second (_, x) = x

doMove :: State -> State
doMove s@(State _ _ p) = let gt = buildGameTree 4 s
            in case p of Black -> let sortedMoves = sortBy compareOption (map (\g -> (play g, g)) (gameTree gt))
                                   in trace ("Sorted state for black move: " ++ show sortedMoves) $
                                   state (second (last sortedMoves))
                         White -> let sortedMoves = sortBy compareOption (map (\g -> (play g, g)) (gameTree gt))
                                   in trace ("Sorted state for black move: " ++ show sortedMoves) $
                                   state (second (head sortedMoves))

-- doMove (State initialBoard White)
