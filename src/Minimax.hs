{-
 Minimax.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}

-- | Strategy based on naive implementation of the Minimax algorithm.
--
-- The game tree is computed beforehand up to a predefined depth and later evaluated.
-- Expect a quite important heap size. Observed about 2G for the first moves of a game with a depth of 4.
--
-- See also "MinimaxLazy", "MinimaxAlphaBeta"
module Minimax where

import           Board     (PieceColor (..))
import           Data.List
import           State

data GameTree = GameTree { getState :: SuperState, getGameTree :: [GameTree]} deriving Show

-- | Evaluate the payoff of a 'GameTree'
play :: GameTree->Int
play gt = play_aux (Black == (getPlayer . fst . getState) gt) gt
      where
          play_aux _ (GameTree state' []) = evalState state'
          play_aux True (GameTree _ xs) = maximum (map (play_aux False) xs)
          play_aux False (GameTree _ xs) = minimum (map (play_aux True) xs)

-- | Build a 'GameTree' of a given 'depth' starting for the provided state.
--
-- >>> buildGameTree 2 (State Board.initialBoard "" White)
-- GameTree {getState = -> move: 
--      ---- B ----  
--    a b c d e f g h
--   ┌────────────────┐
-- 0 │♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ │
-- 1 │♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ │
-- 2 │. . . . . . . . │
-- 3 │. . . . . . . . │
-- 4 │. . . . . . . . │           <- initial state
-- 5 │. . . . . . . . │
-- 6 │♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ │
-- 7 │♖ ♘ ♗ ♔ ♕ ♗ ♘ ♖ │
--   └────────────────┘
--    a b c d e f g h
--      ---- W ----  
--
-- -> player: White
-- -> score: 0
-- , getGameTree = [GameTree {getState = -> move: a6->a5
--      ---- B ----  
--    a b c d e f g h
--   ┌────────────────┐
-- 0 │♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ │
-- 1 │♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ │
-- 2 │. . . . . . . . │
-- 3 │. . . . . . . . │           <- state after move a6->a5
-- 4 │. . . . . . . . │
-- 5 │♙ . . . . . . . │
-- 6 │. ♙ ♙ ♙ ♙ ♙ ♙ ♙ │
-- 7 │♖ ♘ ♗ ♔ ♕ ♗ ♘ ♖ │
--   └────────────────┘
--    a b c d e f g h
--      ---- W ----  
-- ...
--
buildGameTree :: Int -> SuperState -> GameTree
buildGameTree 0 s = GameTree s []
buildGameTree depth s = let states = nextStates s
                             in GameTree s (map (buildGameTree (depth - 1)) states)

-- | Compare two 'GameTree' based on the payoff computed with 'play'.
compareGT :: GameTree -> GameTree -> Ordering
compareGT s1 s2 = play s1 `compare` play s2

-- | Compare two pair of ('Int', 'GameTree') based on the value of the first Int.
compareOption :: (Int, GameTree) -> (Int, GameTree) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

-- | Depth of the 'GameTree's to explore.
defaultDepth :: Int
defaultDepth = 4

-- | Build the 'GameTree's for the next moves, evaluate their payoff and select the best one based on the player color.
--
-- Thie method consumes a 'State' and return a 'State' with the history and the player updated.
-- The depth of the 'GameTree's used for the evaluation is defined by 'defaultDepth'.
--
-- Because of the way 'Board.evalBoard' (and 'State.evalState') are defined, the 'Black' player is the maximizing player
-- while 'White' player is the minimizing one.
-- 
-- >>> doMove (State Board.initialBoard History.newHistory White)
-- -> move: 
--     a6->a5
--      ---- B ----  
--    a b c d e f g h
--   ┌────────────────┐
-- 0 │♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ │
-- 1 │♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ │
-- 2 │. . . . . . . . │
-- 3 │. . . . . . . . │
-- 4 │. . . . . . . . │
-- 5 │♙ . . . . . . . │
-- 6 │. ♙ ♙ ♙ ♙ ♙ ♙ ♙ │
-- 7 │♖ ♘ ♗ ♔ ♕ ♗ ♘ ♖ │
--   └────────────────┘
--    a b c d e f g h
--      ---- W ----  
-- 
-- -> player: Black
-- -> score: 0
doMove :: SuperState -> SuperState
doMove state = getState $ snd (optimizeBy compareOption options)
         where    gt = getGameTree $ buildGameTree defaultDepth state
                  doPlay g = (play g, g)
                  options = map doPlay gt
                  optimizeBy = case (getPlayer . fst) state of White -> minimumBy
                                                               Black -> maximumBy
