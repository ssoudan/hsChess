{-
 MinimaxLazy.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}

-- | Strategy based on a lazy implementation of the Minimax algorithm.
--
-- The game tree is NOT computed beforehand but generated up to a certain depth while it is evaluated.
-- The size of the heap is quite limited - it can fit in 3MB for a depth of 4. 
-- The total amount of allocation is still important.
-- This implementation is slower than the one of "Minimax".
--
-- See also "Minimax", "MinimaxAlphaBeta"
module MinimaxLazy (doMove) where

import           Board     (PieceColor (..))
import           Data.List
import           State

-- | Lazy implementation of the minimax
-- Game tree is constructed as we go through it.
-- 
-- Relies on 'State.evalState' method to rate a given 'State'.
--
-- from wikipedia [http://en.wikipedia.org/wiki/Minimax]
--
-- function minimax(node, depth, maximizingPlayer)
--     if depth = 0 or node is a terminal node
--         return the heuristic value of node
--     if maximizingPlayer
--         bestValue := -∞
--         for each child of node
--             val := minimax(child, depth - 1, FALSE)
--             bestValue := max(bestValue, val);
--         return bestValue
--     else
--         bestValue := +∞
--         for each child of node
--             val := minimax(child, depth - 1, TRUE)
--             bestValue := min(bestValue, val);
--         return bestValue
-- (* Initial call for maximizing player *)
-- minimax(origin, depth, TRUE)
minimax :: SuperState -- ^ The current state
           -> Int     -- ^ The remaining depth to explore
           -> Bool    -- ^ Maximizing player?
           -> Int     -- ^ Returns the payoff of this branch
minimax state 0 _ = evalState state
minimax state depth maximizingPlayer = if maximizingPlayer then maximum $ map (\m -> minimax m (depth - 1) False) nextState
                                                           else minimum $ map (\m -> minimax m (depth - 1) True) nextState
                                       where nextState = nextStates state

-- | Depth of the game tree to explore
defaultDepth :: Int
defaultDepth = 4

-- | Evaluate an option as defined by the 'SuperState' is would lead a player to.
evalOption :: SuperState -> PieceColor -> (Int, SuperState)
evalOption state color = (minimax state defaultDepth (color == Black), state)

-- | Compare two pair of ('Int', 'SuperState') based on the value of the first Int.
compareOption :: (Int, SuperState) -> (Int, SuperState) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

-- | Select the next move based on the minimax algorithm and the 'evalOption'/'compareOption' 'SuperState' comparison functions.
-- 
-- >>> doMove (State Board.initialBoard History.newHistory White, [])
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
doMove s = snd $ optimizeBy compareOption options 
     where options = map (`evalOption` playerColor) (nextStates s)
           playerColor = (getPlayer . fst) s
           optimizeBy = case playerColor of White -> minimumBy
                                            Black -> maximumBy


