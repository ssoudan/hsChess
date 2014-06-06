{-
 MinimaxLazy.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module MinimaxLazy where

import           Board     (PieceColor(..))
import           Data.List
import           State 

-- | Lazy implementation of the minimax
-- Game tree is constructed as we go through it.
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
minimax :: State     -- ^ The current state
           -> Int    -- ^ The remaining depth to explore
           -> Bool   -- ^ Maximizing player?
           -> Int    -- ^ Returns the payoff of this branch
minimax state 0 _ = evalState state
minimax state depth maximizingPlayer = case maximizingPlayer of True -> maximum $ map (\m -> minimax m (depth - 1) False) nextState
                                                                False -> minimum $ map (\m -> minimax m (depth - 1) True) nextState
                                       where nextState = nextStates state

evalOption :: State -> PieceColor -> (Int, State)
evalOption state color = (minimax state 4 (color == Black), state)

compareOption :: (Int, State) -> (Int, State) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

doMove :: State -> State
doMove s = case playerColor of
                              White -> snd $ minimumBy compareOption options
                              Black -> snd $ maximumBy compareOption options
     where options = map (`evalOption` playerColor) (nextStates s)
           playerColor = getPlayer s

