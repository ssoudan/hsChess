module MinimaxLazy where

import           Board
import           Data.List
import           Move

-- from wikipedia [http://en.wikipedia.org/wiki/Minimax]
--
--function minimax(node, depth, maximizingPlayer)
--    if depth = 0 or node is a terminal node
--        return the heuristic value of node
--    if maximizingPlayer
--        bestValue := -∞
--        for each child of node
--            val := minimax(child, depth - 1, FALSE)
--            bestValue := max(bestValue, val);
--        return bestValue
--    else
--        bestValue := +∞
--        for each child of node
--            val := minimax(child, depth - 1, TRUE)
--            bestValue := min(bestValue, val);
--        return bestValue

--(* Initial call for maximizing player *)
--minimax(origin, depth, TRUE)

minimax :: State -> Int -> Bool -> Int
minimax (State cur _ _) 0 _ = evalBoard cur
minimax state depth True = maximum $ map (\m -> minimax m (depth-1) False) (nextStates state)
minimax state depth False = minimum $ map (\m -> minimax m (depth-1) True) (nextStates state)

evalOption :: State -> PieceColor -> (Int, State)
evalOption state color = (minimax state 4 (color == Black), state)

compareOption :: (Int, State) -> (Int, State) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

doMove :: State -> State
doMove s@(State _ _ White) = case playerColor of 
                                                White -> snd $ minimumBy compareOption options
                                                Black -> snd $ maximumBy compareOption options
                             where options = map (`evalOption` playerColor) (nextStates s)
                                   playerColor = player s



-- doMove (State initialBoard White)
