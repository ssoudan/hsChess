module MinimaxLazy where

import Board
import Move
import Data.List

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
minimax (State cur _ _) 0 maximizingPlayer = evalBoard cur
minimax state depth True = maximum $ map (\m -> minimax m (depth-1) False) (nextStates state)
minimax state depth False = minimum $ map (\m -> minimax m (depth-1) True) (nextStates state)

evalOption :: State -> PieceColor -> (Int, State)
evalOption state color = (minimax state 4 (color == Black), state)

compareOption :: (Int, State) -> (Int, State) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

second (_, x) = x

doMove :: State -> State
doMove s@(State cur _ White) = second $ head ( sortBy compareOption ( map (\s -> evalOption s White) (nextStates s) ))
doMove s@(State cur _ Black) = second $ last ( sortBy compareOption ( map (\s -> evalOption s Black) (nextStates s) ))


-- doMove (State initialBoard White)
