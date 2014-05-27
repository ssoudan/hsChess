module MinimaxAlphaBeta where

import Board
import Move
import Data.List
import Debug.Trace

-- from wikipedia [http://en.wikipedia.org/wiki/Alpha-beta_pruning]

--  function alphabeta(node, depth, α, β, maximizingPlayer)
--    if depth = 0 or node is a terminal node
--        return the heuristic value of node
--    if maximizingPlayer
--        for each child of node
--            α := max(α, alphabeta(child, depth - 1, α, β, FALSE))
--            if β ≤ α
--                break (* β cut-off *)
--        return α
--    else
--        for each child of node
--            β := min(β, alphabeta(child, depth - 1, α, β, TRUE))
--            if β ≤ α
--                break (* α cut-off *)
--        return β
--  (* Initial call *)
--  alphabeta(origin, depth, -∞, +∞, TRUE)

initial_alpha :: Int
initial_alpha = -100000
initial_beta :: Int
initial_beta = 100000

mmax :: Int -> Int -> Int
mmax a b 
      | a > b = a 
      | otherwise = b

mmin :: Int -> Int -> Int
mmin a b 
      | a < b = a 
      | otherwise = b


minimax_aux :: Int -> Int -> Int -> [State] -> Bool -> Int
minimax_aux _ alpha _ [] True = alpha
minimax_aux _ _ beta [] False = beta
minimax_aux depth alpha beta (x:xs) True = let alphaP = minimax x (depth-1) alpha beta False
                                               alphaMax = mmax alphaP alpha
                                            in 
                                               if beta <= alphaMax
                                                   then alphaMax
                                                   else minimax_aux depth alphaMax beta xs True
minimax_aux depth alpha beta (x:xs) False = let betaP = minimax x (depth-1) alpha beta True 
                                                betaMin = mmin betaP beta
                                               in 
                                                  if betaMin <= alpha
                                                      then betaMin
                                                      else minimax_aux depth alpha betaMin xs False

minimax :: State -> Int -> Int -> Int -> Bool -> Int
minimax (State cur _ _) 0 _ _ _ = evalBoard cur
minimax state depth alpha beta b = minimax_aux depth alpha beta (nextStates state) b


evalOption :: State -> Bool -> (Int, State)
evalOption state maximizingPlayer = (minimax state 4 initial_alpha initial_beta maximizingPlayer, state)

compareOption :: (Int, State) -> (Int, State) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

second :: (a, State) -> State
second (_, x) = x

doMove :: State -> State
doMove s@(State _ _ White) = let sortedStates = ( sortBy compareOption ( map (\ss -> evalOption ss True) (nextStates s) ))
                              in trace ("Sorted state for white move: " ++ show sortedStates) $
                                 second $ head sortedStates
doMove s@(State _ _ Black) = let sortedStates = ( sortBy compareOption ( map (\ss -> evalOption ss False) (nextStates s) ))
                              in trace ("Sorted state for black move: " ++ show sortedStates) $ 
                              second $ last sortedStates
