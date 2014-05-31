module MinimaxAlphaBeta where

import           Board
import           Data.List
import           Move

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

initialAlpha :: Int
initialAlpha = -100000
initialBeta :: Int
initialBeta = 100000

minimax :: State -> Int -> Int -> Int -> Bool -> Int
minimax (State cur _ _) 0 _ _ _ = evalBoard cur
minimax state' depth' alpha' beta' b' = minimaxAux depth' alpha' beta' (nextStates state') b'
  where
    minimaxAux :: Int -> Int -> Int -> [State] -> Bool -> Int
    minimaxAux _ alpha _ [] True = alpha
    minimaxAux _ _ beta [] False = beta
    minimaxAux depth alpha beta (x:xs) True = let alphaP = minimax x (depth-1) alpha beta False
                                                  alphaMax = max alphaP alpha
                                               in
                                                  if beta <= alphaMax
                                                  then alphaMax
                                                  else minimaxAux depth alphaMax beta xs True
    minimaxAux depth alpha beta (x:xs) False = let betaP = minimax x (depth-1) alpha beta True
                                                   betaMin = min betaP beta
                                                in
                                                   if betaMin <= alpha
                                                   then betaMin
                                                   else minimaxAux depth alpha betaMin xs False


evalOption :: State -> Bool -> (Int, State)
evalOption state maximizingPlayer = (minimax state 4 initialAlpha initialBeta maximizingPlayer, state)

compareOption :: (Int, State) -> (Int, State) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

doMove :: State -> State
doMove s = case s of
                (State _ _ White) -> snd $ minimumBy compareOption ( map (`evalOption` True) (nextStates s))
                (State _ _ Black) -> snd $ maximumBy compareOption ( map (`evalOption` False) (nextStates s))
