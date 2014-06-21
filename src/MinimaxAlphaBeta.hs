{-
 MinimaxAlphaBeta.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}

-- | Strategy based on a lazy implementation of the Minimax algorithm with alpha-beta pruning.
--
-- The game tree is NOT computed beforehand but generated up to a certain depth while it is evaluated.
-- The size of the heap is quite limited - it can fit in 3MB for a depth of 4.
-- The total amount of allocation is still important.
-- Subtree might not get explored because of the pruning.
--
-- This implementation is slower than the one of "Minimax" but faster than "MinimaxLazy"
--
-- See also "Minimax", "MinimaxLazy"
module MinimaxAlphaBeta (doMove) where

import           Board     (PieceColor (..))
import           Data.List
import           ParUtils
import           State


-- | Minimax with alphabeta pruning
--
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
minimax :: SuperState -> Int -> Int -> Int -> Bool -> Int
minimax state' 0 _ _ _ = evalState state'
minimax state' depth' alpha' beta' b' = if null children
                                         then evalState state'
                                         else minimaxAux depth' alpha' beta' children b'

  where
    children :: [SuperState]
    children = nextStates state'
    minimaxAux :: Int -> Int -> Int -> [SuperState] -> Bool -> Int
    minimaxAux _ alpha beta [] maximizingPlayer = if maximizingPlayer then alpha else beta
    minimaxAux depth alpha beta (x:xs) True = let alphaP = minimax x (depth - 1) alpha beta False
                                                  alphaMax = max alphaP alpha
                                               in
                                                  if beta <= alphaMax
                                                  then alphaMax
                                                  else minimaxAux depth alphaMax beta xs True
    minimaxAux depth alpha beta (x:xs) False = let betaP = minimax x (depth - 1) alpha beta True
                                                   betaMin = min betaP beta
                                                in
                                                   if betaMin <= alpha
                                                   then betaMin
                                                   else minimaxAux depth alpha betaMin xs False

-- | Initial value of Alpha (Must be lower than any result of 'State.evalState')
initialAlpha :: Int
initialAlpha = -100000

-- | Initial value of Beta (Must be greater than any result of 'State.evalState')
initialBeta :: Int
initialBeta = 100000

-- | Depth of the game tree to explore
defaultDepth :: Int
defaultDepth = 4

-- | Evaluate an option as defined by the 'SuperState' is would lead a player to.
--
-- Limit the exploration to a depth of 'defaultDepth'
--
evalOption :: SuperState -> Bool -> (Int, SuperState)
evalOption state maximizingPlayer = (minimax state defaultDepth initialAlpha initialBeta maximizingPlayer, state)

-- | Compare two pair of ('Int', 'SuperState') based on the value of the first Int.
compareOption :: (Int, SuperState) -> (Int, SuperState) -> Ordering
compareOption (s1,_) (s2,_) = s1 `compare` s2

-- | Select the next move based on the minimax algorithm and the 'evalOption'/'compareOption' 'SuperState' comparison functions.
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
doMove s = snd $ optimizeBy (nextStates s)
     where playerColor = (getPlayer . fst) s
           optimizeBy state = case playerColor of White -> minimumBy compareOption (myRunPar $ myParMap (`evalOption` True) state)
                                                  Black -> maximumBy compareOption (myRunPar $ myParMap (`evalOption` False) state)

