{-
 State.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module State where

import           Board (Board, PieceColor, evalBoard, otherPlayer, prettyBoard)
import           Move
import           History
import           Data.List(intersperse)

data State = State { getCurrent :: Board, getHistory :: History, getPlayer :: PieceColor }

instance Show State where
    show state = "-> moves: \n\t" ++ concat (intersperse ", \n\t" (historyToList (getHistory state)))
                      ++ "\n" ++ prettyBoard currentBoard
                      ++ "\n-> player: " ++ show (getPlayer state)
                      ++ "\n-> score: " ++ show (evalBoard currentBoard)
                      ++ "\n"
        where currentBoard = getCurrent state

-- | Generate all the possible next states for a turn.
--
-- TODO generate game state as well - stalemate, mate, checkmate
-- TODO include castle
-- TODO include 'prises en passant'
--
nextStates :: State -> [State]
nextStates (State cur history p) = let pieces = colorPos p cur
                            in concatMap (\m -> [ State newboard (appendHistory history move) (otherPlayer p) | (newboard, move) <- genMoves m cur]) pieces

-- | Apply a 'Move' to a 'State' to generate another 'State'
--
-- The new 'State' contains the representation of all the moves that lead to this state.
-- 
applyMove :: Move -> State -> State
applyMove m s = State (applyMoveOnBoard (getCurrent s) m) 
                      (appendHistory (getHistory s) m) 
                      (otherPlayer (getPlayer s))

-- | Generate all the possible moves
--
-- TODO use this method to implement nextStates
--
genAllMoves :: State -> [Move]
genAllMoves state = let board = getCurrent state
                        pieces = colorPos (getPlayer state) board
                     in concatMap (`genValidMoves` board) pieces

-- | 'State' evaluation function.
--
-- Currently it is basing the evaluation only on the 'Board' it contains.
-- This is the method that should be used to evaluate a State in the strategies.
--
-- >>> evalState (State Board.initialBoard "" White) 
-- 0
evalState :: State -> Int
evalState = evalBoard . getCurrent
