{-
 State.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module State where

import           Board (Board, PieceColor, evalBoard, prettyBoard, otherPlayer)
import           Move

data State = State { getCurrent :: Board, getMove :: String, getPlayer :: PieceColor }

instance Show State where
    show (State cur m p) = "-> move: " ++ m
                      ++ "\n" ++ prettyBoard cur
                      ++ "\n-> player: " ++ show p
                      ++ "\n-> score: " ++ show (evalBoard cur)
                      ++ "\n"

--showState :: State -> String
--showState (State cur m p) = "move: " ++ m
--                       ++ "\n" ++ prettyBoard cur
--                       ++ "\n-> player: " ++ show p
--                       ++ "\n-> score: " ++ show (evalBoard cur)
--                       ++ "\n"

-- | Generate all the possible next states for a turn.
--
-- TODO generate game state as well - stalemate, mate, checkmate
-- TODO include castle
-- TODO include 'prises en passant'
--
nextStates :: State -> [State]
nextStates (State cur _ p) = let pieces = colorPos p cur
                            in concatMap (\m -> [ State newboard (show move) (otherPlayer p) | (newboard, move) <- genMoves m cur]) pieces

applyMove :: Move -> State -> State
applyMove m s = State (applyMoveOnBoard (getCurrent s) m) (show m) (otherPlayer (getPlayer s))

-- | Generate all the possible moves
--
-- TODO use this methode to implement nextStates
--
genAllMoves :: State -> [Move]
genAllMoves state = let board = getCurrent state
                        pieces = colorPos (getPlayer state) board
                     in concatMap (\pos -> genValidMoves pos board) pieces


evalState :: State -> Int
evalState = evalBoard . getCurrent
