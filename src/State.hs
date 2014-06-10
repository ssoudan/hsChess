{-
 State.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module State where

import           Board     (Board, PieceColor(..), evalBoard, otherPlayer,
                            prettyBoard, initialBoard)
import           Data.List (intersperse)
import           History
import           Move

-- [http://en.wikipedia.org/wiki/Castling]
--
-- Castling consists of moving the king two squares towards a rook on the player's first rank,
-- then moving the rook to the square over which the king crossed.
-- Castling can only be done if:
-- • the king has never moved,
-- • the rook involved has never moved,
-- • the squares between the king and the rook involved are unoccupied,
-- • the king is not in check,
-- • and the king does not cross over or end on a square in which it would be in check.

-- | State for one player
data PlayerState = PlayerState {
      canCastleLeft  :: Bool,         -- ^ True if left rook and the king have never moved
      canCastleRight :: Bool,         -- ^ True if right rook and the king have never moved
      isCheck        :: Bool,         -- ^ True if the king is check - need to move the king out of check state
      isCheckMate    :: Bool          -- ^ True if the king is checkmate - game is over
    }

data State = State {
      getBoard       :: Board,        -- ^ the current board
      getHistory     :: History,      -- ^ the move history
      getPlayer      :: PieceColor,   -- ^ the color of the player to play
      getWhiteState  :: PlayerState,  -- ^ the state of white player
      getBlackState  :: PlayerState   -- ^ the state of black player
    }

-- | Create a new 'PlayerState' as it is supposed to be at the beginning of the game
newPlayerState :: PlayerState
newPlayerState = PlayerState True True False False

-- | Create a new 'State' as it is supposed to be at the beginning of the game
newState :: State
newState = State Board.initialBoard newHistory White newPlayerState newPlayerState

instance Show State where
    show state = "-> moves: \n\t" ++ concat (intersperse ", \n\t" (historyToList (getHistory state)))
                      ++ "\n" ++ prettyBoard currentBoard
                      ++ "\n-> player: " ++ show (getPlayer state)
                      ++ "\n-> score: " ++ show (evalBoard currentBoard)
                      ++ "\n"
        where currentBoard = getBoard state

-- | Generate all the possible next states for a turn.
--
-- TODO generate game state as well - stalemate, mate, checkmate
-- TODO include castle
-- TODO include 'prises en passant'
--
nextStates :: State -> [State]
nextStates state = [ applyMove move state | move <- genAllMoves state]

-- | Apply a 'Move' to a 'State' to generate another 'State'
--
-- The new 'State' contains the representation of all the moves that lead to this state.
--
applyMove :: Move -> State -> State
applyMove move state = State (applyMoveOnBoard (getBoard state) move)
                             (appendHistory (getHistory state) move)
                             (otherPlayer (getPlayer state))
                             (getWhiteState state) -- TODO update states if needed
                             (getBlackState state) -- TODO update states if needed

-- | Generate all the possible moves
--
genAllMoves :: State -> [Move]
genAllMoves state = let board = getBoard state
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
evalState = evalBoard . getBoard
