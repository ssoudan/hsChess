{-
 State.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module State where

import           Board     (Board, PieceColor (..), Pos (..), evalBoard,
                            initialBlackKingPosition, initialBoard,
                            initialWhiteKingPosition, isEmpty, otherPlayer,
                            prettyBoard, findFirstPiece, king)
import           Data.List (intercalate)
import           History
import           Move
import           Data.Maybe


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
    } deriving Show

data State = State {
      getBoard      :: Board,        -- ^ the current board
      getHistory    :: History,      -- ^ the move history
      getPlayer     :: PieceColor,   -- ^ the color of the player to play
      getWhiteState :: PlayerState,  -- ^ the state of white player
      getBlackState :: PlayerState   -- ^ the state of black player
    }

-- | Create a new 'PlayerState' as it is supposed to be at the beginning of the game
newPlayerState :: PlayerState
newPlayerState = PlayerState True True False False

-- | Create a new 'State' as it is supposed to be at the beginning of the game
newState :: State
newState = State Board.initialBoard newHistory White newPlayerState newPlayerState

instance Show State where
    show state = "-> moves: \n\t" ++ intercalate ", \n\t" (historyToList (getHistory state))
                      ++ "\n" ++ prettyBoard currentBoard
                      ++ "\n-> player: " ++ show (getPlayer state)
                      ++ "\n-> score: " ++ show (evalBoard currentBoard)
                      ++ "\n-> white state: " ++ show (getWhiteState state)
                      ++ "\n-> Black state: " ++ show (getBlackState state)
                      ++ "\n"
        where currentBoard = getBoard state

type SuperState = (State, [Move])

-- | Generate all the possible next states for a turn.
--
-- TODO generate game state as well - stalemate, mate, checkmate
-- TODO include 'prises en passant'
--
nextStates :: SuperState -> [SuperState]
nextStates (state, allMoves) = map (`applyMove` state) allMoves

newSuperState :: SuperState
newSuperState = let state = newState
                 in (state, genAllMoves state)

-- | Check if the player identifed by 'PieceColor' is check with one of the '[Move]' the other player can do on 'Board'.
isPlayerCheck :: PieceColor -> Board -> [Move] -> Bool
isPlayerCheck playerColor board = any $ (== theKing) . getDestination 
                  where theKing = fromMaybe (Pos (-1,-1)) $ findFirstPiece (king playerColor) board

mkPlayerState :: Bool -> Bool -> Bool -> Bool -> PlayerState
mkPlayerState = PlayerState 

getCurrentPlayerState :: SuperState -> PlayerState
getCurrentPlayerState state = let state' = fst state
                               in case getPlayer state' of White -> getWhiteState state'
                                                           Black -> getBlackState state'

isCurrentPlayerMate :: SuperState -> Bool
isCurrentPlayerMate = isCheckMate . getCurrentPlayerState

-- | Apply a 'Move' to a 'State' to generate a new 'SuperState'
--
-- The new 'SuperState' contains the representation of all the moves that lead to this state plus the moves
-- that can be done from here.
--
-- >>> getWhiteState $ fst $ applyMove (makeMove (Pos (7,3)) (Pos (5,3))) newState
-- PlayerState {canCastleLeft = False, canCastleRight = False, isCheck = False, isCheckMate = False}
--
applyMove :: Move -> State -> SuperState
applyMove move state = (state', nextPlayerMoves)
                  where
                       nextPlayerMoves = genAllMoves state'                         -- TODO if nextMoves contains the king of the currentPlayer, current player's state must be updated to check
                       state' = State nextBoard                                     -- TODO if nextMoves is empty, next player is checkmate
                                      (appendHistory (getHistory state) move)
                                      nextPlayer
                                      nextWhitePlayerState
                                      nextBlackPlayerState
                       currentPlayerMoves = genBasicMoves nextBoard currentPlayer
                       otherPlayerMoves = genBasicMoves nextBoard nextPlayer
                       currentPlayer = getPlayer state
                       nextPlayer = otherPlayer currentPlayer
                       nextBoard = applyMoveOnBoard previousBoard move
                       isWhiteCheck = ((currentPlayer == Black) && isNextPlayerCheck) || (currentPlayer == White && isCurrentPlayerCheck)
                       isBlackCheck = ((currentPlayer == White) && isNextPlayerCheck) || (currentPlayer == Black && isCurrentPlayerCheck)
                       nextWhitePlayerState = updatePlayerState source White (getWhiteState state) isWhiteCheck ((currentPlayer == Black) && null nextPlayerMoves)
                       nextBlackPlayerState = updatePlayerState source Black (getBlackState state) isBlackCheck ((currentPlayer == White) && null nextPlayerMoves)
                       isNextPlayerCheck = isPlayerCheck nextPlayer nextBoard currentPlayerMoves
                       isCurrentPlayerCheck = isPlayerCheck currentPlayer nextBoard otherPlayerMoves
                       source :: Pos
                       source = getSource move
                       previousBoard :: Board
                       previousBoard = getBoard state

-- | Update the 'PlayerState' to reflect if the player can still castle or not
--
-- TODO does not work in case the rook has been captured before moving
--
updatePlayerState :: Pos -> PieceColor -> PlayerState -> Bool -> Bool -> PlayerState        
updatePlayerState source playerColor playerState | playerColor == White && source == initialBlackKingPosition = mkPlayerState False False
                                                 | playerColor == White && source == Pos (7,0)                = mkPlayerState False (canCastleRight playerState)
                                                 | playerColor == White && source == Pos (7,7)                = mkPlayerState (canCastleLeft playerState) False
                                                 | playerColor == Black && source == initialWhiteKingPosition = mkPlayerState False False
                                                 | playerColor == Black && source == Pos (0,0)                = mkPlayerState (canCastleLeft playerState) False
                                                 | playerColor == Black && source == Pos (0,7)                = mkPlayerState False (canCastleRight playerState)
                                                 | otherwise                                                  = mkPlayerState (canCastleLeft playerState) (canCastleRight playerState)

-- | Check whether the player can castle
canCastle :: PieceColor -> PlayerState -> Board -> (Bool, Bool)
canCastle playerColor playerState board = (left, right)
            where isBoardEmpty pos = isEmpty pos board
                  right = canCastleRight playerState && ((playerColor == White && isBoardEmpty (7,5) && isBoardEmpty (7,6))
                                                     ||  (playerColor == Black && isBoardEmpty (0,1) && isBoardEmpty (0,2) && isBoardEmpty (0,3)))
                  left  = canCastleLeft playerState  && ((playerColor == White && isBoardEmpty (7,1) && isBoardEmpty (7,2) && isBoardEmpty (7,3))
                                                     ||  (playerColor == Black && isBoardEmpty (0,5) && isBoardEmpty (0,6)))

-- | Build needed castle moves
castleMoves :: PieceColor -> PlayerState -> Board -> [Move]
castleMoves playerColor playerState board = case canCastle playerColor playerState board of (True, True)    -> [leftCastleMove, rightCastleMove]
                                                                                            (True, False)   -> [leftCastleMove]
                                                                                            (False, True)   -> [rightCastleMove]
                                                                                            (False, False)  -> []
                                                              where leftCastleMove = case playerColor of White  -> CastleWhiteLeft
                                                                                                         Black  -> CastleBlackLeft
                                                                    rightCastleMove = case playerColor of White -> CastleWhiteRight
                                                                                                          Black -> CastleBlackRight

genBasicMoves :: Board -> PieceColor -> [Move]
genBasicMoves board playerColor = let pieces = colorPos playerColor board
                                   in concatMap (`genValidMoves` board) pieces

-- | Generate all the possible moves
--
genAllMoves :: State -> [Move]
genAllMoves state                         = let playerColor = getPlayer state
                                                playerState = currentPlayerState state
                                                board = getBoard state
                                                allMoves = genBasicMoves board playerColor ++ castleMoves playerColor playerState board
                                                currentPlayerState = case playerColor of White -> getWhiteState
                                                                                         Black -> getBlackState
                                                stillCheck = not . isCheck . currentPlayerState . fst . (`applyMove` state)
                                             in 
                                               if isCheck playerState
                                                -- in case of check, filter out the move that keep the player check
                                                then filter stillCheck allMoves 
                                                -- else, return all the moves
                                                else allMoves

-- | True if the 'Move' is valid for the 'SuperState' 
validMove :: Move -> SuperState -> Bool
validMove m s = elem m $ snd s

-- | 'State' evaluation function.
--
-- Currently it is basing the evaluation only on the 'Board' it contains.
-- This is the method that should be used to evaluate a State in the strategies.
--
-- >>> evalState (newState, [])
-- 0
evalState :: SuperState -> Int
evalState state = let s = fst state
                      isWhiteMate = isCheckMate . getWhiteState $ s
                      isBlackMate = isCheckMate . getBlackState $ s
                   in if isWhiteMate
                      then -1000000
                      else if isBlackMate
                        then 1000000
                        else evalBoard . getBoard $ s


-- | Return the move history as a [String]
getMoveHistoryFromState :: State -> [String]
getMoveHistoryFromState = historyToList . getHistory
