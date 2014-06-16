{-
 State.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module State where

import           Board     (Board, PieceColor (..), Pos (..), evalBoard,
                            initialBlackKingPosition, initialBoard,
                            initialWhiteKingPosition, isEmpty, otherPlayer,
                            prettyBoard)
import           Data.List (intercalate)
import qualified Data.Set  as Set
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
                      ++ "\n"
        where currentBoard = getBoard state

type SuperState = (State, [Move])

-- | Generate all the possible next states for a turn.
--
-- TODO generate game state as well - stalemate, mate, checkmate
-- TODO include 'prises en passant'
--
nextStates :: SuperState -> [SuperState]
nextStates (state, allMoves) = [ applyMove move state | move <- allMoves ]

newSuperState :: SuperState
newSuperState = let state = newState
                    player = getPlayer state
                    playerState = case player of White -> getWhiteState state
                                                 Black -> getBlackState state
                    board = getBoard state
                 in (state, genAllMoves player playerState board)


-- | Apply a 'Move' to a 'State' to generate a new 'SuperState'
--
-- The new 'SuperState' contains the representation of all the moves that lead to this state plus the moves
-- that can be done from here.
--
-- >>> getWhiteState $ fst $ applyMove (makeMove (Pos (7,3)) (Pos (5,3))) newState
-- PlayerState {canCastleLeft = False, canCastleRight = False, isCheck = False, isCheckMate = False}
--
applyMove :: Move -> State -> SuperState
applyMove move state = (state', genAllMoves nextPlayer nextPlayerState nextBoard)
                  where
                       state' = State nextBoard
                                      (appendHistory (getHistory state) move)
                                      nextPlayer
                                      nextWhitePlayerState
                                      nextBlackPlayerState
                       nextPlayer = otherPlayer (getPlayer state)
                       nextBoard = applyMoveOnBoard previousBoard move
                       nextWhitePlayerState = updatePlayerState White (getWhiteState state) -- TODO update states if needed
                       nextBlackPlayerState = updatePlayerState Black (getBlackState state) -- TODO update states if needed
                       nextPlayerState = case nextPlayer of White -> nextWhitePlayerState
                                                            Black -> nextBlackPlayerState
                       source :: Pos
                       source = getSource move
                       --movedPiece :: Square
                       --movedPiece = elementAt source previousBoard
                       previousBoard :: Board
                       previousBoard = getBoard state
                       -- | Update the 'PlayerState' to reflect if the player can still castle or not
                       updatePlayerState :: PieceColor -> PlayerState -> PlayerState
                       updatePlayerState White playerState | source == initialBlackKingPosition = PlayerState False False (isCheck playerState) (isCheckMate playerState)
                                                           | source == Pos (7,0) = PlayerState False (canCastleRight playerState) (isCheck playerState) (isCheckMate playerState)
                                                           | source == Pos (7,7) = PlayerState (canCastleLeft playerState) False (isCheck playerState) (isCheckMate playerState)
                                                           | otherwise = playerState
                       updatePlayerState Black playerState | source == initialWhiteKingPosition = PlayerState False False (isCheck playerState) (isCheckMate playerState)
                                                           | source == Pos (0,0) = PlayerState (canCastleLeft playerState) False (isCheck playerState) (isCheckMate playerState)
                                                           | source == Pos (0,7) = PlayerState False (canCastleRight playerState) (isCheck playerState) (isCheckMate playerState)
                                                           | otherwise = playerState

-- | Check whether the player can castle
canCastle :: PieceColor -> PlayerState -> Board -> (Bool, Bool)
canCastle playerColor playerState board = (left, right)
            where isBoardEmpty pos = isEmpty pos board
                  right  = canCastleRight playerState && ((playerColor == White && isBoardEmpty (7,5) && isBoardEmpty (7,6))
                                                      ||  (playerColor == Black && isBoardEmpty (0,1) && isBoardEmpty (0,2) && isBoardEmpty (0,3)))
                  left   = canCastleLeft playerState  && ((playerColor == White && isBoardEmpty (7,1) && isBoardEmpty (7,2) && isBoardEmpty (7,3))
                                                      ||  (playerColor == Black && isBoardEmpty (0,5) && isBoardEmpty (0,6)))

-- | Build needed castle moves
castleMoves :: PieceColor -> PlayerState -> Board -> [Move]
castleMoves playerColor playerState board = case canCastle playerColor playerState board of (True, True) -> [leftCastleMove, rightCastleMove]
                                                                                            (True, False) -> [leftCastleMove]
                                                                                            (False, True) -> [rightCastleMove]
                                                                                            (False, False) -> []
                                                              where leftCastleMove = case playerColor of White -> CastleWhiteLeft
                                                                                                         Black -> CastleBlackLeft
                                                                    rightCastleMove = case playerColor of White -> CastleWhiteRight
                                                                                                          Black -> CastleBlackRight

-- | Generate all the possible moves
--
-- TODO keep only legal moves in case of check
genAllMoves :: PieceColor -> PlayerState -> Board -> [Move]
genAllMoves playerColor playerState board = let pieces = colorPos playerColor board
                                             in concatMap (`genValidMoves` board) pieces ++ castleMoves playerColor playerState board

validMove :: Move -> SuperState -> Bool
validMove m s = Set.member m (Set.fromList $ snd s)

-- | 'State' evaluation function.
--
-- Currently it is basing the evaluation only on the 'Board' it contains.
-- This is the method that should be used to evaluate a State in the strategies.
--
-- >>> evalState newState
-- 0
evalState :: SuperState -> Int
evalState = evalBoard . getBoard . fst


-- | Return the move history as a [String]
getMoveHistoryFromState :: State -> [String]
getMoveHistoryFromState = historyToList . getHistory
