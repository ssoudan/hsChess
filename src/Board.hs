{-
 Board.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module Board where

import           Data.List
import           Data.Maybe (isJust)
import           Utils (applyAt)

data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving Eq
data PieceColor = Black | White deriving Eq
data Piece = Piece {pieceType::PieceType, pieceColor::PieceColor} deriving Eq

type Square = Maybe Piece

type Board = [[Square]]

newtype Pos = Pos (Int, Int) deriving (Show, Ord)

instance Eq Pos where 
    (Pos (a,b)) == (Pos (c,d)) = a==c && b==d

instance Show PieceColor where
    show Black  = "Black"
    show White  = "White"

otherPlayer :: PieceColor -> PieceColor
otherPlayer Black = White
otherPlayer White = Black

instance Show PieceType where
    show King   = "K"
    show Rook   = "R"
    show Queen  = "Q"
    show Knight = "N"
    show Pawn   = "P"
    show Bishop = "B"

prettyPrintPiece :: Piece -> String
prettyPrintPiece (Piece p White) = case p of King -> "\x2654 "
                                             Queen -> "\x2655 "
                                             Rook -> "\x2656 "
                                             Bishop -> "\x2657 "
                                             Knight -> "\x2658 "
                                             Pawn -> "\x2659 "
prettyPrintPiece (Piece p Black) = case p of King -> "\x265A "
                                             Queen -> "\x265B "
                                             Rook -> "\x265C "
                                             Bishop -> "\x265D "
                                             Knight -> "\x265E "
                                             Pawn -> "\x265F "

instance Show Piece where
    show = prettyPrintPiece

-- | Create an 8x8 emptyBoard
emptyBoard :: Board
emptyBoard = [[Nothing | _ <- [(1::Integer)..8]] | _ <- [(1::Integer)..8]]

-- | Create a 8x8 chess board ready to start
initialBoard :: Board
initialBoard = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
        [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
        [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece King White), Just (Piece Queen White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

-- | Create a 8x8 chess board with a 'jeu ouvert' setup
jeuOuvert :: Board
jeuOuvert = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
        [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Nothing, Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Just (Piece Pawn Black), Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Just (Piece Pawn White), Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Nothing, Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
        [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece King White), Just (Piece Queen White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

-- | Pretty print a square
--
-- Use ". " for empty squares and the unicode character followed by a space for pieces.
-- 
-- >>> prettySquare (Just (Piece Rook Black))
-- "\9820 "
-- 
-- >>> prettySquare Nothing
-- ". "
--
prettySquare :: Square -> String
prettySquare = maybe ". " show

-- | Pretty print a row of squares
prettyPrintLine :: [Square] -> String
prettyPrintLine = foldr ((++) . prettySquare) []

-- | Pretty print a board
-- 
-- >>> prettyBoard initialBoard
-- "  ---- B ----  \n\9820 \9822 \9821 \9819 \9818 \9821 \9822 \9820 \n\9823 \9823 ..."
--
prettyBoard :: Board -> String
prettyBoard board = "     ---- B ----  \n" 
                 ++ "   a b c d e f g h\n" 
                 ++ "  \x250c\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2510\n"
                 ++ unlines (map (\(row, line) -> show row ++ " " ++ line) (zip [0..7::Int] ( map (wrap . prettyPrintLine) board )))
                 ++ "  \x2514\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2518\n"
                 ++ "   a b c d e f g h\n" 
                 ++ "     ---- W ----  \n"
        where wrap s = "\x2502" ++ s ++ "\x2502"

-- | Returns the value of a PieceType.
--
-- This is the center of our evaluation strategy.
--
-- TODO: definitively need to be improved
valuePieceMap :: PieceType -> Int
valuePieceMap King = 1000
valuePieceMap Rook = 5
valuePieceMap Queen = 9
valuePieceMap Knight = 3
valuePieceMap Pawn = 1
valuePieceMap Bishop = 3

-- | Returns the value of a Piece
valuePiece :: Piece -> Int
valuePiece = valuePieceMap . pieceType

instance Ord Piece where
  p1 `compare` p2 = valuePiece p1 `compare` valuePiece p2

squareScore :: Square -> Int
squareScore = maybe 0 valuePiece

evalBoardFor :: [Square] -> Int
evalBoardFor = sum . map squareScore

isPiece :: Square -> Bool
isPiece = isJust

isBlack :: Square -> Bool
isBlack (Just (Piece _ Black)) = True
isBlack (Just (Piece _ White)) = False
isBlack Nothing = error "Not a piece"

evalBoard :: Board -> Int
evalBoard board = blackScore - whiteScore
                where blackScore = evalBoardFor blacks
                      whiteScore = evalBoardFor whites
                      (blacks, whites) = partition isBlack $ filter isPiece $ concat board

-- | Apply function f on square at position 'Pos (x,y)' of 'board'.
-- Leave the rest unmodified.
applyOnBoard :: (Square -> Square) -> Pos -> Board -> Board
applyOnBoard f (Pos (x,y)) = applyAt (applyAt f y) x

-- | Replace the piece at position 'pos' with the Piece 'piece' in 'board'.
updateBoard :: Pos -> Square -> Board -> Board
updateBoard pos piece = applyOnBoard (const piece) pos

-- | Delete the piece at position 'pos' in a board and replace it with a Nothing.
deleteSquare :: Pos -> Board -> Board
deleteSquare pos = updateBoard pos Nothing

-- | Returns the 'square' of the 'board' that is a position the provided position 'Pos (x,y)'
elementAt :: Pos -> Board -> Square
elementAt (Pos (x,y)) board = (board!!x)!!y

showPos :: Pos -> String
showPos (Pos (x,y)) = (['a'..]!!y):show x

nameMove :: Board -> Pos -> Square -> Pos -> String
nameMove _ _ Nothing _ = "No piece to move"
nameMove board _ (Just (Piece pt _)) destination = let destPiece = elementAt destination board
                                                    in case destPiece of Nothing -> show pt ++ showPos destination
                                                                         Just _  -> show pt ++ "x" ++ showPos destination

movePieceOnBoard :: Board -> Pos -> Pos -> Board
movePieceOnBoard board origin destination = let piece = elementAt origin board
                                                board' = deleteSquare origin board
                                             in updateBoard destination piece board'

piecePosition :: Board -> [(Int, Int, Square)]
piecePosition board = [ (x,y,c) | (x, row) <- zip [0..] board, (y,c) <- zip [0..] row, isJust c ]

