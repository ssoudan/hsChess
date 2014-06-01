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

newtype Pos = Pos (Int, Int) deriving (Eq, Show)

instance Show PieceColor where
    show Black  = "B"
    show White  = "W"

instance Show PieceType where
    show King   = "K"
    show Rook   = "R"
    show Queen  = "Q"
    show Knight = "N"
    show Pawn   = "P"
    show Bishop = "B"

prettyPrintPiece :: Piece -> String
prettyPrintPiece (Piece p White) = case p of King -> "\x2654"
                                             Queen -> "\x2655"
                                             Rook -> "\x2656"
                                             Bishop -> "\x2657"
                                             Knight -> "\x2658"
                                             Pawn -> "\x2659"
prettyPrintPiece (Piece p Black) = case p of King -> "\x265A"
                                             Queen -> "\x265B"
                                             Rook -> "\x265C"
                                             Bishop -> "\x265D"
                                             Knight -> "\x265E"
                                             Pawn -> "\x265F"

instance Show Piece where
    show = prettyPrintPiece

-- Just (Piece Rook Black)
emptyBoard :: Board
emptyBoard = [[Nothing | _ <- [(1::Integer)..8]] | _ <- [(1::Integer)..8]]

initialBoard :: Board
initialBoard = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
        [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
        [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece King White), Just (Piece Queen White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

jeuOuvert :: Board
jeuOuvert = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
        [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Nothing, Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Just (Piece Pawn Black), Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Just (Piece Pawn White), Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Nothing, Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
        [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece King White), Just (Piece Queen White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]


prettySquare :: Square -> String
prettySquare Nothing = ". "
prettySquare (Just p) = show p ++ " "

prettyPrintLine :: [Square] -> String
prettyPrintLine = foldr ((++) . prettySquare) []

prettyBoard :: Board -> String
prettyBoard board = "  ---- B ----  \n" ++ unlines (map prettyPrintLine board) ++ "  ---- W ----  \n"

-- putStr $ prettyBoard emptyBoard
valuePieceMap :: PieceType -> Int
valuePieceMap King = 1000
valuePieceMap Rook = 5
valuePieceMap Queen = 9
valuePieceMap Knight = 3
valuePieceMap Pawn = 1
valuePieceMap Bishop = 3

valuePiece :: Piece -> Int
valuePiece = valuePieceMap . pieceType

instance Ord Piece where
  p1 `compare` p2 = valuePiece p1 `compare` valuePiece p2

squareScore :: Square -> Int
squareScore (Just piece) = valuePiece piece
squareScore Nothing = 0


-- (Piece King Black) > (Piece Queen Black)
-- :t (fmap squareScore)

-- sum (map (sum . fmap squareScore) [[Nothing, Nothing], [Nothing, Just (Piece Knight Black)]])

-- pieceColor (Piece King Black)


evalBoardFor :: [Square] -> Int
evalBoardFor x = sum $ map squareScore x

isPiece :: Square -> Bool
isPiece = isJust

isBlack :: Square -> Bool
isBlack (Just (Piece _ Black)) = True
isBlack (Just (Piece _ White)) = False
isBlack Nothing = error "Not a piece"

evalBoard :: Board -> Int
evalBoard board = let blackScore = evalBoardFor blacks
                      whiteScore = evalBoardFor whites
                   in (blackScore - whiteScore)
                where (blacks, whites) =  partition isBlack $ filter isPiece $ concat board

-- | Apply function f on sqaure at position 'Pos (x,y)' of 'board'.
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

type BoardWithMove = (Board, String)

nameMove :: Board -> Pos -> Square -> Pos -> String
nameMove board _ (Just (Piece pt _)) destination = let destPiece = elementAt destination board
                                     in case destPiece of Nothing -> show pt ++ showPos destination
                                                          Just _ ->  show pt ++ "x" ++ showPos destination
nameMove _ _ Nothing _ = "No piece to move"

movePos :: Pos -> Pos -> Board -> BoardWithMove
movePos origin destination board = let piece = elementAt origin board
                                       board' = deleteSquare origin board
                                    in (updateBoard destination piece board', nameMove board origin piece destination)
-- :t movePos
-- prettyBoard $ movePos (0,0) (2,0) initialBoard
-- distributeLabel :: (Int, [(a,b)]) -> [(Int, a, b)]
-- distributeLabel (l, xs) = map (\(y,e) -> (l, y, e)) xs

piecePosition :: Board -> [(Int, Int, Square)]
piecePosition board = [ (x,y,c) | (x, row) <- zip [0..] board, (y,c) <- zip [0..] row, isJust c ]

