module Board where

import Flatten

data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving Eq
data PieceColor = Black | White deriving Eq
data Piece = Piece {pieceType::PieceType, pieceColor::PieceColor} deriving Eq

type Square = Maybe Piece

type Board = [[Square]]

instance Show PieceColor where
    show Black = "B"
    show White = "W"

instance Show PieceType where
    show King = "K"
    show Rook = "R"
    show Queen = "Q"
    show Knight = "N"
    show Pawn = "P"
    show Bishop = "B"

prettyPrintPiece (Piece p White) 
    | p == King = "\x2654"
    | p == Queen = "\x2655"
    | p == Rook = "\x2656"
    | p == Bishop = "\x2657"
    | p == Knight = "\x2658"
    | p == Pawn = "\x2659"

prettyPrintPiece (Piece p Black) 
    | p == King = "\x265A"
    | p == Queen = "\x265B"
    | p == Rook = "\x265C"
    | p == Bishop = "\x265D"
    | p == Knight = "\x265E"
    | p == Pawn = "\x265F"


instance Show Piece where
    show piece@(Piece p c) = prettyPrintPiece piece

-- Just (Piece Rook Black)

emptyBoard = [[Nothing | _ <- [1..8]] | _ <- [1..8]]

initialBoard = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
				[Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
				[Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece King White), Just (Piece Queen White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

prettySquare Nothing = ". "
prettySquare (Just p) = show p ++ " "

prettyBoard::Board->String
prettyBoard = unlines . map prettyPrintLine 
    where 
        prettyPrintLine [] = []
        prettyPrintLine (x:xs) = (prettySquare x)++(prettyPrintLine xs)

-- putStr $ prettyBoard emptyBoard

valuePiece (Piece King _) = 1000
valuePiece (Piece Rook _) = 5
valuePiece (Piece Queen _) = 9
valuePiece (Piece Knight _) = 3
valuePiece (Piece Pawn _) = 1
valuePiece (Piece Bishop _) = 3

instance Ord Piece where 
	 p1 `compare` p2 = (valuePiece p1) `compare` (valuePiece p2)

squareScore Nothing = 0
squareScore (Just piece@(Piece p c)) = valuePiece piece

-- (Piece King Black) > (Piece Queen Black) 
-- :t (fmap squareScore)

-- (flatten emptyBoard)

-- sum (map (sum . fmap squareScore) [[Nothing, Nothing], [Nothing, Just (Piece Knight Black)]])

-- pieceColor (Piece King Black)

evalBoardFor color x = sum $ map squareScore (filter (\x -> case x of Just piece@(Piece _ color) -> True
                                                                      _ -> False) (flatten x))

evalBoard x = let blackScore = evalBoardFor Black x
                  whiteScore = evalBoardFor White x 
              in (blackScore - whiteScore)


