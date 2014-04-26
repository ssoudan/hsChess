module Board where

import Flatten
import Utils

data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving Eq
data PieceColor = Black | White deriving Eq
data Piece = Piece {pieceType::PieceType, pieceColor::PieceColor} deriving Eq

type Square = Maybe Piece

type Board = [[Square]]

type Pos = (Int, Int)

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

prettyPrintPiece :: Piece -> String
prettyPrintPiece (Piece p White) | p == King = "\x2654"| p == Queen = "\x2655"| p == Rook = "\x2656"| p == Bishop = "\x2657"| p == Knight = "\x2658"| p == Pawn = "\x2659"
prettyPrintPiece (Piece p Black) | p == King = "\x265A"| p == Queen = "\x265B"| p == Rook = "\x265C"| p == Bishop = "\x265D"| p == Knight = "\x265E"| p == Pawn = "\x265F"
prettyPrintPiece (Piece _ _) = "??"

instance Show Piece where
    show piece = prettyPrintPiece piece

-- Just (Piece Rook Black)
emptyBoard :: Board
emptyBoard = [[Nothing | _ <- [1..8]] | _ <- [1..8]]

initialBoard :: Board
initialBoard = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
				[Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
				[Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
				[Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece King White), Just (Piece Queen White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

prettySquare :: Square -> String
prettySquare Nothing = ". "
prettySquare (Just p) = show p ++ " "

prettyPrintLine :: [Square] -> String
prettyPrintLine [] = []
prettyPrintLine (x:xs) = (prettySquare x)++(prettyPrintLine xs)

prettyBoard::Board->String
prettyBoard board = unlines $ map prettyPrintLine board

-- putStr $ prettyBoard emptyBoard

valuePiece :: Piece -> Int
valuePiece (Piece King _) = 1000
valuePiece (Piece Rook _) = 5
valuePiece (Piece Queen _) = 9
valuePiece (Piece Knight _) = 3
valuePiece (Piece Pawn _) = 1
valuePiece (Piece Bishop _) = 3

instance Ord Piece where 
	 p1 `compare` p2 = (valuePiece p1) `compare` (valuePiece p2)

squareScore :: Square -> Int
squareScore Nothing = 0
squareScore (Just piece) = valuePiece piece

-- (Piece King Black) > (Piece Queen Black) 
-- :t (fmap squareScore)

-- (flatten emptyBoard)

-- sum (map (sum . fmap squareScore) [[Nothing, Nothing], [Nothing, Just (Piece Knight Black)]])

-- pieceColor (Piece King Black)

evalBoardFor :: PieceColor -> Board -> Int
evalBoardFor color x = sum $ map squareScore (filter (\p -> case p of Just (Piece _ c) -> c == color
                                                                      _ -> False) (flatten x))


evalBoard :: Board -> Int
evalBoard x = let blackScore = evalBoardFor Black x
                  whiteScore = evalBoardFor White x 
              in (blackScore - whiteScore)


applyOnBoard :: (Square -> Square) -> Pos -> Board -> Board
applyOnBoard f (x,y) board = applyAt (\row -> applyAt f y row) x board

updateBoard :: Pos -> Square -> Board -> Board
updateBoard pos piece board = applyOnBoard (\_ -> piece) pos board

deleteSquare :: Pos -> Board -> Board
deleteSquare pos board = updateBoard pos (Nothing) board

-- prettyBoard (updateBoard (3,3) (Just (Piece Bishop Black)) emptyBoard)
-- prettyBoard (deleteSquare (0,0) initialBoard)

elementAt :: Pos -> Board -> Square
elementAt (x,y) board = (board!!x)!!y

-- elementAt (0,0) initialBoard 

movePos :: Pos -> Pos -> Board -> Board
movePos origin destination board = let piece = elementAt origin board
                                       board' = deleteSquare origin board
                                    in updateBoard destination piece board'
-- :t movePos
-- prettyBoard $ movePos (0,0) (2,0) initialBoard
distributeLabel :: (Int, [(a,b)]) -> [(Int, a, b)]
distributeLabel (l, xs) = map (\(y,e) -> (l, y, e)) xs

piecePosition :: Board -> [(Int, Int, Square)]
piecePosition board = concatMap distributeLabel $ zip [0..] (map (zip [0..]) board)

colorPos :: PieceColor->Board->[Pos]
colorPos color board = map (\(x,y,_) -> (x,y)) $ filter (\(_, _, e) -> case e of Just (Piece _ c) -> color == c 
                                                                                 _ -> False) $ piecePosition board

moves::PieceType->[(Int,Int)]
moves King   = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
moves Rook   = [(x,0) | x <- [-7..7]] ++ [(0,y) | y <- [-7..7], y /= 0]
moves Queen  = [(x,0) | x <- [-7..7]] ++ [(0,y) | y <- [-7..7], y /= 0] ++ [(y,y) | y <- [-7..7], y /= 0] ++ [(-y,y) | y <- [-7..7], y /= 0]
moves Knight = [(x,0) | x <- [-1..1]] ++ [(0,y) | y <- [-1..1], y /= 0] ++ [(y,y) | y <- [-1..1], y /= 0] ++ [(-y,y) | y <- [-1..1], y /= 0]
moves Bishop = [(0,0)] ++ [(y,y) | y <- [-7..7], y /= 0] ++ [(-y,y) | y <- [-7..7], y /= 0]
moves Pawn   = []

-- pos initialBoard
--colorPos White initialBoard