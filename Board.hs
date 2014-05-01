module Board where

import Utils
import Data.List

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
prettyPrintLine [] = []
prettyPrintLine (x:xs) = (prettySquare x)++(prettyPrintLine xs)

prettyBoard::Board->String
prettyBoard board = ("  ---- B ----  \n" ++ unlines (map prettyPrintLine board) ++ "  ---- W ----  \n")

-- putStr $ prettyBoard emptyBoard

valuePiece :: Piece -> Int
valuePiece (Piece King c) = 1000
valuePiece (Piece Rook c) = 5
valuePiece (Piece Queen c) = 9
valuePiece (Piece Knight c) = 3
valuePiece (Piece Pawn c) = 1
valuePiece (Piece Bishop c) = 3

instance Ord Piece where 
	p1 `compare` p2 = (valuePiece p1) `compare` (valuePiece p2)

squareScore :: Square -> Int
squareScore (Just piece) = valuePiece piece
squareScore Nothing = 0


-- (Piece King Black) > (Piece Queen Black) 
-- :t (fmap squareScore)

-- sum (map (sum . fmap squareScore) [[Nothing, Nothing], [Nothing, Just (Piece Knight Black)]])

-- pieceColor (Piece King Black)


--evalBoardFor :: PieceColor -> Board -> Int
--evalBoardFor color x = sum $ map squareScore $ concatMap (filter (\p -> case p of Just (Piece _ c) -> c == color
--                                                                                  _ -> False)) x

evalBoardFor :: [Square] -> Int
evalBoardFor x = sum $ map squareScore x

isPiece :: Square -> Bool
isPiece Nothing = False
isPiece _ = True

isBlack :: Square -> Bool
isBlack (Just (Piece _ Black)) = True
isBlack (Just (Piece _ White)) = False 

evalBoard :: Board -> Int
evalBoard board = let blackScore = evalBoardFor blacks
                      whiteScore = evalBoardFor whites 
                   in (blackScore - whiteScore)
                where (blacks, whites) =  partition isBlack $ filter isPiece $ concat board

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

showPos (x,y) = (['a'..]!!y):show x

type BoardWithMove = (Board, String)

nameMove :: Board -> Pos -> Square -> Pos -> String
nameMove board pos (Just (Piece pieceType _)) destination = let destPiece = elementAt destination board
                                     in case destPiece of Nothing -> show pieceType ++ showPos destination
                                                          Just _ ->  show pieceType ++ "x" ++ showPos destination

movePos :: Pos -> Pos -> Board -> BoardWithMove
movePos origin destination board = let piece = elementAt origin board
                                       board' = deleteSquare origin board
                                    in (updateBoard destination piece board', nameMove board origin piece destination)
-- :t movePos
-- prettyBoard $ movePos (0,0) (2,0) initialBoard
-- distributeLabel :: (Int, [(a,b)]) -> [(Int, a, b)]
-- distributeLabel (l, xs) = map (\(y,e) -> (l, y, e)) xs

piecePosition :: Board -> [(Int, Int, Square)]
piecePosition board = [ (x,y,c) | (x, row) <- (zip [0..] board), (y,c) <- (zip [0..] row), c /= Nothing ]

