module Move where

import Board
import Flatten
import Utils
import Data.List

colorPos :: PieceColor -> Board -> [Pos]
colorPos color board = map (\ (x,y,_) -> (x,y)) (filter (\(_, _, e) -> case e of Just (Piece _ c) -> color == c 
                                                                                 _ -> False) $ piecePosition board)

-- pos initialBoard
-- colorPos White initialBoard

moves :: PieceType -> [(Int,Int)]
moves King   = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
moves Rook   = [(x,0) | x <- [-7..7]] ++ [(0,y) | y <- [-7..7], y /= 0]
moves Queen  = [(x,0) | x <- [-7..7]] ++ [(0,y) | y <- [-7..7], y /= 0] ++ [(y,y) | y <- [-7..7], y /= 0] ++ [(-y,y) | y <- [-7..7], y /= 0]
moves Knight = [(1,2), (2,1), (1,-2), (-2,1), (-1,-2), (-2,-1), (-1,2), (2,-1)]
moves Bishop = [(0,0)] ++ [(y,y) | y <- [-7..7], y /= 0] ++ [(-y,y) | y <- [-7..7], y /= 0]
moves Pawn   = []

boardFilter :: Pos -> Bool
boardFilter (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

movesAtPosition :: Pos -> PieceType -> [Pos] 
movesAtPosition (x,y) pt = map (\(lx,ly) -> (x+lx, y+ly)) (moves pt)

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

-- sortByDirection (px,py) (x,y) | px == x && y > py = N
--                               | px == x && y < py = S
--                               | py == y && x > px = E
--                               | py == y && x < px = W
--                               | (py - y) == (px - x) && x > px = NE
--                               | (py - y) == (x - px) && x > px = SE
--                               | (py - y) == (x - px) && x < px = NW
--                               | (py - y) == (px - x) && x < px = SW

filterByDirection :: Direction -> Pos -> (Int, Int, a) -> Bool
filterByDirection W (px,py) (x,y, _) = px == x && y < py
filterByDirection E (px,py) (x,y, _) = px == x && y > py
filterByDirection N (px,py) (x,y, _) = py == y && x < px
filterByDirection S (px,py) (x,y, _) = py == y && x > px
filterByDirection NE (px,py) (x,y, _) = (py - y) == (x - px) && x < px
filterByDirection NW (px,py) (x,y, _) = (py - y) == (px - x) && x < px
filterByDirection SW (px,py) (x,y, _) = (py - y) == (x - px) && x > px
filterByDirection SE (px,py) (x,y, _) = (py - y) == (px - x) && x > px

distance :: Pos -> (Int, Int, a) -> Int
distance (px,py) (x,y,_) = abs (px - x) + abs (py - y) 

orderPosition :: Pos -> (Int, Int, a) -> (Int, Int, b) -> Ordering 
orderPosition pos p1 p2 = (distance pos p1) `compare` (distance pos p2)

elementByDirection :: Direction -> Pos -> [(Int, Int, a)] -> [(Int, Int, a)]
elementByDirection direction pos occupied = filter (filterByDirection direction pos) occupied

findElementsByDirection :: Pos -> [(Int, Int, a)] -> [[(Int, Int, a)]]
findElementsByDirection pos occupied = map (\d -> elementByDirection d pos occupied) [N,NE,E,SE,S,SW,W,NW]

firstElementByDirection :: Pos -> Board -> [(Direction, [(Int, Int, Square)])]
firstElementByDirection pos board = zip [N,NE,E,SE,S,SW,W,NW] (map (\ xs -> take 1 (sortBy (orderPosition pos) xs))  (findElementsByDirection pos (piecePosition board)))
-- :t firstElementByDirection

forbiddenMoves :: Direction -> Pos -> PieceColor -> [(Int, Int, Square)] -> [Pos]
forbiddenMoves _ _ _ ((_, _, Nothing):_) = error "invalid "
forbiddenMoves dir (px,py) color ((ex, ey, Just (Piece _ ecolor)):xs) = case dir of N -> [(a, py) | a <- [0..ex], (a /= ex || color == ecolor) ] 
                                                                                    S -> [(a, py) | a <- [ex..7], (a /= ex || color == ecolor) ] 
                                                                                    E -> [(px, a) | a <- [ey..7], (a /= ey || color == ecolor) ] 
                                                                                    W -> [(px, a) | a <- [0..ey], (a /= ey || color == ecolor) ] 
                                                                                    SE -> [(ex+a, ey+a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (ex+a, ey+a)]
                                                                                    NE -> [(ex-a, ey+a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (ex-a, ey+a)]
                                                                                    NW -> [(ex-a, ey-a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (ex-a, ey-a)]
                                                                                    SW -> [(ex+a, ey-a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (ex+a, ey-a)]
forbiddenMoves _ _ _ [] = []

-- horizon pos@(px,py) color board = concatMap (\(dir, e) -> buildHorizonMoves dir pos color e) (firstElementByDirection pos board)
forbiddenHorizon :: Pos -> PieceColor -> Board -> [Pos]
forbiddenHorizon pos color board = concatMap (\(dir, e) -> forbiddenMoves dir pos color e) (firstElementByDirection pos board)

-- forbiddenHorizon (7,2) White initialBoard
-- firstElementByDirection (7,2) initialBoard
-- firstElementByDirection (0,0) initialBoard

otherPlayer :: PieceColor -> PieceColor
otherPlayer Black = White
otherPlayer White = Black

makePawnLegalMoves :: PieceColor -> Pos -> Board -> [Pos]
makePawnLegalMoves color (px,py) board = let otherPlayerPosition = colorPos (otherPlayer color) board
                                             validMoves = case color of Black -> [(px + a, py) | a <- [1,2], a /= 2 || px == 1]
                                                                        White -> [(px - a, py) | a <- [1,2], a /= 2 || px == 6]
                                             optMoves = case color of Black -> [(px + 1, py + 1), (px + 1, py - 1)]
                                                                      White -> [(px - 1, py + 1), (px - 1, py - 1)]
                                          in [(x,y) | (x,y) <- (validMoves \\ otherPlayerPosition) ++ (optMoves `intersect` otherPlayerPosition), boardFilter (x, y)] 

-- makePawnLegalMoves Black (1,0) initialBoard

genValidMoves :: Board -> Pos -> [Pos]
genValidMoves board pos = let (Just (Piece pt color)) = elementAt pos board
                           in let opt = case pt of Pawn -> makePawnLegalMoves color pos board
                                                   _ -> ((filter boardFilter $ movesAtPosition pos pt) \\ [pos])
                                  forbidden = case pt of Knight -> colorPos color board
                                                         Pawn -> colorPos color board
                                                         King -> colorPos color board
                                                         Queen -> forbiddenHorizon pos color board
                                                         Rook -> forbiddenHorizon pos color board
                                                         Bishop -> forbiddenHorizon pos color board
                               in opt \\ forbidden

-- genValidMoves (movePos (1,3) (5,3) initialBoard) (5,3)

genMoves :: Board -> Pos -> [Board]
genMoves board pos = map (\newpos -> movePos pos newpos board) $ genValidMoves board pos

-- :t genMoves

-- genMoves initialBoard (1,0) 
data State = State { current :: Board, player :: PieceColor } 

instance Show State where
    show state = case state of (State current player) -> "player: " ++ (show player) ++ "\n" ++ (prettyBoard current)

nextStates :: State -> [State]
nextStates (State current player) = let pieces = colorPos player current
                                   in concatMap (\move -> [ State newboard (otherPlayer player) | newboard <- (genMoves current move)]) pieces


