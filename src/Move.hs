{-
 Move.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module Move where

import           Board
import           Control.Arrow ((***))
import           Data.List
import           Data.Maybe    (listToMaybe)

colorPos :: PieceColor -> Board -> [Pos]
colorPos color board = map (\ (x,y,_) -> (Pos (x,y))) (filter (\(_, _, e) -> case e of Just (Piece _ c) -> color == c
                                                                                       _ -> False) $ piecePosition board)

-- pos initialBoard
-- colorPos White initialBoard

moves :: PieceType -> [(Int, Int)]
moves King   = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
moves Rook   = [(x,0) | x <- [-7..7]] ++ [(0,y) | y <- [-7..7], y /= 0]
moves Queen  = [(x,0) | x <- [-7..7]] ++ [(0,y) | y <- [-7..7], y /= 0] ++ [(y,y) | y <- [-7..7], y /= 0] ++ [(-y,y) | y <- [-7..7], y /= 0]
moves Knight = [(1,2), (2,1), (1,-2), (-2,1), (-1,-2), (-2,-1), (-1,2), (2,-1)]
moves Bishop = [(0,0)] ++ [(y,y) | y <- [-7..7], y /= 0] ++ [(-y,y) | y <- [-7..7], y /= 0]
moves Pawn   = [] -- Pawn move depend on the color and if it is their first move

boardFilter :: Pos -> Bool
boardFilter (Pos (x, y)) = x >= 0 && x <= 7 && y >= 0 && y <= 7

movesAtPosition :: Pos -> PieceType -> [Pos]
movesAtPosition (Pos (x,y)) pt = map (Pos . ((+) x *** (+) y)) (moves pt)

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

filterByDirection :: Direction -> Pos -> (Int, Int, a) -> Bool
filterByDirection direction (Pos (px,py)) (x,y, _) = case direction of W -> px == x && y < py
                                                                       E -> px == x && y > py
                                                                       N -> py == y && x < px
                                                                       S -> py == y && x > px
                                                                       NE -> (py - y) == (x - px) && x < px
                                                                       NW -> (py - y) == (px - x) && x < px
                                                                       SW -> (py - y) == (x - px) && x > px
                                                                       SE -> (py - y) == (px - x) && x > px

distance :: Pos -> (Int, Int, a) -> Int
distance (Pos (px,py)) (x,y,_) = abs (px - x) + abs (py - y)

orderPosition :: Pos -> (Int, Int, a) -> (Int, Int, b) -> Ordering
orderPosition pos p1 p2 = distance pos p1 `compare` distance pos p2

elementByDirection :: Direction -> Pos -> [(Int, Int, a)] -> [(Int, Int, a)]
elementByDirection direction pos = filter (filterByDirection direction pos)

findElementsByDirection :: Pos -> [(Int, Int, a)] -> [[(Int, Int, a)]]
findElementsByDirection pos occupied = map (\d -> elementByDirection d pos occupied) [N,NE,E,SE,S,SW,W,NW]

firstElementByDirection :: Pos -> Board -> [(Direction, Maybe (Int, Int, Square))]
firstElementByDirection pos board = zip [N,NE,E,SE,S,SW,W,NW] (map (listToMaybe . sortBy (orderPosition pos))  (findElementsByDirection pos (piecePosition board)))

forbiddenMoves :: Direction -> Pos -> PieceColor -> Maybe (Int, Int, Square) -> [Pos]
forbiddenMoves _ _ _ Nothing = []
forbiddenMoves dir (Pos (px,py)) _ (Just (ex, ey, Nothing)) = error $ "dir="++ show dir ++" (px,py)=(" ++ show px ++ "," ++ show py ++ ") (ex,ey)=(" ++ show ex ++", "++ show ey ++")"
forbiddenMoves dir (Pos (px,py)) color (Just (ex, ey, Just (Piece _ ecolor))) = case dir of N -> [Pos (a, py) | a <- [0..ex], a /= ex || color == ecolor ]
                                                                                            S -> [Pos (a, py) | a <- [ex..7], a /= ex || color == ecolor ]
                                                                                            E -> [Pos (px, a) | a <- [ey..7], a /= ey || color == ecolor ]
                                                                                            W -> [Pos (px, a) | a <- [0..ey], a /= ey || color == ecolor ]
                                                                                            SE -> [Pos (ex+a, ey+a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (Pos (ex+a, ey+a))]
                                                                                            NE -> [Pos (ex-a, ey+a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (Pos (ex-a, ey+a))]
                                                                                            NW -> [Pos (ex-a, ey-a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (Pos (ex-a, ey-a))]
                                                                                            SW -> [Pos (ex+a, ey-a) | a <- [0..7], (a /= 0 || color == ecolor) && boardFilter (Pos (ex+a, ey-a))]

forbiddenHorizon :: Pos -> PieceColor -> Board -> [Pos]
forbiddenHorizon pos color board = concatMap (\(dir, e) -> forbiddenMoves dir pos color e) (firstElementByDirection pos board)

-- forbiddenHorizon (7,0) Black initialBoard
-- forbiddenHorizon (7,2) White initialBoard
-- (movePos (6,0) (5,0) initialBoard)

-- [[Just ♜,Just ♞,Just ♝,Just ♛,Just ♚,Just ♝,Just ♞,Just ♜],
--  [Just ♟,Just ♟,Just ♟,Just ♟,Just ♟,Just ♟,Just ♟,Just ♟],
--  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
--  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
--  [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
--  [Just ♙,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
--  [Nothing,Just ♙,Just ♙,Just ♙,Just ♙,Just ♙,Just ♙,Just ♙],
--  [Just ♖,Just ♘,Just ♗,Just ♔,Just ♕,Just ♗,Just ♘,Just ♖]]

-- firstElementByDirection (7,0) (movePos (6,0) (5,0) initialBoard)
-- [(N,Just (6,0,Nothing)),
--  (NE,Just (6,1,Just ♙)),
--  (E,Just (7,1,Just ♘)),
--  (SE,Nothing),
--  (S,Nothing),
--  (SW,Nothing),
--  (W,Nothing),
--  (NW,Nothing)]

-- (findElementsByDirection (7,0) (piecePosition (movePos (6,0) (5,0) initialBoard)))
-- N [[(0,0,Just ♜),(1,0,Just ♟),(2,0,Nothing),(3,0,Nothing),(4,0,Nothing),(5,0,Just ♙),(6,0,Nothing)],
-- NE [(0,7,Just ♜),(1,6,Just ♟),(2,5,Nothing),(3,4,Nothing),(4,3,Nothing),(5,2,Nothing),(6,1,Just ♙)],
-- ..  [(7,1,Just ♘),(7,2,Just ♗),(7,3,Just ♔),(7,4,Just ♕),(7,5,Just ♗),(7,6,Just ♘),(7,7,Just ♖)]
-- ..  ,[],[],[],[],[]]


-- firstElementByDirection (0,0) initialBoard

otherPlayer :: PieceColor -> PieceColor
otherPlayer Black = White
otherPlayer White = Black

-- | 'makePawnLegalMoves' generates the possible moves of a pawn depending on its position, 
-- its color, and the positions of the other player
-- 
-- >>> makePawnLegalMoves Black (Pos (1,0)) initialBoard
-- [Pos (2,0),Pos (3,0)]
makePawnLegalMoves :: PieceColor  -- ^ The color of the pawn
                      -> Pos      -- ^ The position of the pawn
                      -> Board    -- ^ The board 
                      -> [Pos]    -- ^ Returns the list of possible positions for this pawn
makePawnLegalMoves color (Pos (px,py)) board = let otherPlayerPosition = colorPos (otherPlayer color) board
                                                   validMoves = case color of Black -> [Pos (px + a, py) | a <- [1,2], a /= 2 || px == 1]
                                                                              White -> [Pos (px - a, py) | a <- [1,2], a /= 2 || px == 6]
                                                   optMoves = case color of Black -> [Pos (px + 1, py + 1), Pos (px + 1, py - 1)]
                                                                            White -> [Pos (px - 1, py + 1), Pos (px - 1, py - 1)]
                                                in [Pos (x,y) | (Pos (x,y)) <- (validMoves \\ otherPlayerPosition) ++ (optMoves `intersect` otherPlayerPosition), boardFilter (Pos (x, y))]

-- | 'genValidMoves' generates the possibles moves any piece on the board based on its legal 
-- moves and the positions of the other pieces.
--
-- >>> let (newBoard, _) = (movePos (Pos (1,3)) (Pos (5,3)) initialBoard) in genValidMoves newBoard (Pos (5,3))
-- [Pos (6,4),Pos (6,2)]
genValidMoves :: Board -> Pos -> [Pos]
genValidMoves board pos = let (Just (Piece pt color)) = elementAt pos board
                           in let opt = case pt of Pawn -> makePawnLegalMoves color pos board
                                                   _ -> filter boardFilter (movesAtPosition pos pt) \\ [pos]
                                  forbidden = case pt of Knight -> colorPos color board
                                                         Pawn -> [] -- makePawnLegalMoves already excludes the position of the other player
                                                         King -> colorPos color board
                                                         Queen -> forbiddenHorizon pos color board
                                                         Rook -> forbiddenHorizon pos color board
                                                         Bishop -> forbiddenHorizon pos color board
                               in opt \\ forbidden


genMoves :: Board -> Pos -> [BoardWithMove]
genMoves board pos = map (\newpos -> movePos pos newpos board) $ genValidMoves board pos

-- genMoves initialBoard (Pos (1,0))

data State = State { current :: Board, move :: String, player :: PieceColor }

instance Show State where
    show (State cur m p) = "-> move: " ++ m 
                      ++ "\n-> player: " ++ show p 
                      ++ "\n-> score: " ++ show (evalBoard cur) 
                      ++ "\n"

showState :: State -> String
showState (State cur m p) = "move: " ++ m 
                       ++ "\n" ++ prettyBoard cur 
                       ++ "\n-> player: " ++ show p 
                       ++ "\n-> score: " ++ show (evalBoard cur) 
                       ++ "\n"

nextStates :: State -> [State]
nextStates (State cur _ p) = let pieces = colorPos p cur
                            in concatMap (\m -> [ State newboard notation (otherPlayer p) | (newboard, notation) <- genMoves cur m]) pieces


