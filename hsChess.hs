module Main where

import Board
import Move
import Minimax

{-|
From [http://www.haskell.org/haskellwiki/Learning_Haskell_with_Chess#Exercise_3_-_gametree_generation_and_minimax_algorithm]

1 Exercise 1 - data types

1.1 Learning targets
recapitulate Haskell types (keywords type and data, product and sum types)
Helium: equality and show functions (pattern matching)
Haskell: type classes (Show, Eq, deriving)
list handling (boards will be represented by lists of lists)
special character '\n', putStr::String->IO ()

1.2 Tasks
Define data types that represent boards (Board), squares (Square), positions (Pos), pieces (Piece, supported by PieceColor and PieceType) 
and game states (State).
Helium: Implement suited eq and show functions.
Haskell: Define/derive instances of Show and Eq
Implement a function prettyBoard::Board->String, that transforms a board into a clearly arranged string representation (human readable :-)). 
Support this function with auxiliary functions that pretty print pieces, squares, ...
Define the initial board (initialBoard::Board), test prettyBoard with initialBoard.
Implement a simple evaluation function evalBoard::Board->Int as the difference of material on board, for this purpose define a function 
valuePiece that maps pieces to their values (pawn->1, knight and bishop->3, queen->9, rook->5, king->"infinity"=1000).

2 Exercise 2 - move generator

2.1 Learning targets
list comprehension
stepwise refinement

2.2 Tasks
Define a function movePos::Pos->Pos->Board->Board such that movePos p1 p2 b moves the piece at p1 to p2 (overwrite the square at p2 with 
the square at p1 and delete p1). 
Do not check whether the move is valid at this stage. 
Hint: Define a function updateBoard first. On top of this further define a function deleteSquare. Its much simpler to have these functions 
at hand.
Implement a function colorPos::PieceColor->Board->[Pos] whose result is the list of the positions of the pieces of a certain color.
Define moves::PieceType->[(Int,Int)] that returns the base moving vectors for a particular piece (return empty list for pawns here, because 
their vector depends on the color).
Implement genMoves::Board->Pos->[Board], it takes a board b and a position p and results in the list of all boards that may follow if you 
move the piece at p on board b.
Combine colorPos and genMoves to a function nextStates::State->[State] that returns all possible successor states.

3 Exercise 3 - gametree generation and minimax algorithm

3.1 Learning targets
break code in modules
complexity
recursive data structures -> recursive algorithms

3.2 Tasks
Define a data type that represents a game tree (GameTree).
Roughly estimate the number of nodes of the gametree with depth 4.
Define a function play::Gametree->Int, that computes the value of a given game tree using the minimax Algorithm.
Implement the function doMove::State->State, that choses the (best) next state.

-}


main = do 
        putStrLn $ "board score's is: " ++ (show $ evalBoard initialBoard)
        putStrLn $ "black score's is: " ++ (show $ evalBoardFor Black initialBoard)
        putStrLn $ "white score's is: " ++ (show $ evalBoardFor White initialBoard)        

        putStrLn ""
        --putStr (prettyBoard initialBoard)

        -- sequence $ map print (nextStates (State initialBoard White))

        -- iterate print (doMove (State initialBoard White))
        sequence $ map print $ take 300 (iterate doMove (State initialBoard White))