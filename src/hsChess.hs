{-
 hsChess.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module Main where

import           Board
import           InteractiveGame  as Paul
import           Minimax          as M
import           MinimaxAlphaBeta as AB
import           MinimaxLazy      as ML
import           State

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
Define a function movePieceOnBoard::Pos->Pos->Board->Board such that movePieceOnBoard p1 p2 b moves the piece at p1 to p2 (overwrite the square at p2 with
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


-- TODO:
--
--    [√] add manual strategy
--    [√] improve UX
--    [ ] mate state detection
--    [ ] stalemate detection
--    [ ] check state detection and move selection
--    [ ] web or GUI


alternate :: (a->a) -> (a->a) -> a -> [a]
alternate f g a = a : alternate g f (f a)

playATurn :: Options -> State -> IO State
playATurn options state = do
                            case (getPlayer state) of White -> do
                                                              s' <- whiteStrategy state
                                                              assistantStrategy state
                                                              putStr $ show s'
                                                              return s'
                                                      Black -> do
                                                              s' <- return $ blackStrategy state
                                                              putStr $ show s'
                                                              return s'
                  where (assistantOption, opponentOption) = options
                        whiteStrategy = Paul.doMove
                        blackStrategy = case opponentOption of ML -> ML.doMove
                                                               AB -> AB.doMove
                                                               M -> M.doMove
                        assistantStrategy s = case assistantOption of AssistedML -> do
                                                                                      putStrLn "[ML] Thinking to what I would have played..." 
                                                                                      (print . ML.doMove) s
                                                                                      putStrLn "[ML] But you played:" 

                                                                      AssistedAB -> do
                                                                                      putStrLn "[AB] Thinking to what I would have played..."
                                                                                      (print . AB.doMove) s
                                                                                      putStrLn "[AB] But you played:" 
                                                                      AssistedM -> do
                                                                                     putStrLn "[M] Thinking to what I would have played..."
                                                                                     (print . M.doMove) s
                                                                                     putStrLn "[M] But you played:" 
                                                                      NotAssisted -> (const (return ())) s



playForEver :: Options -> State -> IO State
playForEver options state = do
                              s <- playATurn options state
                              nextState <- playForEver options s
                              return nextState

banner :: IO ()
banner = do
           putStrLn "                                                  "
           putStrLn "88  88 .dP\"Y8  dP\"\"b8 88  88 888888 .dP\"Y8 .dP\"Y8 "
           putStrLn "88  88 `Ybo.\" dP   `\" 88  88 88__   `Ybo.\" `Ybo.\" "
           putStrLn "888888 o.`Y8b Yb      888888 88\"\"   o.`Y8b o.`Y8b "
           putStrLn "88  88 8bodP'  YboodP 88  88 888888 8bodP' 8bodP' "
           putStrLn "                                       - @ssoudan "
           putStrLn "                                                  "

getOptions :: IO Options
getOptions = do
               putStrLn "Assistant options are: "
               putStrLn " * NotAssisted: play like a grown up"
               putStrLn " * AssistedM: show the the M strategy would have played"
               putStrLn " * AssistedML: show the the ML strategy would have played"
               putStrLn " * AssistedAB: show the the AB strategy would have played"
               putStrLn "Which one do you want?"
               assistantOption <- getLine


               putStrLn " * M: play against the M strategy"
               putStrLn " * ML: play against the M strategy"
               putStrLn " * AB: play against the M strategy"
               putStrLn "Which one do you want to be defeated by?"
               opponentOption <- getLine
               return ((read assistantOption) :: AssistantOption, (read opponentOption) :: OpponentOption)

main :: IO ()
main = do
        banner
        options <- getOptions
        let initState = State jeuOuvert "init" White
        putStr $ show initState
        endState <- playForEver options initState
        putStr $ show endState


data AssistantOption = NotAssisted | AssistedM | AssistedML | AssistedAB deriving (Eq, Show, Read)
data OpponentOption = M | ML | AB deriving (Eq, Show, Read)

type Options = (AssistantOption, OpponentOption)
