{-
 MinimaxLazy.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module InteractiveGame (doMove) where

import           Board      (PieceColor)
import qualified Data.Set   as Set
import           Move
import           MoveParser (parseMove)
import           State


doMove :: State -> IO State
doMove s = do
              putStrLn $ "[II] " ++ show playerColor ++ " turn\nWhat do want to play? (e.g. a6b7)"
              --putStrLn "[II] Possible moves: "
              --putStrLn . concat $ (intersperse " ") (map show allMoves)
              moveCommandInput <- getLine -- get input (String)
              playMove (parseMove moveCommandInput) s -- parse and play move if parsing succeeded
     where
           playerColor :: PieceColor
           playerColor = getPlayer s
           allMoves :: [Move]
           allMoves = genAllMoves s
           validMove :: Move -> Bool
           validMove m = Set.member m (Set.fromList allMoves)
           playMove :: Show a => Either a Move -> State -> IO State
           playMove playerMove previousState = case playerMove of (Left msg) -> do
                                                                                    print msg
                                                                                    putStrLn "[EE] Same player play again."
                                                                                    return previousState
                                                                  (Right m) -> if validMove m then (do
                                                                                                      putStrLn $ "[II] Choosen move for " ++ show (getPlayer previousState) ++ ": " ++ show m
                                                                                                      return $ applyMove m previousState)
                                                                                              else (do
                                                                                                      putStrLn "[EE] Invalid move!"
                                                                                                      putStrLn "[EE] Same player play again."
                                                                                                      return previousState)
