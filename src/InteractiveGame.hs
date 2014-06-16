{-
 MinimaxLazy.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module InteractiveGame (doMove, playMove) where

import           Board      (PieceColor)
import           MoveParser (parseMove)
import           State


doMove :: SuperState -> IO SuperState
doMove s = do
              putStrLn $ "[II] " ++ show playerColor ++ " turn\nWhat do want to play? (e.g. a6b7)"
              --putStrLn "[II] Possible moves: "
              --putStrLn . concat $ (intersperse " ") (map show allMoves)
              moveCommandInput <- getLine -- get input (String)
              playMove moveCommandInput s -- parse and play move if parsing succeeded
     where
           playerColor :: PieceColor
           playerColor = (getPlayer . fst) s



           
playMove :: String -> SuperState -> IO SuperState
playMove playerMove previousState = case parseMove playerMove of (Left _) -> do
                                                                                putStrLn $ "Failed to parse: " ++ playerMove
                                                                                putStrLn "[EE] Same player play again."
                                                                                return previousState
                                                                 (Right m) -> if validMove m previousState then (do
                                                                                                                   putStrLn $ "[II] Choosen move for " ++ show ((getPlayer . fst) previousState) ++ ": " ++ show m
                                                                                                                   return $ applyMove m (fst previousState))
                                                                                                           else (do
                                                                                                                   putStrLn "[EE] Invalid move!"
                                                                                                                   putStrLn "[EE] Same player play again."
                                                                                                                   return previousState)

