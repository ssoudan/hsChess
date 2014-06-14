{-
 MinimaxLazy.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module InteractiveGame (doMove, playMove) where

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
              playMove moveCommandInput s -- parse and play move if parsing succeeded
     where
           playerColor :: PieceColor
           playerColor = getPlayer s



           
playMove :: String -> State -> IO State
playMove playerMove previousState = case parsedPlayerMove of (Left _) -> do
                                                                            putStrLn $ "Failed to parse: " ++ playerMove
                                                                            putStrLn "[EE] Same player play again."
                                                                            return previousState
                                                             (Right m) -> if validMove m then (do
                                                                                                 putStrLn $ "[II] Choosen move for " ++ show (getPlayer previousState) ++ ": " ++ show m
                                                                                                 return $ applyMove m previousState)
                                                                                         else (do
                                                                                                 putStrLn "[EE] Invalid move!"
                                                                                                 putStrLn "[EE] Same player play again."
                                                                                                 return previousState)
                          where allMoves :: [Move]
                                allMoves = genAllMoves previousState
                                validMove :: Move -> Bool
                                validMove m = Set.member m (Set.fromList allMoves)
                                parsedPlayerMove = parseMove playerMove