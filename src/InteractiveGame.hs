{-
 MinimaxLazy.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module InteractiveGame where

import           Board (PieceColor)
import           Data.List (intersperse)
import qualified Data.Set as Set
import           Move
import           MoveParser (parseMove)


doMove :: State -> IO State
doMove s = do 
              putStrLn $ "[II] " ++ show playerColor ++ " turn"
              putStrLn "[II] Possible moves: " 
              putStrLn . concat $ (intersperse " ") (map show allMoves)
              moveCommandInput <- getLine -- get input (String)
              playMove (parseMove moveCommandInput) s -- parse and play move if parsing succeeded
     where 
           playerColor :: PieceColor
           playerColor = player s
           allMoves :: [Move]
           allMoves = genAllMoves s
           validMove :: Move -> Bool
           validMove m = Set.member m (Set.fromList allMoves)
           playMove :: Show a => (Either a Move) -> State -> IO State
           playMove playerMove previousState = case playerMove of (Left msg) -> do 
                                                                                    putStrLn $ show msg
                                                                                    putStrLn "[EE] Same player play again."
                                                                                    return previousState
                                                                  (Right m) -> do 
                                                                                 putStrLn $ show m
                                                                                 case (validMove m) of True -> do 
                                                                                                                 putStrLn $ "[II] Choosen move " ++ show m
                                                                                                                 return $ applyMove m previousState
                                                                                                       False -> do 
                                                                                                                  putStrLn "[EE] Invalid move!"
                                                                                                                  putStrLn "[EE] Same player play again."
                                                                                                                  return previousState