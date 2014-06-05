{-
 MinimaxLazy.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module InteractiveGame where

import           Board
import           Data.List (intersperse)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Move
import           MoveParser (parseMove)


doMove :: State -> IO State
doMove s = do 
              putStrLn $ "[II] " ++ show playerColor ++ " turn"
              putStrLn "[II] Possible moves: " 
              putStrLn . concat $ (intersperse " ") (map show allMoves)
              move <- getLine -- get input (String)
              playMove (parseMove move) s playerColor -- parse and play move if parsing succeeded
     where 
           playerColor = player s
           allMoves = genAllMoves s
           validMove m = Set.member m (Set.fromList allMoves)
           playMove move s playerColor = case move of (Left error) -> do 
                                                                        putStrLn $ show error
                                                                        putStrLn "[EE] Same player play again."
                                                                        return s
                                                      (Right m) -> do 
                                                                    putStrLn $ show m
                                                                    case (validMove m) of True -> do 
                                                                                                   putStrLn $ "[II] Choosen move " ++ show m
                                                                                                   return $ applyMove m s
                                                                                          False -> do 
                                                                                                    putStrLn "[EE] Invalid move!"
                                                                                                    putStrLn "[EE] Same player play again."
                                                                                                    return s