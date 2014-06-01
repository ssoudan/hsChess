{-
 Utils.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module Utils where

applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt f n xs = map (\ (r,v) -> case r == n of True -> f v
                                                False -> v) (zip [0..] xs)


