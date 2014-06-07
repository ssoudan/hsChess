{-
 Utils.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module Utils where

-- | Apply function at a particular position in a list.
--
-- Keeps the existing value for the other positions.
--
applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt f n xs = map (\ (r,v) -> case r == n of True -> f v
                                                False -> v) (zip [0..] xs)


