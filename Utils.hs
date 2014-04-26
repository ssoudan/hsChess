module Utils where

applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt f n xs = map (\ (r,v) -> case r == n of True -> f v
                                                False -> v) (zip [0..] xs)