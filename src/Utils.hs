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
applyAt f n xs = map (\ (r,v) -> (if r == n then f v else v)) (zip [0..] xs)

first3 :: (a->b) -> (a,c,d) -> (b,c,d)
first3 f (a, c, d) = (f a, c, d)

second3 :: (c->b) -> (a,c,d) -> (a,b,d)
second3 f (a, c, d) = (a, f c, d)

thrid3 ::  (d->b) -> (a,c,d) -> (a,c,b)
thrid3 f (a, c, d) = (a, c, f d)

fst3 :: (a,b,c) -> a
fst3 (a, _, _) = a

snd3 :: (a,b,c) -> b
snd3 (_, b, _) = b

thrd3 :: (a,b,c) -> c
thrd3 (_, _, c) = c