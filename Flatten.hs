{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FlexibleInstances #-}
module Flatten where

class Flatten i o where
  flatten :: [i] -> [o]

instance Flatten a a where
  flatten = id

instance Flatten i o => Flatten [i] o where 
  flatten = concatMap flatten