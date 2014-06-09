{-
 History.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}

-- | Move history module.
-- Based on DList to have O(1) inserts
-- We only convert it back to List to get it diplayed
--
module History where

import           Data.DList as DL
import           Move(Move)

type History = DList Move

-- | Create a new empty history
newHistory :: History
newHistory = DL.empty

-- | Append a move to the history
appendHistory :: History -> Move -> History
appendHistory = DL.snoc 

-- | Convert the history to a list of String
historyToList :: History -> [String]
historyToList = DL.toList . (DL.map show)