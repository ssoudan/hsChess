{-
 Options.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module Options where


data AssistantOption = NotAssisted | AssistedM | AssistedML | AssistedAB deriving (Eq, Show, Read)
data OpponentOption = M | ML | AB deriving (Eq, Show, Read)
data UIOption = GUI | TextUI deriving (Eq, Show, Read)

type Options = (Maybe AssistantOption, Maybe OpponentOption, Maybe UIOption)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

