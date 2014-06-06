{-
 MoveParser.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module MoveParser where

import           Board                         (Pos (..))
import           Move                          (Move, makeMove)

import           Data.Char
import           Text.ParserCombinators.Parsec

--
--  Grammar:
--  move: source dst
--
--  source: row column
--
--  destination: row column
--
--  row: [a-h]
--
--  column: [0-7]
--

parseMove :: String -> Either ParseError Move
parseMove = parse doParseMove "failed"
      where doParseMove = do
                            srcRow <- anyChar
                            srcColumn <- digit
                            dstRow <- anyChar
                            dstColumn <- digit
                            return $ makeMove (Pos (colToInt srcColumn, rowToInt srcRow)) (Pos (colToInt dstColumn, rowToInt dstRow))
            rowToInt :: Char -> Int
            rowToInt x = (ord x) - 97
            colToInt :: Char -> Int
            colToInt = digitToInt


