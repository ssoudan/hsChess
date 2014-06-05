module MoveParser where

import           Board                              (Pos(..))
import           Move                               (Move, makeMove)

import           Text.ParserCombinators.Parsec
import           Data.Char

--
--  Grammar:
--  move: source destination
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
                            sourceRow <- anyChar
                            sourceColumn <- digit
                            destinationRow <- anyChar
                            destinationColumn <- digit
                            return $ makeMove (Pos (colToInt sourceColumn, rowToInt sourceRow)) (Pos (colToInt destinationColumn, rowToInt destinationRow))            
            rowToInt :: Char -> Int
            rowToInt x = (ord x) - 97
            colToInt :: Char -> Int
            colToInt = digitToInt 


