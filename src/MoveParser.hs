{-
 MoveParser.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module MoveParser where

import           Board                         (Pos (..))
import           Move                          (Move(..), makeMove)

import           Data.Char
import           Text.ParserCombinators.Parsec

--
--  Grammar:
--  move: source dst
--        | 'wlc'
--        | 'wrc'
--        | 'blc'
--        | 'brc'
--
--  source: row column
--
--  destination: row column
--
--  row: [a-h]
--
--  column: [0-7]
--

-- | Parser a 'Move' from a 'String'
-- Note that it returns an Either. 
-- The Move is in the Right, the error in the Left -- this is not a political statement.
parseMove :: String -> Either ParseError Move
parseMove = parse (choice [doParseWLCastle, doParseWRCastle, doParseBLCastle, doParseBRCastle, doParseSimpleMove]) "failed"
      where doParseSimpleMove = do
                                    srcRow <- anyChar
                                    srcColumn <- digit
                                    dstRow <- anyChar
                                    dstColumn <- digit
                                    return $ makeMove (Pos (colToInt srcColumn, rowToInt srcRow)) (Pos (colToInt dstColumn, rowToInt dstRow))
            rowToInt :: Char -> Int
            rowToInt x = ord x - 97
            colToInt :: Char -> Int
            colToInt = digitToInt
            doParseWLCastle = do 
                                 try $ string "wlc"
                                 return $ CastleWhiteLeft
            doParseWRCastle = do 
                                 try $ string "wrc"
                                 return $ CastleWhiteRight
            doParseBLCastle = do 
                                 try $ string "blc"
                                 return $ CastleBlackLeft
            doParseBRCastle = do 
                                 try $ string "brc"
                                 return $ CastleBlackRight


