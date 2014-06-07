{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
 BoardTest.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module BoardTest where

import           Board
import           Data.Maybe (isNothing)
import           Test.QuickCheck     (Arbitrary (..))
import           Test.QuickCheck.Gen (choose, elements)

prop_evalBoard :: Bool
prop_evalBoard = evalBoard initialBoard == 0

prop_elementAt :: Bool
prop_elementAt = elementAt (Pos (0,0)) initialBoard == Just (Piece Rook Black)

prop_elementAt2 :: Bool
prop_elementAt2 = isNothing $ elementAt (Pos (4,4)) initialBoard

prop_elementAt3 :: Bool
prop_elementAt3 = elementAt (Pos (7,7)) initialBoard == Just (Piece Rook White)

instance Arbitrary PieceType where
    arbitrary = elements [Pawn, Rook, Queen, King, Bishop, Knight]

instance Arbitrary PieceColor where
    arbitrary = elements [Black, White]

instance Arbitrary Pos where
    arbitrary = do
      x <- choose (0,7)
      y <- choose (0,7)
      return $ Pos (x, y)

prop_valuePiece :: PieceType -> Bool
prop_valuePiece ptype = valuePiece (Piece ptype Black) == valuePiece (Piece ptype White)

prop_deleteSquare :: Pos -> Bool
prop_deleteSquare pos = isNothing $ elementAt pos updatedBoard
                       where
                             updatedBoard = deleteSquare pos initialBoard

prop_movePieceOnBoard :: Pos -> Pos -> Bool
prop_movePieceOnBoard origin destination = let originalBoard = initialBoard
                                               movedBoard = movePieceOnBoard originalBoard origin destination 
                                   in elementAt destination movedBoard == elementAt origin originalBoard && (isNothing (elementAt origin movedBoard) || origin == destination)
