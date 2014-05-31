module BoardTest where

import           Board
import           Test.QuickCheck     (Arbitrary (..))
import           Test.QuickCheck.Gen (choose, elements)

testEvalBoard = evalBoard initialBoard == 0

testElementAt = elementAt (Pos (0,0)) initialBoard == Just (Piece Rook Black)
testElementAt2 = elementAt (Pos (4,4)) initialBoard == Nothing
testElementAt3 = elementAt (Pos (7,7)) initialBoard == Just (Piece Rook White)

instance Arbitrary PieceType where
    arbitrary = elements [Pawn, Rook, Queen, King, Bishop, Knight]

instance Arbitrary PieceColor where
    arbitrary = elements [Black, White]

instance Arbitrary Pos where
    arbitrary = do
      x <- choose (0,7)
      y <- choose (0,7)
      return $ Pos (x, y)

testValuePiece pieceType = valuePiece (Piece pieceType Black) == valuePiece (Piece pieceType White)

testDeleteSquare :: Pos -> Bool
testDeleteSquare pos = elementAt pos updatedBoard == Nothing
                       where
                             updatedBoard = deleteSquare pos initialBoard
