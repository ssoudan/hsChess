{-
 MainTestSuite.hs

 Copyright (c) 2014 by Sebastien Soudan.  
 Apache License Version 2.0, January 2004
-}
module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import BoardTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
  [
    testGroup "Board" 
    [
         testProperty "evalBoard" prop_evalBoard
       , testProperty "elementAt" prop_elementAt 
       , testProperty "elementAt" prop_elementAt2 
       , testProperty "elementAt" prop_elementAt3
       , testProperty "valuePiece" prop_valuePiece
       , testProperty "deleteSquare" prop_deleteSquare
       , testProperty "movePieceOnBoard" prop_movePieceOnBoard
    ]
    --,    
    --testGroup "Move"
    --[
    --   testProperty "Two string literals" testStringLiteral
    --]    
  ]
