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
         testProperty "evalBoard" testEvalBoard
       , testProperty "elementAt" testElementAt 
       , testProperty "elementAt" testElementAt2 
       , testProperty "elementAt" testElementAt3
       , testProperty "valuePiece" testValuePiece
       , testProperty "deleteSquare" testDeleteSquare
    ]
    --,    
    --testGroup "Move"
    --[
    --   testProperty "Two string literals" testStringLiteral
    --]    
  ]
