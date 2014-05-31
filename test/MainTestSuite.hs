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
    ]
    --,    
    --testGroup "Move"
    --[
    --   testProperty "Two string literals" testStringLiteral
    --]    
  ]
