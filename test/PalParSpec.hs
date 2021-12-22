{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning
 File Name: PalParSpec.hs

-}

import Test.HUnit

import PalParSequential(palParSequential)
import PalParParallel(palParParallel)

readFileHelper' :: String -> String -> String -> IO (Int)
readFileHelper' filename mode version
  | mode == "s" = do
    word <- readFile filename
    let result = palParSequential word version
    return result
  | otherwise = do
    word <- readFile filename
    let result = palParParallel word version (length word)
    return result

testSequentialNaive1 :: Test
testSequentialNaive1 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/alphabet1.txt" "s" "1")
  assertEqual "testSequentialNaive1" actual expected

testSequentialNaive2 :: Test
testSequentialNaive2 = TestCase $ do
  let expected = 1
  actual <- (readFileHelper' "./test/ValidCases/alphabet2.txt" "s" "1")
  assertEqual "testSequentialNaive2" actual expected

testSequentialNaive3 :: Test
testSequentialNaive3 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "s" "1")
  assertEqual "testSequentialNaive3" actual expected

testSequentialNaive4 :: Test
testSequentialNaive4 = TestCase $ do
  let expected = 3
  actual <- (readFileHelper' "./test/ValidCases/medium1.txt" "s" "1")
  assertEqual "testSequentialNaive4" actual expected

testSequentialNaive5 :: Test
testSequentialNaive5 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/medium2.txt" "s" "1")
  assertEqual "testSequentialNaive5" actual expected

testSequentialNaive6 :: Test
testSequentialNaive6 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "s" "1")
  assertEqual "testSequentialNaive6" actual expected

testSequentialNaive7 :: Test
testSequentialNaive7 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/palindrome1.txt" "s" "1")
  assertEqual "testSequentialNaive7" actual expected

testList :: Test
testList = TestList [TestLabel "testSequentialNaive1" testSequentialNaive1,
                     TestLabel "testSequentialNaive2" testSequentialNaive2,
                     TestLabel "testSequentialNaive3" testSequentialNaive3,
                     TestLabel "testSequentialNaive4" testSequentialNaive4,
                     TestLabel "testSequentialNaive5" testSequentialNaive5,
                     TestLabel "testSequentialNaive6" testSequentialNaive6,
                     TestLabel "testSequentialNaive7" testSequentialNaive7]

main :: IO ()
main = do
  {- Doing this throw-away bind because of the following warning:
       " A do-notation statement discarded a result of type ‘Counts’
         Suppress this warning by saying ‘_ <- runTestTT testList’ "   -}
  _ <- runTestTT testList
  return ()
