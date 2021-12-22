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

testSequentialNaiveList :: Test
testSequentialNaiveList = TestList [TestLabel "testSequentialNaive1" testSequentialNaive1,
                                    TestLabel "testSequentialNaive2" testSequentialNaive2,
                                    TestLabel "testSequentialNaive3" testSequentialNaive3,
                                    TestLabel "testSequentialNaive4" testSequentialNaive4,
                                    TestLabel "testSequentialNaive5" testSequentialNaive5,
                                    TestLabel "testSequentialNaive6" testSequentialNaive6,
                                    TestLabel "testSequentialNaive7" testSequentialNaive7]

testSequentialDP1 :: Test
testSequentialDP1 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/alphabet1.txt" "s" "2")
  assertEqual "testSequentialDP1" actual expected

testSequentialDP2 :: Test
testSequentialDP2 = TestCase $ do
  let expected = 1
  actual <- (readFileHelper' "./test/ValidCases/alphabet2.txt" "s" "2")
  assertEqual "testSequentialDP2" actual expected

testSequentialDP3 :: Test
testSequentialDP3 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "s" "2")
  assertEqual "testSequentialDP3" actual expected

testSequentialDP4 :: Test
testSequentialDP4 = TestCase $ do
  let expected = 3
  actual <- (readFileHelper' "./test/ValidCases/medium1.txt" "s" "2")
  assertEqual "testSequentialDP4" actual expected

testSequentialDP5 :: Test
testSequentialDP5 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/medium2.txt" "s" "2")
  assertEqual "testSequentialDP5" actual expected

testSequentialDP6 :: Test
testSequentialDP6 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "s" "2")
  assertEqual "testSequentialDP6" actual expected

testSequentialDP7 :: Test
testSequentialDP7 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/palindrome1.txt" "s" "2")
  assertEqual "testSequentialDP7" actual expected

testSequentialDPList :: Test
testSequentialDPList = TestList [TestLabel "testSequentialDP1" testSequentialDP1,
                                 TestLabel "testSequentialDP2" testSequentialDP2,
                                 TestLabel "testSequentialDP3" testSequentialDP3,
                                 TestLabel "testSequentialDP4" testSequentialDP4,
                                 TestLabel "testSequentialDP5" testSequentialDP5,
                                 TestLabel "testSequentialDP6" testSequentialDP6,
                                 TestLabel "testSequentialDP7" testSequentialDP7]

main :: IO ()
main = do
  {- Doing these throw-away binds because of the following warning:
       " A do-notation statement discarded a result of type ‘Counts’
         Suppress this warning by saying ‘_ <- runTestTT testList’ "   -}
  putStrLn ""
  putStrLn "Running testSequentialNaiveList"
  _ <- runTestTT testSequentialNaiveList
  putStrLn "Running testSequentialDPList"
  _ <- runTestTT testSequentialDPList
  putStrLn ""
  return ()
