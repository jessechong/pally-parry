{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning
 File Name: PalParSpec.hs

-}

import Test.HUnit

import PalParSequential(palParSequential)
import PalParParallel(palParParallel)

-- Helper function to read a one word file for testing purposes
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
  assertEqual "testSequentialNaive1" expected actual

testSequentialNaive2 :: Test
testSequentialNaive2 = TestCase $ do
  let expected = 3
  actual <- (readFileHelper' "./test/ValidCases/medium1.txt" "s" "1")
  assertEqual "testSequentialNaive2" expected actual

testSequentialNaive3 :: Test
testSequentialNaive3 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/palindrome1.txt" "s" "1")
  assertEqual "testSequentialNaive3" expected actual

testSequentialNaive4 :: Test
testSequentialNaive4 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "s" "1")
  assertEqual "testSequentialNaive4" expected actual

testSequentialNaive5 :: Test
testSequentialNaive5 = TestCase $ do
  let expected = 15
  actual <- (readFileHelper' "./test/ValidCases/alphabet16.txt" "s" "1")
  assertEqual "testSequentialNaive5" expected actual

testSequentialDP1 :: Test
testSequentialDP1 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/alphabet1.txt" "s" "2")
  assertEqual "testSequentialDP1" expected actual

testSequentialDP2 :: Test
testSequentialDP2 = TestCase $ do
  let expected = 3
  actual <- (readFileHelper' "./test/ValidCases/medium1.txt" "s" "2")
  assertEqual "testSequentialDP2" expected actual

testSequentialDP3 :: Test
testSequentialDP3 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/palindrome1.txt" "s" "2")
  assertEqual "testSequentialDP3" expected actual

testSequentialDP4 :: Test
testSequentialDP4 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "s" "2")
  assertEqual "testSequentialDP4" expected actual

testSequentialDP5 :: Test
testSequentialDP5 = TestCase $ do
  let expected = 15
  actual <- (readFileHelper' "./test/ValidCases/alphabet16.txt" "s" "2")
  assertEqual "testSequentialDP5" expected actual

testSequentialList :: Test
testSequentialList = TestList [TestLabel "testSequentialNaive1" testSequentialNaive1,
                               TestLabel "testSequentialNaive2" testSequentialNaive2,
                               TestLabel "testSequentialNaive3" testSequentialNaive3,
                               TestLabel "testSequentialNaive4" testSequentialNaive4,
                               TestLabel "testSequentialNaive5" testSequentialNaive5,
                               TestLabel "testSequentialDP1"    testSequentialDP1,
                               TestLabel "testSequentialDP2"    testSequentialDP2,
                               TestLabel "testSequentialDP3"    testSequentialDP3,
                               TestLabel "testSequentialDP4"    testSequentialDP4,
                               TestLabel "testSequentialDP5"    testSequentialDP5]

testParallelVOne1 :: Test
testParallelVOne1 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/alphabet1.txt" "p" "1")
  assertEqual "testParallelVOne1" expected actual

testParallelVOne2 :: Test
testParallelVOne2 = TestCase $ do
  let expected = 3
  actual <- (readFileHelper' "./test/ValidCases/medium1.txt" "p" "1")
  assertEqual "testParallelVOne2" expected actual

testParallelVOne3 :: Test
testParallelVOne3 = TestCase $ do
  let expected = 0
  actual <- (readFileHelper' "./test/ValidCases/palindrome1.txt" "p" "1")
  assertEqual "testParallelVOne3" expected actual

testParallelVOne4 :: Test
testParallelVOne4 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "1")
  assertEqual "testParallelVOne4" expected actual

testParallelVOne5 :: Test
testParallelVOne5 = TestCase $ do
  let expected = 15
  actual <- (readFileHelper' "./test/ValidCases/alphabet16.txt" "p" "1")
  assertEqual "testParallelVOne5" expected actual

testParallelVTwo1 :: Test
testParallelVTwo1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "2")
  assertEqual "testParallelVTwo1" expected actual

testParallelVTwo2 :: Test
testParallelVTwo2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "2")
  assertEqual "testParallelVTwo2" expected actual

testParallelVThree1 :: Test
testParallelVThree1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "3")
  assertEqual "testParallelVThree1" expected actual

testParallelVThree2 :: Test
testParallelVThree2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "3")
  assertEqual "testParallelVThree2" expected actual

testParallelVFour1 :: Test
testParallelVFour1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "4")
  assertEqual "testParallelVFour1" expected actual

testParallelVFour2 :: Test
testParallelVFour2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "4")
  assertEqual "testParallelVFour2" expected actual

testParallelVFive1 :: Test
testParallelVFive1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "5")
  assertEqual "testParallelVFive1" expected actual

testParallelVFive2 :: Test
testParallelVFive2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "5")
  assertEqual "testParallelVFive2" expected actual

testParallelVSix1 :: Test
testParallelVSix1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "6")
  assertEqual "testParallelVSix1" expected actual

testParallelVSix2 :: Test
testParallelVSix2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "6")
  assertEqual "testParallelVSix2" expected actual

testParallelVSeven1 :: Test
testParallelVSeven1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "7")
  assertEqual "testParallelVSeven1" expected actual

testParallelVSeven2 :: Test
testParallelVSeven2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "7")
  assertEqual "testParallelVSeven2" expected actual

testParallelVEight1 :: Test
testParallelVEight1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "8")
  assertEqual "testParallelVEight1" expected actual

testParallelVEight2 :: Test
testParallelVEight2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "8")
  assertEqual "testParallelVEight2" expected actual

testParallelVNine1 :: Test
testParallelVNine1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "9")
  assertEqual "testParallelVNine1" expected actual

testParallelVNine2 :: Test
testParallelVNine2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "9")
  assertEqual "testParallelVNine2" expected actual

testParallelVTen1 :: Test
testParallelVTen1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "10")
  assertEqual "testParallelVTen1" expected actual

testParallelVTen2 :: Test
testParallelVTen2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "10")
  assertEqual "testParallelVTen2" expected actual

testParallelVEleven1 :: Test
testParallelVEleven1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "11")
  assertEqual "testParallelVEleven1" expected actual

testParallelVEleven2 :: Test
testParallelVEleven2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "11")
  assertEqual "testParallelVEleven2" expected actual

testParallelVTwelve1 :: Test
testParallelVTwelve1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "12")
  assertEqual "testParallelVTwelve1" expected actual

testParallelVTwelve2 :: Test
testParallelVTwelve2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "12")
  assertEqual "testParallelVTwelve2" expected actual

testParallelVThirteen1 :: Test
testParallelVThirteen1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "13")
  assertEqual "testParallelVThirteen1" expected actual

testParallelVThirteen2 :: Test
testParallelVThirteen2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "13")
  assertEqual "testParallelVThirteen2" expected actual

testParallelVFourteen1 :: Test
testParallelVFourteen1 = TestCase $ do
  let expected = 6
  actual <- (readFileHelper' "./test/ValidCases/medium3.txt" "p" "14")
  assertEqual "testParallelVFourteen1" expected actual

testParallelVFourteen2 :: Test
testParallelVFourteen2 = TestCase $ do
  let expected = 7
  actual <- (readFileHelper' "./test/ValidCases/alphabet8.txt" "p" "14")
  assertEqual "testParallelVFourteen2" expected actual

testParallelList :: Test
testParallelList = TestList [TestLabel "testParallelVOne1"      testParallelVOne1,
                             TestLabel "testParallelVOne2"      testParallelVOne2,
                             TestLabel "testParallelVOne3"      testParallelVOne3,
                             TestLabel "testParallelVOne4"      testParallelVOne4,
                             TestLabel "testParallelVOne5"      testParallelVOne5,
                             TestLabel "testParallelVTwo1"      testParallelVTwo1,
                             TestLabel "testParallelVTwo2"      testParallelVTwo2,
                             TestLabel "testParallelVThree1"    testParallelVThree1,
                             TestLabel "testParallelVThree2"    testParallelVThree2,
                             TestLabel "testParallelVFour1"     testParallelVFour1,
                             TestLabel "testParallelVFour2"     testParallelVFour2,
                             TestLabel "testParallelVFive1"     testParallelVFive1,
                             TestLabel "testParallelVFive2"     testParallelVFive2,
                             TestLabel "testParallelVSix1"      testParallelVSix1,
                             TestLabel "testParallelVSix2"      testParallelVSix2,
                             TestLabel "testParallelVSeven1"    testParallelVSeven1,
                             TestLabel "testParallelVSeven2"    testParallelVSeven2,
                             TestLabel "testParallelVEight1"    testParallelVEight1,
                             TestLabel "testParallelVEight2"    testParallelVEight2,
                             TestLabel "testParallelVNine1"     testParallelVNine1,
                             TestLabel "testParallelVNine2"     testParallelVNine2,
                             TestLabel "testParallelVTen1"      testParallelVTen1,
                             TestLabel "testParallelVTen2"      testParallelVTen2,
                             TestLabel "testParallelVEleven1"   testParallelVEleven1,
                             TestLabel "testParallelVEleven2"   testParallelVEleven2,
                             TestLabel "testParallelVTwelve1"   testParallelVTwelve1,
                             TestLabel "testParallelVTwelve2"   testParallelVTwelve2,
                             TestLabel "testParallelVThirteen1" testParallelVThirteen1,
                             TestLabel "testParallelVThirteen2" testParallelVThirteen2,
                             TestLabel "testParallelVFourteen1" testParallelVFourteen1,
                             TestLabel "testParallelVFourteen2" testParallelVFourteen2]

main :: IO ()
main = do
  {- Doing these throw-away binds because of the following warning:
       " A do-notation statement discarded a result of type ‘Counts’
         Suppress this warning by saying ‘_ <- runTestTT testList’ "   -}
  putStrLn ""
  putStrLn "Running testSequentialList"
  _ <- runTestTT testSequentialList
  -- All the parallel algorithms are identical hence why V2 to V14 have only 2 test cases compared to V1
  putStrLn "Running testParallelList"
  _ <- runTestTT testParallelList
  putStrLn ""
  return ()
