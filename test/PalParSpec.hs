{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning
 File Name: PalParSpec.hs

-}

import Test.HUnit

import PalParSequential(palParSequentialNaive)
import PalParParallel(palParParallel)


readFileHelper' filename func = do
  content <- readFile filename
  let ls = lines content
  return [16]

testSequentialNaive1 = TestCase $ do
  let actual   = readFileHelper' "./ValidCases/big1.txt" palParSequentialNaive
      expected = [16]
  assertEqual "Sequential Naive 1" actual expected

testlist = TestList [TestLabel "testSequentialNaive1" testSequentialNaive1]

main :: IO ()
main = do
  runTestTT testlist
  return ()
