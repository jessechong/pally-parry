{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning
 File Name: PalParSpec.hs

-}

import Test.HUnit

testSum = TestCase $ assertEqual "Test 1" 15 (10 + 5)
testPred = TestCase $ assertBool "10 > 5" (10 > 5)

testlist = TestList [TestLabel "testSum" testSum,
                     TestLabel "testPred" testPred
                    ]

main :: IO ()
main = do
  runTestTT testlist
  return ()
