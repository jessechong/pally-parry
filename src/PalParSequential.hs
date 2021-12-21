{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParSequential.hs

-}

module PalParSequential(palParSequential) where

import PalParCommon(isPalindrome)

import Data.Matrix(identity, getElem, setElem)

{- Set "version" to a different integer to choose what sequential algorithm gets used.
Version 1 is a O(n^3) naive solution.
Version 2 is a O(n^2) dynamic programming solution.

The below Haskell solutions were written after referencing the Python and C++ implementations on GeeksForGeeks:
  - https://www.geeksforgeeks.org/palindrome-partitioning-dp-17/
-}
palParSequential :: String -> String -> Int
palParSequential word version
  | version == "1"  = palParSequentialNaive word 0 ((length word) - 1)
  | version == "2"  = palParSequentialDP word (length word)
  | otherwise = palParSequentialNaive word 0 ((length word) - 1)

{- palParSequentialNaive, the O(n^3) solution.
Past about 16 characters, this algorithm experiences immense slow down. -}
palParSequentialNaive :: String -> Int -> Int -> Int
palParSequentialNaive word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (map (\m -> palParInnerLoop' word l m r) [l..(r - 1)])
  where
    -- Helper function to help with the recursion
    palParInnerLoop' :: String -> Int -> Int -> Int -> Int
    palParInnerLoop' word l m r = 1 + (palParSequentialNaive word l m) + (palParSequentialNaive word (m + 1) r)

-- palParSequentialDP, the O(n^2) solution.
palParSequentialDP :: String -> Int -> Int
palParSequentialDP word len = do
  let c = replicate (len) 0 -- Initiate list of cuts
      p = identity (len)    -- Initiate 2x2 identity matrix of substring lengths
      p1 = foldl (outerLoop2 word len) p [2..(len)]
      c1 = foldl (ceeOuterLoop2 p1) c [0..(len - 1)]
  c1 !! (len - 1)
  where
    outerLoop2 word len p l = foldl (innerLoop2 word l) p [0..(len - l)]
    innerLoop2 word l p i = do
      let j = i + l - 1
      if l == 2 then do
        setElem' (fromEnum $ ((word !! i) == (word !! j))) (i, j) p
      else do
        setElem' (fromEnum $ ((word !! i) == (word !! j) && ((getElem' (i + 1) (j - 1) p) == 1))) (i, j) p

    ceeOuterLoop2 p1 c i
      | getElem' 0 i p1 == 1 = update' i 0 c
      | otherwise = foldl (ceeInnerLoop2 p1 i) (update' i (maxBound :: Int) c) [0..(i - 1)]

    ceeInnerLoop2 p1 i c j = do
      if ((getElem' (j + 1) i p1) == 1) && ((1 + (c !! j)) < (c !! i)) then do
        update' i (1 + (c !! j)) c
      else do
        c

    -- Helper functions to make matrix functions getElem and setElem use zero based indexing
    getElem' x y m = getElem (x + 1) (y + 1) m
    setElem' a (p1, p2) m = setElem a (p1 + 1, p2 + 1) m

    -- Helper function that inserts an element into a list based on the given index
    update' i v l = (take i l) ++ [v] ++ (tail $ drop i l)


