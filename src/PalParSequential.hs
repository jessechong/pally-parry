{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParSequential.hs

-}

module PalParSequential
    ( palParSequential
    ) where

import Data.Matrix

-- Set ver to a different integer to change what algorithm gets used.
palParSequential word ver
  | ver == "1"  = palParSequential1 word 0 ((length word) - 1)
  | ver == "2"  = palParSequential2 word (length word)
  | otherwise = palParSequential1 word 0 ((length word) - 1)

-- palParSequential1 is based on an O(n^3) solution.
-- Past about 16 characters, this palParSequential1 algorithm starts to experience immense slow down.
palParSequential1 word l r
  | l >= r = 0
  | (isPalindrome1 word l r) == True = 0
  | otherwise = minimum (map (\m -> palParLoop1' word l m r) [l..(r - 1)])

palParLoop1' word l m r = 1 + (palParSequential1 word l m) + (palParSequential1 word (m + 1) r)

isPalindrome1 word l r
  | l >= r                     = True
  | (word !! l) /= (word !! r) = False
  | otherwise                  = isPalindrome1 word (l + 1) (r - 1)




-- palParSequential2 is based on an O(n^2) solution.
palParSequential2 word len = do
  let c = replicate (len) 0
      p = identity (len)
      p1 = foldl (outerLoop2 word len) p [2..(len)]
      c1 = foldl (ceeOuterLoop2 p1) c [0..(len - 1)]
  c1 !! (len - 1)

outerLoop2 word len p l = foldl (innerLoop2 word l) p [0..(len - l)]
innerLoop2 word l p i = do
  let j = i + l - 1
  if l == 2 then do
    setElem' (fromEnum $ ((word !! i) == (word !! j))) (i, j) p
  else do
    setElem' (fromEnum $ ((word !! i) == (word !! j) && ((getElem' (i + 1) (j - 1) p) == 1))) (i, j) p

ceeOuterLoop2 p1 c i
  | getElem' 0 i p1 == 1 = update i 0 c
  | otherwise = foldl (ceeInnerLoop2 p1 i) (update i (maxBound :: Int) c) [0..(i - 1)]

ceeInnerLoop2 p1 i c j = do
  if ((getElem' (j + 1) i p1) == 1) && ((1 + (c !! j)) < (c !! i)) then do
    update i (1 + (c !! j)) c
  else do
    c

-- Helper function to make matrix functions use zero based indexing
getElem' x y m = getElem (x + 1) (y + 1) m
setElem' a (p1, p2) m = setElem a (p1 + 1, p2 + 1) m

-- [1, 2, 3, 4]
-- update 1 10 [1,2,3,4] == [1, 10, 3, 4]
update i v l = (take i l) ++ [v] ++ (tail $ drop i l)


