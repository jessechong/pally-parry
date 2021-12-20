{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParSequential.hs

-}

module PalParSequential
    ( palParSequential
    ) where

palParSequential word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (map (\m -> palParLoop' word l m r) [l..(r - 1)])

palParLoop' word l m r = 1 + (palParSequential word l m) + (palParSequential word (m + 1) r)

isPalindrome word l r
  | l >= r                     = True
  | (word !! l) /= (word !! r) = False
  | otherwise                  = isPalindrome word (l + 1) (r - 1)
