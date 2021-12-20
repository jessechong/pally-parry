{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParParallel.hs

-}

module PalParParallel
    ( palParParallel
    ) where

import Control.Parallel
import Control.Parallel.Strategies

palParParallel word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMap rseq (\m -> palParLoop' word l m r) [l..(r - 1)])


-- palParLoop' word l m r = 1 + (palParParallel word l m) + (palParParallel word (m + 1) r)

palParLoop' word l m r = parCall1 `pseq` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel word l m
    parCall2 = palParParallel word (m + 1) r

isPalindrome word l r
  | l >= r                     = True
  | (word !! l) /= (word !! r) = False
  | otherwise                  = isPalindrome word (l + 1) (r - 1)
