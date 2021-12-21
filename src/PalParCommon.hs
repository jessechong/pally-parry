{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParCommon.hs

-}

module PalParCommon(isPalindrome) where

-- Helper function to check if a given String is a palindrome.
-- Starts from the ends of the String until it reaches the middle.
isPalindrome :: String -> Int -> Int -> Bool
isPalindrome str l r
  | l >= r                   = True
  | (str !! l) /= (str !! r) = False
  | otherwise                = isPalindrome str (l + 1) (r - 1)
