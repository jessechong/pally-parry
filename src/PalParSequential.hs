{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParSequential.hs

-}

module PalParSequential(palParSequential) where

import PalParCommon(isPalindrome)
import Data.Matrix(Matrix, getElem, identity, setElem)

{- Set "version" to a different integer to choose what sequential algorithm gets used.
Version 1 is an O(n^3) naive solution.
Version 2 is an O(n^2) dynamic programming solution.

The below Haskell solutions were written after referencing the Python and C++ implementations on GeeksForGeeks:
  - https://www.geeksforgeeks.org/palindrome-partitioning-dp-17/
-}
palParSequential :: String -> String -> Int
palParSequential word version
  | version == "1"  = palParSequentialNaive word 0 ((length word) - 1)
  | version == "2"  = palParSequentialDP word (length word)
  | otherwise       = palParSequentialNaive word 0 ((length word) - 1)

{- palParSequentialNaive, the O(n^3) solution.
Past about 16 characters, this algorithm experiences immense slow down. -}
palParSequentialNaive :: String -> Int -> Int -> Int
palParSequentialNaive word l r
  | l >= r                          = 0
  | (isPalindrome word l r) == True = 0
  | otherwise                       = minimum (map (\m -> palParInnerLoop' word l m r) [l..(r - 1)])
  where
    -- Helper function to help with the recursion
    palParInnerLoop' :: String -> Int -> Int -> Int -> Int
    palParInnerLoop' word' l' m' r' = 1 + (palParSequentialNaive word' l' m') + (palParSequentialNaive word' (m' + 1) r')

-- palParSequentialDP, the O(n^2) solution.
palParSequentialDP :: String -> Int -> Int
palParSequentialDP word len = do
  {- c[i]    = A list holding minimum number of cuts needed for each substring for the input string from 0 to i
     p[i][j] = A matrix storing whether or not the substring from i to j is a palindrome.
               If 1, then substring is a palindrome. Else, then not. -}
  let c  = replicate (len) 0 -- Initiate list of cuts
      p  = identity (len)    -- Initiate 2x2 identity matrix of substring lengths
      p0 = foldl (palSubStringLoop' word len) p [2..(len)]
      c0 = foldl (palCutLoop' p0) c [0..(len - 1)]
  c0 !! (len - 1) -- Returns the minimum cut value for the string

  where
    {- Builds the matrix of substring palindrome conditions.
       p'   = palindrome matrix
       len' = length of the input string
       l'   = length of the substring     -}
    palSubStringLoop' :: String -> Int -> Matrix Int -> Int -> Matrix Int
    palSubStringLoop' word' len' p' l' = foldl (palSubStringInner' word' l') p' [0..(len' - l')]

    -- Checks whether the substring is a palindrome or not.
    palSubStringInner' :: String -> Int -> Matrix Int -> Int -> Matrix Int
    palSubStringInner' word' l' p' i' = do
      let j' = i' + l' - 1 -- Ending index for the substring
      if l' == 2 then do -- Length of the substring was 2, so just compare two characters
        setElem' (fromEnum $ ((word' !! i') == (word' !! j'))) (i', j') p'
      else do
        setElem' (fromEnum $ ((word' !! i') == (word' !! j') && ((getElem' (i' + 1) (j' - 1) p') == 1))) (i', j') p'

    {- Builds the list of minimum cuts needed for the substrings.
       p0' = palindrome matrix
       c'  = palindrome cut list
       l'  = length of the substring -}
    palCutLoop' :: Matrix Int -> [Int] -> Int -> [Int]
    palCutLoop' p0' c' l'
      | getElem' 0 l' p0' == 1 = update' l' 0 c' -- The substring was a palindrome. No cuts needed; set to 0
      | otherwise            = foldl (palCutInner' p0' l') (update' l' (maxBound :: Int) c') [0..(l' - 1)]

    -- Counts the number of cuts for each substring
    palCutInner' :: Matrix Int -> Int -> [Int] -> Int -> [Int]
    palCutInner' p0' l' c' j' = do
      if ((getElem' (j' + 1) l' p0') == 1) && ((1 + (c' !! j')) < (c' !! l')) then do
        update' l' (1 + (c' !! j')) c'
      else do
        c'

    {- Modification on the Matrix getElem function to use zero based indexing.
       https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:getElem -}
    getElem' :: Int -> Int -> Matrix a -> a
    getElem' x y m = getElem (x + 1) (y + 1) m

    {- Modification on the Matrix setElem function to use zero based indexing.
       https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:setElem -}
    setElem' :: a -> (Int, Int) -> Matrix a -> Matrix a
    setElem' a (x, y) m = setElem a (x + 1, y + 1) m

    -- Helper function that updates the given list with the given element at the given index
    update' :: Int -> a -> [a] -> [a]
    update' i v ls = (take i ls) ++ [v] ++ (tail $ drop i ls)
