{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParParallel.hs

-}

module PalParParallel(palParParallel) where

import PalParCommon(isPalindrome)
import Control.Parallel(par, pseq)
import Control.Parallel.Strategies(NFData, Strategy, parBuffer, parList, parListChunk, parMap,
                                   rdeepseq, rpar, rseq, withStrategy)

-- Set version to a different integer to change what algorithm gets used.
palParParallel :: String -> String -> Int -> Int
palParParallel word version lenls
  | version == "1"  = palParParallel1  word 0 ((length word) - 1)
  | version == "2"  = palParParallel2  word 0 ((length word) - 1)
  | version == "3"  = palParParallel3  word 0 ((length word) - 1) lenls
  | version == "4"  = palParParallel4  word 0 ((length word) - 1) lenls
  | version == "5"  = palParParallel5  word 0 ((length word) - 1) 4 -- Change this last number to change depth. Default = 4
  | version == "6"  = palParParallel6  word 0 ((length word) - 1) 4 -- Change this last number to change depth. Default = 4
  | version == "7"  = palParParallel7  word 0 ((length word) - 1)
  | version == "8"  = palParParallel8  word 0 ((length word) - 1)
  | version == "9"  = palParParallel9  word 0 ((length word) - 1)
  | version == "10" = palParParallel10 word 0 ((length word) - 1)
  | version == "11" = palParParallel11 word 0 ((length word) - 1) 4 -- Change this last number to change depth. Default = 4
  | version == "12" = palParParallel12 word 0 ((length word) - 1) 4 -- Change this last number to change depth. Default = 4
  | version == "13" = palParParallel13 word 0 ((length word) - 1) 4 -- Change this last number to change depth. Default = 4
  | version == "14" = palParParallel14 word 0 ((length word) - 1) 4 -- Change this last number to change depth. Default = 4
  | otherwise       = palParParallel1  word 0 ((length word) - 1)

-- Helper function for parListChunk
withChunk' :: Int -> Strategy y -> (x -> y) -> [x] -> [y]
withChunk' c' s' f' = withStrategy (parListChunk c' s') . map f'

-- Helper function for parList rdeepseq
parMapDeepSeq' :: (NFData y) => (x -> y) -> [x] -> [y]
parMapDeepSeq' f' = withStrategy (parList rdeepseq) . map f'

-- Helper function for parBuffer rdeepseq
parBufferMapDeepSeq' :: (NFData y) => (x -> y) -> [x] -> [y]
parBufferMapDeepSeq' f' = withStrategy (parBuffer 500 rdeepseq) . map f'

-- Version 1
-- One layer of parallelization
palParParallel1 :: String -> Int -> Int -> Int
palParParallel1 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMap rpar (\m -> palParLoop1' word l m r) [l..(r - 1)])

palParLoop1' :: String -> Int -> Int -> Int -> Int
palParLoop1' word l m r = 1 + (palParParallel1 word l m) + (palParParallel1 word (m + 1) r)

-- Version 2
-- Two layers of parallelization
palParParallel2 :: String -> Int -> Int -> Int
palParParallel2 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMap rpar (\m -> palParLoop2' word l m r) [l..(r - 1)])

palParLoop2' :: String -> Int -> Int -> Int -> Int
palParLoop2' word l m r = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel2 word l m
    parCall2 = palParParallel2 word (m + 1) r

-- Version 3
-- One layer of parallelization and chunking based on bottleneck size (which is around 16 characters)
palParParallel3 :: String -> Int -> Int -> Int -> Int
palParParallel3 word l r lenls
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | lenls > 16 = minimum (withChunk' 16 rseq (\m -> palParLoop1' word l m r) [l..(r - 1)])
  | otherwise  = minimum (parMap rpar (\m -> palParLoop1' word l m r) [l..(r - 1)])

-- Version 4
-- Two layers of parallelization and chunking based on bottleneck size (which is around 16 characters)
palParParallel4 :: String -> Int -> Int -> Int -> Int
palParParallel4 word l r lenls
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | lenls > 16 = minimum (withChunk' 16 rseq (\m -> palParLoop2' word l m r) [l..(r - 1)])
  | otherwise  = minimum (parMap rpar (\m -> palParLoop2' word l m r) [l..(r - 1)])

-- Version 5
-- One layer of parallelization with depth
palParParallel5 :: String -> Int -> Int -> Int -> Int
palParParallel5 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop5' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMap rpar (\m -> palParLoop5' word l m r (d - 1)) [l..(r - 1)])

palParLoop5' :: String -> Int -> Int -> Int -> Int -> Int
palParLoop5' word l m r d = 1 + (palParParallel5 word l m d) + (palParParallel5 word (m + 1) r d)

-- Version 6
-- Two layers of parallelization with depth
palParParallel6 :: String -> Int -> Int -> Int -> Int
palParParallel6 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop6' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMap rpar (\m -> palParLoop6' word l m r (d - 1)) [l..(r - 1)])

palParLoop6' :: String -> Int -> Int -> Int -> Int -> Int
palParLoop6' word l m r d = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
    where
      parCall1 = palParParallel6 word l m d
      parCall2 = palParParallel6 word (m + 1) r d

-- Version 7
-- One layer of parallelization using rdeepseq strategy
palParParallel7 :: String -> Int -> Int -> Int
palParParallel7 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop7' word l m r) [l..(r - 1)])

palParLoop7' :: String -> Int -> Int -> Int -> Int
palParLoop7' word l m r = 1 + (palParParallel7 word l m) + (palParParallel7 word (m + 1) r)

-- Version 8
-- Two layers of parallelization using rdeepseq strategy
palParParallel8 :: String -> Int -> Int -> Int
palParParallel8 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop8' word l m r) [l..(r - 1)])

palParLoop8' :: String -> Int -> Int -> Int -> Int
palParLoop8' word l m r = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel8 word l m
    parCall2 = palParParallel8 word (m + 1) r

-- Version 9
-- One layer of parallelization using rdeepseq strategy and buffering
palParParallel9 :: String -> Int -> Int -> Int
palParParallel9 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop9' word l m r) [l..(r - 1)])

palParLoop9' :: String -> Int -> Int -> Int -> Int
palParLoop9' word l m r = 1 + (palParParallel9 word l m) + (palParParallel9 word (m + 1) r)

-- Version 10
-- Two layers of parallelization using rdeepseq strategy and buffering
palParParallel10 :: String -> Int -> Int -> Int
palParParallel10 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop10' word l m r) [l..(r - 1)])

palParLoop10' :: String -> Int -> Int -> Int -> Int
palParLoop10' word l m r = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel10 word l m
    parCall2 = palParParallel10 word (m + 1) r

-- Version 11
-- One layer of parallelization using rdeepseq strategy and depth
palParParallel11 :: String -> Int -> Int -> Int -> Int
palParParallel11 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop11' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop11' word l m r (d - 1)) [l..(r - 1)])

palParLoop11' :: String -> Int -> Int -> Int -> Int -> Int
palParLoop11' word l m r d = 1 + (palParParallel11 word l m d) + (palParParallel11 word (m + 1) r d)

-- Version 12
-- Two layers of parallelization using rdeepseq strategy and depth
palParParallel12 :: String -> Int -> Int -> Int -> Int
palParParallel12 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop12' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop12' word l m r (d - 1)) [l..(r - 1)])

palParLoop12' :: String -> Int -> Int -> Int -> Int -> Int
palParLoop12' word l m r d = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel12 word l m d
    parCall2 = palParParallel12 word (m + 1) r d

-- Version 13
-- One layer of parallelization using rdeepseq strategy, depth, and buffering
palParParallel13 :: String -> Int -> Int -> Int -> Int
palParParallel13 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop13' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop13' word l m r (d - 1)) [l..(r - 1)])

palParLoop13' :: String -> Int -> Int -> Int -> Int -> Int
palParLoop13' word l m r d = 1 + (palParParallel13 word l m d) + (palParParallel13 word (m + 1) r d)

-- Version 14
-- Two layers of parallelization using rdeepseq strategy, depth, and buffering
palParParallel14 :: String -> Int -> Int -> Int -> Int
palParParallel14 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop14' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop14' word l m r (d - 1)) [l..(r - 1)])

palParLoop14' :: String -> Int -> Int -> Int -> Int -> Int
palParLoop14' word l m r d = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel14 word l m d
    parCall2 = palParParallel14 word (m + 1) r d
