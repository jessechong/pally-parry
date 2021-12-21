{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParParallel.hs

-}

module PalParParallel
    ( palParParallel
    ) where

import Control.Parallel(par, pseq)
import Control.Parallel.Strategies(rdeepseq, parMap, parList, rpar, withStrategy, parListChunk, rseq, NFData, parBuffer)

-- Set ver to a different integer to change what algorithm gets used.
palParParallel :: String -> String -> Int -> Int
palParParallel word ver lenls
  | ver == "1"  = palParParallel1 word 0 ((length word) - 1)
  | ver == "2"  = palParParallel2 word 0 ((length word) - 1)
  | ver == "3"  = palParParallel3 word 0 ((length word) - 1) lenls
  | ver == "4"  = palParParallel4 word 0 ((length word) - 1) lenls
  | ver == "5"  = palParParallel5 word 0 ((length word) - 1) 2
  | ver == "6"  = palParParallel6 word 0 ((length word) - 1) 2
  | ver == "7"  = palParParallel7 word 0 ((length word) - 1)
  | ver == "8"  = palParParallel8 word 0 ((length word) - 1)
  | ver == "9"  = palParParallel9 word 0 ((length word) - 1)
  | ver == "10"  = palParParallel10 word 0 ((length word) - 1)
  | ver == "11"  = palParParallel11 word 0 ((length word) - 1) 4
  | ver == "12"  = palParParallel12 word 0 ((length word) - 1) 4
  | ver == "13"  = palParParallel13 word 0 ((length word) - 1) 4
  | otherwise = palParParallel1 word 0 ((length word) - 1)

isPalindrome word l r
  | l >= r                     = True
  | (word !! l) /= (word !! r) = False
  | otherwise                  = isPalindrome word (l + 1) (r - 1)

-- Version 1
-- One layer of parallelization
palParParallel1 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMap rpar (\m -> palParLoop1' word l m r) [l..(r - 1)])

palParLoop1' word l m r = 1 + (palParParallel1 word l m) + (palParParallel1 word (m + 1) r)

-- Version 2
-- Two layers of parallelization
palParParallel2 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMap rpar (\m -> palParLoop2' word l m r) [l..(r - 1)])

palParLoop2' word l m r = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel2 word l m
    parCall2 = palParParallel2 word (m + 1) r

-------- Chunk Helper
withChunk' c' s' f' = withStrategy (parListChunk c' s') . map f'

-- Version 3
-- One layer of parallelization and chunking based on bottleneck size (which is around 16 characters)
palParParallel3 word l r lenls
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | lenls > 16 = minimum (withChunk' 16 rseq (\m -> palParLoop1' word l m r) [l..(r - 1)])
  | otherwise  = minimum (parMap rpar (\m -> palParLoop1' word l m r) [l..(r - 1)])

-- Version 4
-- Two layers of parallelization and chunking based on bottleneck size (which is around 16 characters)
palParParallel4 word l r lenls
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | lenls > 16 = minimum (withChunk' 16 rseq (\m -> palParLoop2' word l m r) [l..(r - 1)])
  | otherwise  = minimum (parMap rpar (\m -> palParLoop2' word l m r) [l..(r - 1)])

-- Version 5
-- One layer of parallelization with depth
palParParallel5 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop5' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMap rpar (\m -> palParLoop5' word l m r (d - 1)) [l..(r - 1)])

palParLoop5' word l m r d = 1 + (palParParallel5 word l m d) + (palParParallel5 word (m + 1) r d)

-- Version 6
-- Two layers of parallelization with depth
palParParallel6 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop6' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMap rpar (\m -> palParLoop6' word l m r (d - 1)) [l..(r - 1)])

palParLoop6' word l m r d = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
    where
      parCall1 = palParParallel6 word l m d
      parCall2 = palParParallel6 word (m + 1) r d

parMapDeepSeq' :: (NFData y) => (x -> y) -> [x] -> [y]
parMapDeepSeq' f' = withStrategy (parList rdeepseq) . map f'

-- Version 7
-- One layer of parallelization using rdeepseq strategy
palParParallel7 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop7' word l m r) [l..(r - 1)])

palParLoop7' word l m r = 1 + (palParParallel7 word l m) + (palParParallel7 word (m + 1) r)

-- Version 8
-- Two layers of parallelization using rdeepseq strategy
palParParallel8 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop8' word l m r) [l..(r - 1)])

palParLoop8' word l m r = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel8 word l m
    parCall2 = palParParallel8 word (m + 1) r

------ Helper function for parBuffer rdeepseq
parBufferMapDeepSeq' :: (NFData y) => (x -> y) -> [x] -> [y]
parBufferMapDeepSeq' f' = withStrategy (parBuffer 500 rdeepseq) . map f'


-- Version 9
-- One layer of parallelization using rdeepseq strategy and buffering
palParParallel9 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop9' word l m r) [l..(r - 1)])

palParLoop9' word l m r = 1 + (palParParallel9 word l m) + (palParParallel9 word (m + 1) r)

-- Version 10
-- Two layers of parallelization using rdeepseq strategy and buffering
palParParallel10 word l r
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop10' word l m r) [l..(r - 1)])

palParLoop10' word l m r = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel10 word l m
    parCall2 = palParParallel10 word (m + 1) r


-- Version 11
-- One layer of parallelization using rdeepseq strategy and depth
palParParallel11 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop11' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop11' word l m r (d - 1)) [l..(r - 1)])

palParLoop11' word l m r d = 1 + (palParParallel11 word l m d) + (palParParallel11 word (m + 1) r d)

-- Version 12
-- Two layers of parallelization using rdeepseq strategy and depth
palParParallel12 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop12' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parMapDeepSeq' (\m -> palParLoop12' word l m r (d - 1)) [l..(r - 1)])

palParLoop12' word l m r d = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel12 word l m d
    parCall2 = palParParallel12 word (m + 1) r d

-- Version 13
-- One layer of parallelization using rdeepseq strategy, depth, and buffering
palParParallel13 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop13' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop13' word l m r (d - 1)) [l..(r - 1)])

palParLoop13' word l m r d = 1 + (palParParallel13 word l m d) + (palParParallel13 word (m + 1) r d)

-- Version 14
-- Two layers of parallelization using rdeepseq strategy, depth, and buffering
palParParallel14 word l r d
  | l >= r = 0
  | (isPalindrome word l r) == True = 0
  | d <= 0    = minimum (map (\m -> palParLoop14' word l m r d) [l..(r - 1)])
  | otherwise = minimum (parBufferMapDeepSeq' (\m -> palParLoop14' word l m r (d - 1)) [l..(r - 1)])

palParLoop14' word l m r d = parCall1 `par` parCall2 `pseq` parCall1 + parCall2 + 1
  where
    parCall1 = palParParallel14 word l m d
    parCall2 = palParParallel14 word (m + 1) r d




