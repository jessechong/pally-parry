{-

 Name: Jesse Chong
 Uni: jlc2332
 Final Project: Palindrome Partitioning (PalPar)
 File Name: PalParMain.hs

-}

module Main where

import PalParSequential(palParSequential)
import PalParParallel(palParParallel)

import Data.Char(isLower)
import System.Environment(getArgs, getProgName)
import System.Exit(die)

main :: IO ()
main = do args <- getArgs
          case args of
            [filename, mode, version] -> do
              content <- readFile filename
              let ls = lines content
              palParWrapper' ls mode version
            _ -> do
              pn <- getProgName
              die $ "Usage: " ++ pn ++ " <filename> <mode> <version>"
  where
    palParWrapper' ls mode version
      | any (== True) [ isInvalid' word | word <- ls] = do
        die $ "Input words must consist of all lowercase alphabetical characters"
      | (isValidVersion mode version) == False = do
        die $ "Version must be within the valid range (check the Sequential and Parallel .hs files)"
      | mode == "s" = do
        mapM_ (\word -> putStrLn (show $ palParSequential word version)) ls
      | mode == "p" = do
        mapM_ (\word -> putStrLn (show $ palParParallel word version (length ls))) ls
      | otherwise = do
        die $ "Mode must either be sequential 's' or parallel 'p'"

    isInvalid' word = any (\w -> not (isLower w)) word
    isValidVersion mode version
      | mode == "s" = any (== True) [ version == (show sv) | sv <- [1..2]]
      | otherwise   = any (== True) [ version == (show pv) | pv <- [1..15]]