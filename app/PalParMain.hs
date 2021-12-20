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
            [filename, mode] -> do
              content <- readFile filename
              palParWrapper' content mode
            _ -> do
              pn <- getProgName
              die $ "Usage: " ++ pn ++ " <filename> <mode>"
  where
    palParWrapper' word mode
      | any (\w -> not (isLower w)) word = do
        die $ "Input word must consist of all lowercase alphabetical characters"
      | mode == "s" = do
        let result = palParSequential word 1
        putStrLn (show result)
      | mode == "p" = do
        let result = palParParallel word 0 ((length word) - 1)
        putStrLn (show result)
      | otherwise = do
        die $ "Mode must either be sequential 's' or parallel 'p'"
