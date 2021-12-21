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

{- This program must be given:
1. a path to a file of all lower case Strings
2. a mode, "s" or "p"
3. and an algorithm version (see src .hs files for what versions there are to use) -}
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

    {- palParWrapper' is the function that handles input validation
    before it passes the given args over to the actual processing functions
    such as palParSequential or palParParallel -}
    palParWrapper' :: [String] -> String -> String -> IO ()
    palParWrapper' ls mode version
      | any (== True) [ isInvalid' word | word <- ls] = do
        die $ "Input words must consist of all lowercase alphabetical characters"
      | (isValidVersion' mode version) == False = do
        die $ "Version must be within the valid range (check the Sequential and Parallel .hs files)"
      | mode == "s" = do
        mapM_ (\word -> putStrLn (show $ palParSequential word version)) ls
      | mode == "p" = do
        mapM_ (\word -> putStrLn (show $ palParParallel word version (length ls))) ls
      | otherwise = do
        die $ "Mode must either be sequential 's' or parallel 'p'"

    -- Helper function for checking if a word is all lower case
    isInvalid' :: String -> Bool
    isInvalid' word = any (\w -> not (isLower w)) word

    -- Helper function for checking if the version is valid
    isValidVersion' :: String -> String -> Bool
    isValidVersion' mode version
      | mode == "s" = elem (read version :: Int) [1..2 ]
      | otherwise   = elem (read version :: Int) [1..13]
