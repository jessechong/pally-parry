# PalPar-jlc2332  

Name: Jesse Chong  
Uni: jlc2332  
Final Project: Palindrome Partitioning (PalPar)  

Hello, this is the readme file for my final project "Palindrome Partitioning" or in short "PalPar". This project was for Professor Edwards' COMS W4995 Parallel Functional Programming.  

Commands to run:  
`stack build`  
`stack test`  
`stack exec PalPar-jlc2332-exe ./test/ValidCases/small2.txt p`  
`stack exec PalPar-jlc2332-exe ./test/ValidCases/small2.txt p -- +RTS -ls -N2`  
`threadscope PalPar-jlc2332-exe.eventlog`  
`time stack exec PalPar-jlc2332-exe ./test/ValidCases/big3.txt p`  
`time stack exec PalPar-jlc2332-exe ./test/ValidCases/big3.txt p -- +RTS -ls -N2`  

Special thanks to Professor Edwards for the class and to Max Helman for TA'ing me over the project. I had a lot of fun learning about Haskell and working on the homeworks!  

References:  
  - Course lectures:
    - http://www.cs.columbia.edu/~sedwards/classes/2021/4995-fall/io.pdf
    - http://www.cs.columbia.edu/~sedwards/classes/2021/4995-fall/laziness.pdf
  - Class discussion board:
    - https://edstem.org/us/courses/13664/discussion/948167
  - Leetcode problem this project was based off of:
    - https://leetcode.com/problems/palindrome-partitioning-ii/
  - Referred to GeeksForGeeks Python and C++ implementations to write the algorithm used in this project. Verified that my code was working by comparing results of my program to the Python solution given in the GeeksForGeeks page.
    - https://www.geeksforgeeks.org/palindrome-partitioning-dp-17/
  - Referred to Haskell docs to create this project directory.
    - https://docs.haskellstack.org/en/stable/README/#start-your-new-project
  - General ThreadScope installation and usage.
    - https://wiki.haskell.org/ThreadScope
  - Parallel library documentation.
    - https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html
  - HUnit guide.
    - https://caiorss.github.io/Functional-Programming/haskell/UnitTest_Hunit.html#sec-1-3
  - Cabal FAQ for debugging dependency issues.
    - https://www.haskell.org/cabal/FAQ.html
  - StackOverflow discussion on threading.
    - https://stackoverflow.com/questions/62641707/what-is-ghc-doing-when-run-with-n-parallel-flag
  - StackOverflow discussion on chunking.
    - https://stackoverflow.com/questions/38175725/how-do-i-combine-the-benefits-of-parbuffer-and-parlistchunk
  - StackOverflow post to resolve trouble using rdeepseq due to NFData typing error.
    - https://stackoverflow.com/questions/55662840/please-help-understanding-haskell-parallel
  - Global variables in Haskell article.
    - https://wiki.haskell.org/Global_variables
  - Haskell docs on various RTS options.
    - https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/runtime-control.html