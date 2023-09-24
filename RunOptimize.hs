-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

import System.Environment

import Parser
import Optimization
import PrettyPrinting

main :: IO()
main =
  do
    args <- getArgs
    if length args == 1
       then
         do
           sourceCode <- readFile (args !! 0)
           let optProg = optimize (parseProgram sourceCode)
           putStr $ ppProgram optProg 0
           return ()
       else
           putStrLn "Usage: runhaskell RunOptimize.hs <filename>"
