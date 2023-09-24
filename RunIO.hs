-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Main where

import System.Environment

import AbstractSyntax
import Parser
import Interpreter
import IOPrime

main :: IO()
main =
  do
    args <- getArgs
    if length args == 1
       then
         do
           sourceCode <- readFile (args !! 0)
           translate (run (parseProgram sourceCode) emptyStorage)
           return ()
       else
           putStrLn "Usage: runhaskell RunIO.hs <filename>"
