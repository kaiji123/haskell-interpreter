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

initialStorage :: Integer -> Storage
initialStorage x = update "x" x emptyStorage

main :: IO()
main =
  do
    args <- getArgs
    if length args == 2
       then
         do
           concreteProgram <- readFile (args !! 0)
           let abstractProgram = parseProgram concreteProgram
           let x = read(args !! 1)
           m' <- translate (run abstractProgram (initialStorage x))
           let y = m' "y"
           putStrLn (show y)
       else
           putStrLn "Usage: runhaskell Runxy.hs <filename> <Integer>"
