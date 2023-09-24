-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Interpreter (run, Storage, emptyStorage, update) where

import AbstractSyntax
import IOPrime

type Storage = Identifier -> Integer

emptyStorage :: Storage
emptyStorage i = error ("Uninitialized identifier " ++ i)

update :: Identifier -> Integer -> Storage -> Storage
update i x m = m'
 where
   m' :: Storage
   m' j | i == j    = x
        | otherwise = m j

number :: Bool -> Integer
number False = 0
number True  = 1

boolean :: Integer -> Bool
boolean 0 = False
boolean _ = True

opEval :: OpName -> [Integer] -> Integer
opEval Add     [x, y] = x + y
opEval Sub     [x, y] = x - y
opEval Mul     [x, y] = x * y
opEval Div     [x, y] = x `div` y
opEval Mod     [x, y] = x `mod` y
opEval Eq      [x, y] = number(x == y)
opEval Leq     [x, y] = number(x <= y)
opEval Less    [x, y] = number(x <  y)
opEval Geq     [x, y] = number(x >= y)
opEval Greater [x, y] = number(x >  y)
opEval And     [x, y] = number(boolean x && boolean y)
opEval Or      [x, y] = number(boolean x || boolean y)
opEval Not     [x]    = number(not(boolean x))
opEval op      xs     = error ("Interpreter bug. "
                            ++ "Please contact the software maintainer. "
                            ++ "Tried to apply " ++ show op
                            ++ " to " ++ show xs)

eval :: Storage -> Expr ->  Integer
eval m (Constant x) = x
eval m (Var i)      = m i
eval m (Op o es)    = opEval o [eval m e | e <- es]

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

run :: Program -> Storage -> IO' Storage

run (i := e)          m = pure (update i (eval m e) m)
run (IfElse e p q)    m   
    | boolean(eval m e) = do
      s <- run p m
      pure s
    | otherwise         = do
      s<- run q m
      pure s

run (If e p) m
    | boolean(eval m e) = do
      s<- run p m
      pure s 
    | otherwise         = pure m

run (While e p)       m
    | boolean(eval m e) = m''
    | otherwise         = pure m
    where
      m'  = do
        s<- run p m
        pure s
      m'' = do
        s <- m'
        h <- run (While e p) s
        pure h

run (Block [])        m = pure m
run (Block (p : ps))  m = m''
    where
      m'  = run p m
      m'' = do
        s<- m'
        run (Block ps) s


run (Read i)          m = do
  e <- getLine'
  let j =read e::Integer

  let s = e
  
  run (i := Constant j) m 
run (Write e)         m = do
  let s =eval m e
  putStrLn' (show s)
  pure m
run (Print s)         m = do
  putStrLn' (show s)
  pure m



run (For i mn mx p)   m 
  | boolean (number ((eval m mn)> (eval m mx)) ) = pure m
  |otherwise = do
    m1 <- run (i := mn) m
    m2<- run p m1
    s<- run (For i (Op Add ([Constant 1]++[mn])) mx p) m2
    pure s

