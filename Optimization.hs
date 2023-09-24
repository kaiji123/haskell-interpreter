-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Optimization (optimize) where

import Control.Monad.State

import AbstractSyntax
import Interpreter



---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

type OptStorage = Identifier -> Maybe Integer

emptyOptStorage :: OptStorage
emptyOptStorage i = Nothing

deleteVar :: Identifier -> OptStorage -> OptStorage
deleteVar i m j | i == j    = Nothing
                | otherwise = m j

deleteVars :: [Identifier] -> OptStorage -> OptStorage
deleteVars [] m = m
deleteVars (i:is) m = deleteVars is (deleteVar i m)

-- A pair of stateful monadic computations optimizing expression and
-- programs in the presense of an OptStorage associating *constant*
-- variables to their values ....

--opEval :: OpName -> [Integer] -> Integer
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



evalConst:: Expr ->  Integer 
evalConst  (Constant x) = x
evalConst   (Op o es)  = opEval o [evalConst  e | e <- es]
evalConst  (Var i)      =undefined

takeConstant :: Expr -> Integer 
takeConstant (Constant i) = i
takeConstant _ = undefined
--optimize p = fst $ runState (optProgram p) emptyOptStorage
canOpt :: Expr -> OptStorage->[Bool] 


canOpt (Op s [Constant i , Constant y]) m =  [True]
canOpt (Op Add [Var s,Constant 0]) m = [True]
canOpt (Op Add [d,Constant 0]) m = [True]
canOpt (Op Add [Constant 0, d]) m = [True]
canOpt (Op Or [Constant 1, d]) m = [True]
canOpt (Op Or [d, Constant 1]) m = [True]
canOpt (Op And [d, Constant 0]) m = [True]
canOpt( Op And [Constant 0, d]) m= [True]
canOpt (Op Mul [Constant 0, d]) m= [True]
canOpt (Op Mul [ d, Constant 0]) m= [True]
--canOpt (Op And [Constant 1, d]) m= [True]
--canOpt (Op And [d, Constant 1]) m = [True]
canOpt (Var i) m = if (m i == Nothing) then [False] else [True]

canOpt (Op s xs) m = False:[ y | x<- xs, y<- canOpt x m]
canOpt _ m= [False]


canEx:: Expr -> OptStorage -> Bool 
canEx e m = True `elem` (canOpt e m)



elimConst ::Expr -> Expr 
elimConst (Op Add [Constant i, Constant y]) = Constant (opEval Add [i,y])
elimConst e= e

elimIdent :: Expr -> Expr
elimIdent (Op add [Var s,Constant 0])= Var s
elimIdent e= e



evalVar :: Maybe Integer->String -> Expr
evalVar (Just i) str = Constant i
evalVar Nothing  str =Var str

--opEval o [eval m e | e <- es]
optExs :: [Expr] -> State OptStorage [Expr]
optExs [] = pure []
optExs (e:exs) = do
    lol <- optExpr e
    trol <- optExs exs
    pure (lol:trol)
--(isTrivialIfElse opt s)
removeIf:: OptStorage ->Program -> Program
removeIf m (If expr p) = p
removeIf m s = s

{-x + 0 = 0 + x = x
          x * 1 = 1 * x = x
          0 * x = x * 0 = 0
          1 && x = x && 1 = x
          0 && x = x && 0 = 0
          1 || x = x || 1 = 1
-}

{-do
    m <- get
    if (m i == Nothing) then
        pure (Var i)
    else
        pure (Constant (takeOptValue (m i)))-}
optExpr :: Expr -> State OptStorage Expr
optExpr (Var i) = do
    m <- get
    if (m i == Nothing) then
        pure (Var i)
    else
        pure (Constant (takeOptValue (m i))) 

optExpr (Op Add [d,Constant 0])= pure (d)
optExpr (Op Add [Constant 0, d])= pure (d)
optExpr (Op s [Constant i , Constant y]) = pure (Constant (opEval s [i,y]))
optExpr (Op Or [Constant 1, d]) = pure (Constant 1)
optExpr (Op Or [d, Constant 1]) = pure (Constant 1)
optExpr (Op And [d, Constant 0]) = pure (Constant 0)
optExpr( Op And [Constant 0, d])= pure (Constant 0)
optExpr (Op Mul [Constant 0, d]) = pure (Constant 0)
optExpr (Op Mul [ d, Constant 0]) = pure (Constant 0)
--optExpr (Op And [Constant 1, d])= pure (d)
--optExpr (Op And [d, Constant 1]) = pure (d)
optExpr (Op s ((Var i ):es)) = do
    m <- get
    w<- optExs es
    let ooo  =evalVar (m i) i
    if canEx (Op s ((ooo): w)) m then 
        do
            optimal <- optExpr (Op s ((ooo): w))
            pure optimal
    else do
        pure (Op s ((ooo): w))
optExpr (Op s ((xx):xs))=  do
    xopt <- optExpr xx
    --let j= elimIdent(elimConst(xx))
    m <- get
    w <- optExs xs
    if canEx (Op s (xopt:w)) m then
        do
            optimal <- optExpr (Op s (xopt:w))
            pure optimal
    else 
        pure (Op s (xopt:w))
 
     


optExpr o = pure o

{-run (Block [])        m = pure m
run (Block (p : ps))  m = m''
    where
      m'  = run p m
      m'' = do
        s<- m'
        run (Block ps) s-}
takeOptValue:: Maybe Integer -> Integer 
takeOptValue (Just i) =i 
takeOptValue Nothing = 1

eval :: OptStorage -> Expr -> Integer
eval m (Constant x) = x
eval m (Var i)      =  takeOptValue (m i)
eval m (Op o es)    =  opEval o [eval m e | e <- es]


updates :: Identifier -> Integer -> OptStorage -> OptStorage
updates i x m = m'
 where
   m' :: OptStorage
   m' j | i == j    = Just x
        | otherwise = m j

programs :: [Program] -> State OptStorage [Program]
programs ( [])= do
    pure []
programs (x:xs) = do 
    --use programs to remove trivials 
    opt <- get
    s <- optProgram x
    w<- programs xs
    if (isTrivialIf opt s || isTrivialWhile opt s || trivialFor opt s ) then 
        pure (w)
    else 
        pure (removeIf opt (isTrivialIfElse opt s): w)
isTrivialWhile ::OptStorage ->Program -> Bool 

isTrivialWhile m (While (ex) pro)  = if boolean (eval m ex) then 
    False 
    else 
        True
isTrivialWhile m _  = False 

isTrivialIf :: OptStorage -> Program -> Bool 
isTrivialIf m (If (ex) pro)=if boolean (eval m ex) then 
    False 
    else 
        True
isTrivialIf m _  = False 

isTrivialIfElse ::OptStorage->Program -> Program 
isTrivialIfElse m (IfElse e p1 p2) = if boolean(eval m e) then 
    p1 else
        p2
isTrivialIfElse m s = s


trivialFor :: OptStorage -> Program -> Bool
trivialFor m (For i mn mx p)  = if (eval m mn < eval m mx) then 
    False 
    else 
        True
trivialFor m _  = False 

prints:: OptStorage-> IO ()
prints(s) = do

     putStrLn ("car is " ++ show(s ("y")))

     -- Change state here somehow
    --  colorChange "green"


{-canOpt :: Expr -> [Bool] 


canOpt (Op s [Constant i , Constant y])=  [True]
canOpt (Op Add [Var s,Constant 0]) = [True]


canOpt (Op Add [d,Constant 0])= [True]
canOpt (Op Add [Constant 0, d])= [True]
canOpt (Op Or [Constant 1, d]) = [True]
canOpt (Op Or [d, Constant 1]) = [True]
canOpt (Op And [d, Constant 0]) = [True]
canOpt( Op And [Constant 0, d])= [True]
canOpt (Op Mul [Constant 0, d]) = [True]
canOpt (Op Mul [ d, Constant 0]) = [True]
canOpt (Op And [Constant 1, d])= [True]
canOpt (Op And [d, Constant 1]) = [True]

canOpt (Op s xs) = False:[ y | x<- xs, y<- canOpt x]
canOpt _ = [False]


canEx:: Expr -> Bool 
canEx e = True `elem` (canOpt e)
-}
canPros:: Program -> OptStorage -> [Bool]
canPros (If expr program) m= [True]
canPros (IfElse expr p1 p2) m= [True]
canPros (While expr program) m= if (eval m expr == 0) then
    [True]
    else [False]
canPros (For id mn mx orogram) m= if(eval m mn< eval m mx) then [False] else[True]
canPros ( i := expr) m = if canEx(expr) m then [True] else [False]

canPros (Block []) m= [True]
canPros (Block [x]) m= [True]
canPros (Block xs) m=False : [y | x<-xs, y <- (canPros x m)]
canPros _ m = [False]


canProg :: Program -> OptStorage ->Bool
canProg pro storage  = True `elem` (canPros pro storage)

    
optProgram  :: Program -> State OptStorage Program

optProgram (Block(x:[])) = do 
    pure (x)
optProgram (Block (xs))= do
    
    --let s  =[ isTrivialIfElse opt x|x <- xs, not (x == Block []) && (not (isTrivialWhile opt x)) && not (isTrivialIf opt x)]
    
    let s  =[ x|x <- xs, not (x == Block []) ]
    w <- programs s
    --opt <- get
 
    --let jj = [ isTrivialIfElse opt x|x <- w, not (isTrivialIf opt x) && not (isTrivialWhile opt x) && not(trivialFor opt x)]
    if (length w== 1) then do
        opt <- get
        if (canProg (w!!0) opt) then do
            let pro1 = w!!0
            optimal <- optProgram pro1
            pure optimal
        else
            pure (w!!0)    
    else do
        let pro2 = Block w
        opt <- get
        if (canProg (Block w) opt) then do
            optimal <- optProgram pro2
            pure optimal
        else
            pure (Block w)
   
optProgram (Read id) = do
    opt <- get
    
        
    put (deleteVars [id] opt)
    
    pure (Read id)
optProgram (id := expr) =do
    s<- get
  
    c<- optExpr expr

   -- put (updates id (eval s expr) s)
    put (updates id (eval s c) s)

    
   
    pure (id := c)
optProgram (Write ex) = do
    c <- optExpr ex
    pure (Write c)



optProgram p = pure p


-- Replace this with any implementation you like
optimize :: Program -> Program
optimize p = fst $ runState (optProgram p) emptyOptStorage

-- The suggested implementation for using the above monadic setup:
-- optimize p = fst $ runState (optProgram p) emptyOptStorage
