-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module AbstractSyntax where

type Identifier = String

data OpName  = Or                                 --  ||
             | And                                --  &&
             | Eq                                 --  ==
             | Leq | Less | Geq | Greater         --  <=  <  >=  >
             | Add | Sub                          --  +  -
             | Mul | Div | Mod                    --  *  /  %
             | Not                                --  !
             deriving (Eq,Show)

data Expr    = Constant Integer
             | Var Identifier
             | Op OpName [Expr]
            deriving (Eq,Show)

data Program = Identifier := Expr
             | Block [Program]
             | While Expr Program
             | If Expr Program
             | IfElse Expr Program Program
             | Read Identifier
             | Write Expr
             | Print String
             | For Identifier Expr Expr Program
             deriving (Eq,Show)
