-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module PrettyPrinting where

import AbstractSyntax

indentation = 2

spaces n = take n (repeat ' ')

ppProgram :: Program -> (Int -> String)
ppExpr :: Expr -> String

ppProgram (i := e)       n = spaces n ++ i ++ " := " ++ ppExpr e ++ ";\n"
ppProgram (If e p)       n = spaces n ++ "if (" ++ ppExpr e ++ ")\n" ++ ppProgram p (n+indentation)
ppProgram (IfElse e p q) n = spaces n ++ "if (" ++ ppExpr e ++ ")\n"
                                      ++ ppProgram p (n+indentation)
                         ++  spaces n ++ "else\n"
                                      ++ ppProgram q (n+indentation)
ppProgram (While e p)    n = spaces n ++ "while (" ++ ppExpr e ++ ")\n"
                                      ++ ppProgram p (n+indentation)
ppProgram (Block ps)     n = spaces n ++ "{\n" ++ concat [ppProgram p (n+indentation) | p <- ps]
                         ++  spaces n ++ "}\n"
ppProgram (Write e)      n = spaces n ++ "write " ++ ppExpr e ++ ";\n"
ppProgram (Read i)       n = spaces n ++ "read " ++ i ++ ";\n"
ppProgram (Print s)      n = spaces n ++ "print \"" ++ s ++ "\";\n"
ppProgram (For i s e p)  n = spaces n ++ "for (" ++ i ++ " <- "
                             ++ ppExpr s ++ " .. " ++ ppExpr e ++ ")\n"
                             ++ ppProgram p (n+indentation) 

--Operator Precedence
--Lower precedence binds tighter
-- 1: !
-- 2: * / %     (Left to right)
-- 3: + -       (Left to right)
-- 4: <= < >= > (Left to right)
-- 5: ==        (Left to right)
-- 6: &&        (Left to right)
-- 7: ||        (Left to right)

notPr = 1
mulPr = 2
addPr = 3
relPr = 4
eqPr  = 5
andPr = 6
orPr  = 7
conPr = 0

opPP :: OpName -> String -> String -> String
opPP Not  _  s' =      "! "   ++ s'
opPP Mul  s  s' = s ++ " * "  ++ s'
opPP Div  s  s' = s ++ " / "  ++ s'
opPP Mod  s  s' = s ++ " % "  ++ s'
opPP Add  s  s' = s ++ " + "  ++ s'
opPP Sub  s  s' = s ++ " - "  ++ s'
opPP Leq  s  s' = s ++ " <= " ++ s'
opPP Less s  s' = s ++ " < "  ++ s'
opPP Geq  s  s' = s ++ " >= "  ++ s'
opPP Greater s  s' = s ++ " > "  ++ s'
opPP Eq   s  s' = s ++ " == "  ++ s'
opPP And  s  s' = s ++ " && "  ++ s'
opPP Or   s  s' = s ++ " || "  ++ s'

prec :: OpName -> Int
prec Not     = notPr
prec Mul     = mulPr
prec Div     = mulPr
prec Mod     = mulPr
prec Add     = addPr
prec Sub     = addPr
prec Leq     = relPr
prec Less    = relPr
prec Geq     = relPr
prec Greater = relPr
prec Eq      = eqPr
prec And     = andPr
prec Or      = orPr

precedence  :: Expr -> Int
precedence (Op o _)     = prec o
precedence (Var _)      = conPr
precedence (Constant _) = conPr

isAssoc :: OpName -> Bool
isAssoc n = n `elem` [And, Or, Add, Mul]

name :: Expr -> Maybe OpName
name (Var _) = Nothing
name (Constant _) = Nothing
name (Op op _) = Just op

leftBind :: OpName -> Expr -> Bool
leftBind op expr = (prec op) < (precedence expr)
rightBind :: OpName -> Expr -> Bool
rightBind op expr = if (isAssoc op) && (name expr == Just op)
                       then False
                       else (prec op) <= (precedence expr)

wrap :: String -> String
wrap s = "(" ++ s ++ ")"

smartBracket :: (String -> String -> String) -> Bool -> Bool -> Expr -> Expr -> String
smartBracket printOp p1 p2 e1 e2 = printOp (ifWrapped p1 e1) (ifWrapped p2 e2)
    where
        ifWrapped True e = wrap (ppExpr e)
        ifWrapped False e = ppExpr e

ppExpr (Constant n) = show n
ppExpr (Var x)      = x
ppExpr (Op Not (e:_)) = if (precedence e ) > notPr
                        then opPP Not "" (wrap $ ppExpr e)
                        else opPP Not "" (ppExpr e)
ppExpr (Op op (e1:e2:_)) = smartBracket (opPP op) (leftBind op e1) (rightBind op e2) e1 e2
ppExpr e = error $ "Poorly formed expression " ++ (show e)
        ++ ".\n This is a bug in the program that generated the expression."
