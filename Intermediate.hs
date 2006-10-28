module Intermediate where

import Immediate

data Program = Prog Expr
             | Letrec [(String, Lambda)] Expr
             deriving (Eq, Show, Read)
data Lambda = Lambda [String] Expr
              deriving (Eq, Show, Read)
data Expr = E Immediate
          | Var String
          | If Expr Expr Expr
          | Let [(String, Expr)] Expr
          | App String [Expr]
          | P Prim
            deriving (Eq, Show, Read)
data Immediate = I RegValue
               | C Char
               | B Bool
               | N Nil
                 deriving (Eq, Show, Read)
data Prim = Add1 Expr
          | Sub1 Expr
          | IntToChar Expr
          | CharToInt Expr
          | Nullp Expr
          | Zerop Expr
          | Intp Expr
          | Boolp Expr
          | Charp Expr
          | Not Expr
          | LogNot Expr
          | LogAnd Expr Expr
          | LogOr Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Eqp Expr Expr
          | GreaterThan Expr Expr
          | GreaterThanEq Expr Expr
          | LessThan Expr Expr
          | LessThanEq Expr Expr
            deriving (Eq, Show, Read)

