module SExp where

import Types

-- each binding as a symbol, value and function binding
data Binding = Binding String SExp Func
type Env = [(String, Binding)]

symbolFunction (Binding s v f) = f
symbolName (Binding s v f) = s
symbolValue (Binding s v f) = v

data SExp = Cons SExp SExp
          | E Atom
          | Nil
            deriving (Eq, Ord)

data Func = Prim (Env -> SExp -> SExp)
          | Func SExp
          | Lambda
          | Funbound
          | Special (Env -> SExp -> SExp)

instance Show SExp where
         show Nil   = "nil"
         show (E a) = show a
         show (Cons x y) = "("++show x++showl y
              where
              showl Nil    = ")"
              showl (E y') = " . "++show y'++")"
              showl (Cons x' y') = " "++show x'++showl y'
