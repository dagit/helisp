module Eval where

import Parser
import SExp
import Types
import Builtins

symbolFromSExp :: SExp -> Atom
symbolFromSExp (E (Sym s)) = Sym s

functionFromEnv :: Env -> Atom -> Func
functionFromEnv e (Sym s) = symbolFunction (unsafeLookup e s)
           
symbolTable = builtIns

unsafeLookup :: Env -> String -> Binding
unsafeLookup t s = case lookup s t of
                        Just b -> b
                        Nothing -> error ("Symbol |"++s++"| not bound.")

unsafeLookupSymbol :: Env -> SExp -> Binding
unsafeLookupSymbol t (E (Sym s)) = unsafeLookup t s
unsafeLookupSymbol t x = error ("Not a symbol: "++show x)

builtIns = [("nil", Binding "nil" (E (Sym "nil")) Funbound),
            ("t", Binding "t" (E (Sym "t")) Funbound),
            ("car", Binding "car" Nil (Prim primCar)),
            ("cdr", Binding "cdr" Nil (Prim primCdr)),
--            ("reduce", Binding "reduce" Nil (Prim primFoldl)),
            ("+", Binding "+" Nil (Prim primPlus2)),
            ("eval", Binding "eval" Nil (Prim eval)),
            ("apply", Binding "apply" Nil (Prim apply))]

apply :: Env -> SExp -> SExp
apply e s = primApply e (functionFromEnv e (symbolFromSExp (car s))) 
                      (car (cdr s))

eval :: Env -> SExp -> SExp
eval st (E (Sym s)) = symbolValue (unsafeLookup st s)
eval st (E a)       = E a
eval st Nil         = Nil
eval st (Cons (E (Sym "quote")) y) = car y
eval st (Cons (E (Sym "function")) y) = car y
eval st xs = p st y'
     where
     x = car xs;
     y = cdr xs;
     (Prim p) = (symbolFunction (unsafeLookupSymbol st x))
     y' = primMapCar st (Prim eval) y

testEval :: Env -> SExp -> IO ()
testEval st exp = print (eval st exp)

evil s = head (map (testEval symbolTable) (parseAst s))

