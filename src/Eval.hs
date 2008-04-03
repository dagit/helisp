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
unsafeLookupSymbol t (Cons (E (Sym "lambda")) _) = Binding "lambda" Nil Lambda
unsafeLookupSymbol t x = error ("Not a symbol: "++show x)


testFunc :: Func
-- ((lambda (x y) (+ x y)) 1 2) 
testFunc = Func (Cons (E (Sym "lambda")) (Cons formalParams
                                         (Cons body Nil)))
         where
         formalParams = Cons (E (Sym "x")) (Cons (E (Sym "y")) Nil)
         body = Cons (E (Sym "+")) 
                        (Cons (E (Sym "x")) 
                              (Cons (E (Sym "y")) Nil))


builtIns = [("nil", Binding "nil" (E (Sym "nil")) Funbound),
            ("t", Binding "t" (E (Sym "t")) Funbound),
            ("car", Binding "car" Nil (Prim primCar)),
            ("cdr", Binding "cdr" Nil (Prim primCdr)),
            ("lambda", Binding "lambda" Nil Lambda),
            ("test", Binding "test" Nil testFunc),
            ("let", Binding "let" Nil (Special primLet)),
--            ("reduce", Binding "reduce" Nil (Prim primFoldl)),
            ("+", Binding "+" Nil (Prim primPlus2)),
--            ("setq", Binding "setq" Nil (Prim primSetq1)),
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
eval st xs = case (symbolFunction (unsafeLookupSymbol st x)) of
             (Prim p) -> p st y'
             (Func sexp) -> applyLambda sexp
             Lambda -> applyLambda x
             (Special f) -> f st xs
     where
     x = car xs
     y = cdr xs
     y' = primMapCar st (Prim eval) y
     applyLambda w = lambda st (car (cdr w)) (cdr (cdr w)) y'

testEval :: Env -> SExp -> IO ()
testEval st exp = print (eval st exp)

evil s = head (map (testEval symbolTable) (parseAst s))

-- This function applies a lambda to it's arguments
-- the first sexp is the lambda params, and the second is the lambda body
-- as a list and the then finally the values of the params
-- FIXME: switch this to use the environment monad so that
-- we can thread the env through the body of the lambda and then
-- unwind at the end
lambda :: Env -> SExp -> SExp -> SExp -> SExp
lambda e params body args = evalPart e' body Nil--primMapCar e' (Prim eval) body
       where
       e' = bindParams params args++e
       evalPart env Nil last = last
       evalPart env (Cons x y) last = evalPart env y (eval env x)
-- ((lambda (x y) (+ x y)) 1 2)       
bindParams :: SExp -> SExp -> [(String, Binding)]
bindParams Nil Nil = []
bindParams params values = (s, Binding s (car values) Funbound):
                           bindParams (cdr params) (cdr values)
           where
           (Sym s) = symbolFromSExp (car params)

-- the input is of the form (let ((var1 initform1) ... (varN initformN))
--                               (action1) ... (actionN))
primLet :: Env -> SExp -> SExp
primLet e sexp = lambda e vars actions initForms
        where
        declarations = car (cdr sexp)
        vars = primMapCar e (Prim car') declarations
        initForms = primMapCar e (Prim cadr') declarations
        actions = cdr (cdr sexp)
        car' _ = car
        cadr' _ = car . cdr

-- (let ((x 1) (y 1))
--      (+ x y))