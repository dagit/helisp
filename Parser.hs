module Main where

import Text.ParserCombinators.Parsec
import System.IO
import System
import Maybe
import qualified Data.Map as Map 

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
          | Funbound

instance Show SExp where
         show Nil   = "nil"
         show (E a) = show a
         show (Cons x y) = "("++show x++showl y
              where
              showl Nil    = ")"
              showl (E y') = " . "++show y'++")"
              showl (Cons x' y') = " "++show x'++showl y'

--cons,car,cdr so on should be in a type class so they have the same interface

cons :: SExp -> SExp
cons (Cons x y) = Cons x y

car :: SExp -> SExp
car (Cons x y) = x
car Nil = Nil
car x = error ("car called with: "++show x)
cdr :: SExp -> SExp
cdr (Cons x y) = y
cdr x = error ("cdr called with: "++show x)
--This takes a list of arguments, so we need to take the car then act
--on the list
primCar :: Env -> SExp -> SExp
primCar _ s = car (car s)

primCdr :: Env -> SExp -> SExp
primCdr _ s = cdr (car s)

primSymbolP :: SExp -> Bool
primSymbolP (E (Sym _)) = True
primSymbolP _ = False

primAtomP :: SExp -> Bool
primAtomP (E _) = True
primAtomP _ = False

primListP :: SExp -> Bool
primListP = not . primAtomP

primApply :: Env -> Func -> SExp -> SExp
primApply e (Prim f) s = f e s

--primLet :: Env -> SExp -> IO SExp

lambda :: Env -> SExp -> SExp
lambda e s = error ("lambda called with "++(show s))

--takes a function, a list of SExp's and returns Func applied to
-- the SExp's
-- primMapCar :: Env -> Func -> SExp -> SExp
-- primMapCar _ _ Nil = Nil
-- primMapCar e (Prim f) (Cons x y) = do { r <- f e x;
--                                         rs <- primMapCar e (Prim f) y;
--                                         return (Cons r rs) }
primMapCar :: Env -> Func -> SExp -> SExp
primMapCar _ _ Nil = Nil
primMapCar e (Prim f) (Cons x y) = Cons (f e x) (primMapCar e (Prim f) y)

{-
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
-}
{- things are complicated for us...
(defun foldl (f z list) ...)
which means...
that the first SExp is '(f z list), so lets tear it apart... -}
-- so for example (+ 0 (1 2)) is how we are called from primPlus2 when
-- trying to add 1 and 2
-- (+ 0 (1 2)) => (+ 1 (2)) => (+ 3 ())
primFoldl :: Env -> Func -> SExp -> SExp -> SExp
primFoldl e (Prim f) z list
          | list == Nil = z
          | otherwise = primFoldl e (Prim f) (f e l) xs
          where
          x = car list
          xs = cdr list
          l  = (Cons z (Cons x Nil))
--           | otherwise = primFoldl e (Cons sym
--                                      (Cons (f e (Cons (Cons z (Cons x Nil)) Nil))
--                                       (Cons (cdr list) Nil)))
--           where
--           sym = car s
--           Prim f = symbolFunction (unsafeLookupSymbol e sym)
--           z = car (cdr s)
--           list = car (cdr (cdr s))
--           x = car list

apply :: Env -> SExp -> SExp
apply e s = primApply e (functionFromEnv e (symbolFromSExp (car s))) 
                      (car (cdr s))

symbolFromSExp :: SExp -> Atom
symbolFromSExp (E (Sym s)) = Sym s

functionFromEnv :: Env -> Atom -> Func
functionFromEnv e (Sym s) = symbolFunction (unsafeLookup e s)

primPlus :: Env -> SExp -> SExp
--primPlus _ xs = error ("xs is "++(show xs))
primPlus _ xs = E (Numb (x+y))
         where
         (E (Numb x)) = car xs
         (E (Numb y)) = car (cdr xs)
--         (Cons (E (Numb x)) (Cons (E (Numb y)) _))= car xs

primPlus2 :: Env -> SExp -> SExp
primPlus2 e s = primFoldl e (Prim primPlus) (E (Numb (I 0))) s
--primPlus2 _ s = error ("primPlus2 called with: "++show s)
-- primPlus2 e s = primFoldl e (Cons (E (Sym "+")) 
--                              (Cons (E (Numb (I 0)))
--                               (Cons s Nil)))

data Atom = Numb Number
          | Str String
          | Sym String
            deriving (Eq, Ord)

instance Show Atom where
         show (Numb n) = show n
         show (Str s) = show s
         show (Sym s) = concatMap (\x -> if x=='.' then ['\\','.'] else [x]) s

data Number = I Integer
            | F Float
              deriving (Eq, Ord)

instance Num Number where
         (I x) + (I y) = (I (x + y))
         (F x) + (F y) = (F (x + y))
         (I x) * (I y) = (I (x * y))
         (F x) * (F y) = (F (x * y))
         (I x) - (I y) = (I (x - y))
         (F x) - (F y) = (F (x - y))
         negate (I x)  = (I (negate x))
         negate (F x)  = (F (negate x))
         abs (I x)     = (I (abs x))
         abs (F x)     = (F (abs x))
         signum (I x)  = (I (signum x))
         signum (F x)  = (F (signum x))
         fromInteger x = (I x)

instance Show Number where
         show (I i) = show i
         show (F f) = show f

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

runLex :: Show a => Parser a -> String -> IO ()
runLex p input
        = run (do{ whiteSpace
                 ; x <- many p
                 ; eof
                 ; return x
                 }) input

mkAst :: Show a => Parser a -> String -> a
mkAst p input
      = case (parse p "" input) of
             Right x -> x

parseAst :: String -> [SExp]
parseAst input = mkAst (do{ whiteSpace
                          ; x <- many sexp
                          ; eof
                          ; return x
                          }) input

lexeme :: Parser a -> Parser a
lexeme p = do { r <- p;
                whiteSpace;
                return r }

oparen :: Parser Char
oparen = char '(' 

cparen :: Parser Char
cparen = char ')' 

sexp :: Parser SExp
sexp = try (do { char '\'';
                 s <- lexeme sexp;
                 return (adjustForReader s (Sym "quote"))})
       <|> try (do { string "#'";
                     s <- lexeme sexp;
                     return (adjustForReader s (Sym "function"))})
       <|> try (do { lexeme oparen; 
                     s <- (lexeme sexp);
                     lexeme (char '.');
                     s'<- (lexeme sexp);
                     lexeme cparen;
                     return (Cons s s')})
       <|> do { lexeme oparen;
                s <- consManyTill (lexeme sexp) (try (lexeme cparen));
                return s }
       <|> do { a <- (lexeme atom);
                return (E a) }

adjustForReader :: SExp -> Atom -> SExp
adjustForReader s r = Cons (E r) (Cons s Nil)

consManyTill :: GenParser tok st SExp -> GenParser tok st end -> 
                GenParser tok st SExp
consManyTill p end = do { end; return Nil}
                   <|> do { s <- p;
                            do { ss <- consManyTill p end;
                                 return (Cons s ss) }
                            <|> return (Cons s Nil); }

whiteSpace = do { spaces; 
                  many (lexeme comment); }

comment = do { char ';';
               manyTill anyChar (try eoc); return ()}

eoc = eof <|> do { oneOf "\n\r"; return () }

atom :: Parser Atom
atom = choice [strAtom, numAtom, symAtom]

symbol :: Parser String
symbol = do { s1 <- many1 (noneOf "\" \t\n\r\v();'#|.")
            ; s2 <- many (noneOf "\" \t\n\r\v();'#|")
            ; return (s1++s2) }

symAtom :: Parser Atom
symAtom = do { s <- symbol;
               return (Sym s); }

str :: Parser String
str = do { char '\"';
           text <- manyTill escapeAble (try (char '\"'));
           return (text); }

strAtom :: Parser Atom
strAtom = do { s <- str;
               return (Str s);}

-- run str ['\"', '1', '2', '3', '\\', '\"', '4', '5', '\"']
-- ==> 123"45

escapeAble :: Parser Char
escapeAble = try escape <|> anyChar

escape :: Parser Char
escape = do { char '\\';
              a <- anyChar;
              return a; }

signedNumber :: Parser Number
signedNumber = try ( do { char '-';
                          n <- number;
                          return (-n)} )
               <|> number

number :: Parser Number
number = try (do { f <- float; return (F f)})
         <|> try (do { i <- integer; char '.'; return (I i)})
         <|> do { i <- integer; return (I i)}

numAtom :: Parser Atom
numAtom = do { n <- signedNumber;
               return (Numb n); }

integer :: Parser Integer
integer = do { i <- many1 digit;
               return (read i); } 

float :: Parser Float
float = do { x <- option 0 integer;
             char '.';
             y <- integer;
             return (read (show x ++ "." ++ show y)) }

builtIns = [("nil", Binding "nil" (E (Sym "nil")) Funbound),
            ("t", Binding "t" (E (Sym "t")) Funbound),
            ("car", Binding "car" Nil (Prim primCar)),
            ("cdr", Binding "cdr" Nil (Prim primCdr)),
--            ("reduce", Binding "reduce" Nil (Prim primFoldl)),
            ("+", Binding "+" Nil (Prim primPlus2)),
            ("eval", Binding "eval" Nil (Prim eval)),
            ("apply", Binding "apply" Nil (Prim apply))]
           
symbolTable = builtIns

unsafeLookup :: Env -> String -> Binding
unsafeLookup t s = case lookup s t of
                        Just b -> b
                        Nothing -> error ("Symbol |"++s++"| not bound.")

unsafeLookupSymbol :: Env -> SExp -> Binding
unsafeLookupSymbol t (E (Sym s)) = unsafeLookup t s
unsafeLookupSymbol t x = error ("Not a symbol: "++show x)

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

