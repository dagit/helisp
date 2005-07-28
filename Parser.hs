module Parser where

import Text.ParserCombinators.Parsec
import System.IO
import System
import Maybe
import qualified Data.Map as Map 
import Types
import SExp

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