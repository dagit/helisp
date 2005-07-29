module Main where

import Parser
import Eval

-- this version of main will do the evaluation
{- 
main = do { c <- getContents;
            asts <- return (parseAst c);
            r <- return (map (eval symbolTable) asts);
            mapM print r }
-}
-- This version is for just parsing, output should be equivalent to input
main = do { c <- getContents;
            asts <- return (parseAst c);
            mapM print asts }

--main = getContents >>= (map print) . ((map (eval symbolTable)) . parseAst)