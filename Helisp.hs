module Main where

import Parser
import Eval

main = do { c <- getContents;
            asts <- return (parseAst c);
            r <- return (map (eval symbolTable) asts);
            mapM print r }

--main = getContents >>= (map print) . ((map (eval symbolTable)) . parseAst)