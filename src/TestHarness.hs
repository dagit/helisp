module TestHarness where

import Compile
--import Compile32
import System.IO
import System.Process
import System.Exit
import Control.Exception
import CodeGen
import Intermediate

compileAndRun :: Program -> IO (Either String String)
compileAndRun p = 
  case execCodeGen (emitProgram p) newEnv of
    Left err -> return $ Left err
    Right prog -> do 
      writeFile "test.s" $ unlines prog
      (asinp, asout, aserr, h) <- runInteractiveCommand compileCommand
      errmsg <- hGetContents aserr
      evaluate $ length errmsg
      hClose asinp
      hClose asout
      hClose aserr
      ec <- waitForProcess h
      case ec of
        ExitFailure _ -> do
          return $ Left $ "Failed to compile: " ++ errmsg
        ExitSuccess -> do
          (pin, pout, perr, h') <- runInteractiveCommand "./helisp_test"
          msg <- hGetContents pout
          evaluate $ length msg
          hClose pin
          hClose pout
          hClose perr
          waitForProcess h'
          return $ Right msg
  where
  compileCommand = "as --gstabs test.s -o test.o &&"
                   ++ "gcc -g -o helisp_test helisp.c test.o"

testIt :: (Program, String)
       -> IO (Either String (Bool, String))
testIt (p, answer) = do
  r <- compileAndRun p
  case r of
    Left errmsg -> return $ Left errmsg
    Right s -> return $ Right $ (s == answer, s)

runTests :: [(Program, String)] -> IO ()
runTests ts = do rs <- mapM testIt ts
                 let good = filter (\x -> case x of
                                            Right (True,_) -> True
                                            Right (False,_) -> False
                                            Left _ -> False) rs
                 let bad = filter (\(x, _) -> case x of
                                                Right (True,_) -> False
                                                Right (False,_) -> True
                                                Left _ -> True) (zip rs [0..])
                 let total = length rs
                 let goodCount = length good
                 putStrLn $ show goodCount ++ "/" ++ show total ++ " passed."
                 if goodCount < total
                    then do putStrLn $ "The following tests failed:\n" 
                            mapM_ putStrLn $ map msg bad
                    else return ()
  where
  msg ((Right (False, s)), n) = "Case: " ++ (show $ fst (ts !! n))
                                ++ "\nExpected: " ++ (show $ snd (ts !! n))
                                ++ "\nFound: " ++ s
  msg ((Right (True, _)), _)  = ""
  msg ((Left err), n)    = "Compile error for: " ++ show (ts !! n) ++ ":\n"
                           ++ err

exprToProg :: (Expr, a) -> (Program, a)
exprToProg (expr, a) = (Prog expr, a)
