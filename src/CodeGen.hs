{-# OPTIONS_GHC -fglasgow-exts #-}
module CodeGen ( CodeGen 
               , Instruction
               , fresh
               , runCodeGen
               , execCodeGen
               , emit
               , uniqueLabel
               , lookupVar
               , lookupLVar
               , extendVars
               , extendLVars
               , Env
               , newEnv
               , Control.Monad.Error.throwError
               , Error
               , getStackPointer
               , setStackPointer
               , incStackPointer
               , decStackPointer
               )
where

import MonadUnique
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error hiding (Error)
import Immediate
import qualified Data.Map as M

type Instruction = String
type Error       = String

data Env = Env { lvars :: M.Map String String
               , vars  :: M.Map String RegValue
               , stackpointer :: RegValue }

newEnv :: Env
newEnv = Env M.empty M.empty 0

newtype CodeGen e a = CodeGen (ReaderT e 
                               (WriterT [Instruction]
                                (ErrorT String Unique)) a)
    deriving (Functor, Monad, MonadWriter [Instruction], MonadError Error,
              MonadReader e)

instance MonadUnique (CodeGen e) where
  fresh = CodeGen $ lift $ lift $ lift fresh

runCodeGen :: CodeGen e a -> e -> Either String (a, [Instruction])
runCodeGen (CodeGen x) env = 
    evalUnique (runErrorT (runWriterT (runReaderT x env)))
execCodeGen :: CodeGen e a -> e -> Either String [Instruction]
execCodeGen (CodeGen x) env = 
    evalUnique (runErrorT (execWriterT (runReaderT x env)))

emit :: String -> CodeGen e ()
emit s = tell [s]

lookupVar :: Monad m => String -> CodeGen Env (m RegValue)
lookupVar name = asks $ \env -> M.lookup name (vars env)

lookupLVar :: Monad m => String -> CodeGen Env (m String)
lookupLVar name = asks $ \env -> M.lookup name (lvars env)

extendVars :: [(String, RegValue)] -> CodeGen Env a -> CodeGen Env a
extendVars e = 
    local $ \env -> env { vars = (M.fromList e) `M.union` (vars env) }

extendLVars :: [(String, String)] -> CodeGen Env a -> CodeGen Env a
extendLVars e = 
    local $ \env -> env { lvars = (M.fromList e) `M.union` (lvars env) }

getStackPointer :: CodeGen Env RegValue
getStackPointer = asks stackpointer

setStackPointer :: RegValue -> CodeGen Env a -> CodeGen Env a
setStackPointer si = 
    local $ \env -> env { stackpointer = si }

incStackPointer :: CodeGen Env a -> CodeGen Env a
incStackPointer = 
    local $ \env -> env { stackpointer = (stackpointer env) - wordsize }

decStackPointer :: CodeGen Env a -> CodeGen Env a
decStackPointer = 
    local $ \env -> env { stackpointer = (stackpointer env) + wordsize }

uniqueLabel :: CodeGen e String
uniqueLabel = do
  c <- fresh
  return $ "L_" ++ show c