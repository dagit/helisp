{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
module Compile where

import Prelude hiding (and, or, not)
import Data.Char
import CodeGen
import Intermediate
import Immediate
import Registers                     
import X86_64Inst
import X86_64Regs
import X86_32Inst
import X86_32Regs

-- These are the code emiting combinators 
emitTypeTest :: Imm a => a -> CodeGen Env ()
emitTypeTest a = do 
  and (mask a) rax
  emitValueTest (tag a)

emitValueTest :: RegOrImm a => a -> CodeGen Env ()
emitValueTest a = emitTest a (sete al)

emitTest :: RegOrImm a => a -> CodeGen Env () -> CodeGen Env ()
emitTest value test = do
  cmp value rax
  mov (0::RegValue) rax
  test
  shl bool_shift rax
  or  bool_tag   rax

emitOps :: Expr -> Expr -> CodeGen Env ()
emitOps x y = do
  emitExpr y
  mov rax AtSP
  incStackPointer $ emitExpr x

-- Emits the program entry point, called directly from C
emitEntry :: Expr -> CodeGen Env ()
emitEntry expr = do
    emitFunHeader "L_helisp_entry"
    setStackPointer (-wordsize)$ emitExpr expr
    ret
    emitFunHeader "helisp_entry"
    mov rsp rcx
    call "L_helisp_entry"
    mov rcx rsp
    ret
emitFunHeader :: String -> CodeGen Env ()
emitFunHeader name = do
  emit $ ".globl " ++ name
  emit $ "\t.type\t" ++ name ++ ", @function"
  emit $ name ++ ":"

-- Emits immediate values with correct shifting and tagging
emitImm :: Immediate -> CodeGen Env ()
emitImm (I x) = mov (toImm x) rax
emitImm (B x) = mov (toImm x) rax
emitImm (N x) = mov (toImm x) rax
emitImm (C x) = mov (toImm x) rax  
  
-- Heavy lifting, does all the real code generation
emitPrim :: Prim -> CodeGen Env ()
emitPrim (Add1 x) = emitExpr x >> add (toImm (1::RegValue)) rax
emitPrim (Sub1 x) = emitExpr x >> sub (toImm (1::RegValue)) rax
emitPrim (Not x) = do
  emitExpr x
  emitTypeTest (undefined::Bool)
  mov rax AtSP
  incStackPointer $ emitExpr x
  not rax
  and AtSP rax
  or  bool_tag   rax
emitPrim (LogNot x) = do
  emitExpr x
  shr fixnum_shift rax
  not rax
  shl fixnum_shift rax
emitPrim (IntToChar x) = do
  emitExpr x
  shl count rax
  or char_tag rax
  where
  count = char_shift - fixnum_shift
emitPrim (CharToInt x) = emitExpr x >> shr count rax
  where
  count = char_shift - fixnum_shift
emitPrim (Nullp x) = emitExpr x   >> emitTypeTest Nil
emitPrim (Zerop x) = emitExpr x   >> emitValueTest (0::RegValue)
emitPrim (Intp x)  = emitExpr x   >> emitTypeTest (undefined::RegValue)
emitPrim (Boolp x) = emitExpr x   >> emitTypeTest (undefined::Bool)
emitPrim (Charp x) = emitExpr x   >> emitTypeTest (undefined::Char)
emitPrim (Add x y) = emitOps  x y >> add AtSP rax
emitPrim (Sub x y) = emitOps  x y >> sub AtSP rax
emitPrim (Mul x y) = emitOps  x y >> shr (2::RegValue) rax >> imul AtSP rax
emitPrim (LogAnd x y) = emitOps x y >>  and AtSP rax
emitPrim (LogOr x y)  = emitOps x y >>  or AtSP rax
emitPrim (Div x y) = do 
  emitOps x y
  cltd
  idiv AtSP rax
  shl (2::RegValue) rax
emitPrim (Eqp x y)           = emitOps x y >> emitValueTest AtSP
emitPrim (LessThan x y)      = emitOps x y >> emitTest AtSP (setl al)
emitPrim (GreaterThan x y)   = emitOps x y >> emitTest AtSP (setg al)
emitPrim (LessThanEq x y)    = emitOps x y >> emitTest AtSP (setle al)
emitPrim (GreaterThanEq x y) = emitOps x y >> emitTest AtSP (setge al)

emitExpr :: Expr -> CodeGen Env ()
emitExpr (E i) = emitImm i
emitExpr (P p) = emitPrim p
emitExpr (If b e1 e2) = do
  altLabel <- fmap (++"_altLabel") uniqueLabel
  endLabel <- fmap (++"_endLabel") uniqueLabel
  emitExpr b
  cmp (toImm False) rax
  je altLabel
  emitExpr e1
  jmp endLabel
  label altLabel
  emitExpr e2
  label endLabel
emitExpr (Let [] body) = do
  emitExpr body
emitExpr (Let ((v,e):bs) body)= do
  emitExpr e
  si <- getStackPointer
  mov rax AtSP
  extendVars [(v, si)] $ incStackPointer $ emitExpr (Let bs body) 
emitExpr (Var n) = do
  ref <- lookupVar n
  case ref of
    Nothing -> throwError $ "Undefined variable " ++ n
    Just i  -> mov (Ref i) rax
emitExpr (App name es) = do
  incStackPointer $ emitArguments es
  si <- getStackPointer
  emitAdjustBase (si + wordsize)
  n <- lookupLVar name
  case n of
    Nothing -> throwError $ "Undefined function " ++ name
    Just l  -> do call l
                  emitAdjustBase (- (si + wordsize))
  where
  emitArguments []     = return ()
  emitArguments (x:xs) = do
    emitExpr x
    mov rax AtSP
    incStackPointer $ emitArguments xs

emitAdjustBase :: RegOrImm a => a -> CodeGen Env ()
emitAdjustBase n = add n rsp

emitProgram :: Program -> CodeGen Env ()
emitProgram (Prog expr) = emitEntry expr
emitProgram (Letrec bindings body) = do
  labels <- mapM (\v -> fmap (++"_"++v) uniqueLabel ) lvars
  let env = zip lvars labels
  extendLVars env $ do
    mapM_ emitLambda (zip lambdas labels)
    emitEntry body
  where
  lvars   = map fst bindings
  lambdas = map snd bindings

emitLambda :: (Lambda, String) -> CodeGen Env ()
emitLambda (Lambda [] body, lbl) = do
  emitFunHeader lbl
  setStackPointer (-wordsize) $ emitExpr body
  ret
emitLambda (Lambda fmls body, lbl) = do
  emitFunHeader lbl
  setStackPointer (-wordsize) $ emitFmls fmls
  ret
  where
  emitFmls []     = emitExpr body 
  emitFmls (f:fs) = do
    si <- getStackPointer
    extendVars [(f, si)] $ incStackPointer $ emitFmls fs