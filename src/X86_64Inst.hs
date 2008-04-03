module X86_64Inst where

import CodeGen
import Registers

call :: String -> CodeGen Env ()
call f = emit $ "\tcall\t" ++ f
binOp :: (RegOrImm a,  RegOrImm b) => String 
      -> a -> b -> CodeGen Env ()
binOp op a b
  | isAtSP a && isAtSP b = do sp <- getStackPointer
                              binOp' (Ref sp) (Ref sp)
  | isAtSP a             = do sp <- getStackPointer
                              binOp' (Ref sp) b
  | isAtSP b             = do sp <- getStackPointer
                              binOp' a (Ref sp)
  | otherwise            = binOp' a b
  where 
  binOp' x y = emit $ "\t"++op++"\t"++showRegOrImm x++", "++showRegOrImm y
add :: (RegOrImm a,  RegOrImm b) => a -> b -> CodeGen Env ()
add = binOp "addq"
sub :: (RegOrImm a,  RegOrImm b) => a -> b -> CodeGen Env ()
sub = binOp "subq"
and :: (RegOrImm a,  RegOrImm b) => a -> b -> CodeGen Env ()
and = binOp "andq"
cmp :: (RegOrImm a,  RegOrImm b) => a -> b -> CodeGen Env ()
cmp = binOp "cmpq"
je :: String -> CodeGen Env ()
je l = emit $ "\tje\t" ++ l
jmp :: String -> CodeGen Env ()
jmp l = emit $ "\tjmp\t" ++ l
label :: String -> CodeGen Env ()
label l = emit $ l ++ ":"
mov :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
mov = binOp "movq"
shr :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
shr = binOp "shrq"
shl :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
shl = binOp "shlq"
or :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
or = binOp "orq"
not :: RegOrImm a => a -> CodeGen Env ()
not a = emit $ "\tnotq\t" ++ showRegOrImm a
imul :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
imul = binOp "imulq"
idiv :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
idiv = binOp "idivq"
cltd :: CodeGen Env ()
cltd = emit "\tcltd"
ret :: CodeGen Env ()
ret = emit "\tret"
