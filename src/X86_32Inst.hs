module X86_32Inst where

import Registers
import X86_32Regs
import CodeGen

sete :: Reg8 -> CodeGen Env ()
sete r = emit $ "\tsete\t" ++ showRegOrImm r
setl :: Reg8 -> CodeGen Env ()
setl r = emit $ "\tsetl\t" ++ showRegOrImm r
setg :: Reg8 -> CodeGen Env ()
setg r = emit $ "\tsetg\t" ++ showRegOrImm r
setle :: Reg8 -> CodeGen Env ()
setle r = emit $ "\tsetle\t" ++ showRegOrImm r
setge :: Reg8 -> CodeGen Env ()
setge r = emit $ "\tsetge\t" ++ showRegOrImm r


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
add = binOp "addl"
sub :: (RegOrImm a,  RegOrImm b) => a -> b -> CodeGen Env ()
sub = binOp "subl"
and :: (RegOrImm a,  RegOrImm b) => a -> b -> CodeGen Env ()
and = binOp "andl"
cmp :: (RegOrImm a,  RegOrImm b) => a -> b -> CodeGen Env ()
cmp = binOp "cmpl"
je :: String -> CodeGen Env ()
je l = emit $ "\tje\t" ++ l
jmp :: String -> CodeGen Env ()
jmp l = emit $ "\tjmp\t" ++ l
label :: String -> CodeGen Env ()
label l = emit $ l ++ ":"
mov :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
mov = binOp "movl"
shr :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
shr = binOp "shrl"
shl :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
shl = binOp "shll"
or :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
or = binOp "orl"
not :: RegOrImm a => a -> CodeGen Env ()
not a = emit $ "\tnotl\t" ++ showRegOrImm a
imul :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
imul = binOp "imull"
idiv :: (RegOrImm a, RegOrImm b) => a -> b -> CodeGen Env ()
idiv = binOp "idivl"
cltd :: CodeGen Env ()
cltd = emit "\tcltd"
ret :: CodeGen Env ()
ret = emit "\tret"
