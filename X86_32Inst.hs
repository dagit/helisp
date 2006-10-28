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
