{-# OPTIONS_GHC -fglasgow-exts #-}
module Registers where

import Immediate
import X86_64Regs
import X86_32Regs

data StackVariable = Ref RegValue
                   | AtSP -- value at current stack pointer
                     deriving (Eq, Ord)

instance Show StackVariable where
  show (Ref n) = show n ++ "(" ++ show Rsp ++ ")"
  show _       = error "oops, can't show AtSP"

class RegOrImm a where
  showRegOrImm :: a -> String
  isAtSP :: a -> Bool
  isAtSP = const False

instance RegOrImm Reg8 where
  showRegOrImm = show
  
instance RegOrImm Reg64 where
  showRegOrImm = show

instance RegOrImm RegValue where
  showRegOrImm x = "$" ++ show x

instance RegOrImm Char where
  showRegOrImm x = "$" ++ toRep x

instance RegOrImm Bool where
  showRegOrImm x = "$" ++ toRep x

instance RegOrImm Nil where
  showRegOrImm x = "$" ++ toRep x

instance RegOrImm StackVariable where
  showRegOrImm = show
  isAtSP AtSP  = True
  isAtSP _     = False
