module X86_32Regs where

data Reg8 = Al
          deriving (Eq, Ord)

al :: Reg8
al = Al

instance Show Reg8 where
  show Al = "%al"
