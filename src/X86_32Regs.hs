module X86_32Regs where

data Reg8 = Al
          deriving (Eq, Ord)

al :: Reg8
al = Al

instance Show Reg8 where
  show Al = "%al"

data Reg32 = Eax | Ebx | Ecx | Edx | Esi | Edi
           | Ebp | Esp
           deriving (Eq, Ord)

instance Show Reg32 where
  show Eax = "%eax"
  show Ebx = "%ebx"
  show Ecx = "%ecx"
  show Edx = "%edx"
  show Esi = "%esi"
  show Edi = "%edi"
  show Ebp = "%ebp"
  show Esp = "%esp"

eax :: Reg32
eax = Eax
ebx :: Reg32
ebx = Ebx
ecx :: Reg32
ecx = Ecx
edx :: Reg32
edx = Edx
esi :: Reg32
esi = Esi
edi :: Reg32
edi = Edi
ebp :: Reg32
ebp = Ebp
esp :: Reg32
esp = Esp
