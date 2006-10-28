module X86_64Regs where

data Reg64 = Rax | Rbx | Rcx | Rdx | Rsi | Rdi
           | Rbp | Rsp | R8  | R9  | R10 | R11
           | R12 | R13 | R14 | R15
           deriving (Eq, Ord)

instance Show Reg64 where
  show Rax = "%rax"
  show Rbx = "%rbx"
  show Rcx = "%rcx"
  show Rdx = "%rdx"
  show Rsi = "%rsi"
  show Rdi = "%rdi"
  show Rbp = "%rbp"
  show Rsp = "%rsp"
  show R8  = "%r8"
  show R9  = "%r9"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"

rax :: Reg64
rax = Rax
rbx :: Reg64
rbx = Rbx
rcx :: Reg64
rcx = Rcx
rdx :: Reg64
rdx = Rdx
rsi :: Reg64
rsi = Rsi
rdi :: Reg64
rdi = Rdi
rbp :: Reg64
rbp = Rbp
rsp :: Reg64
rsp = Rsp
r8 :: Reg64
r8  = R8 
r9 :: Reg64
r9  = R9 
r10 :: Reg64
r10 = R10
r11 :: Reg64
r11 = R11
r12 :: Reg64
r12 = R12
r13 :: Reg64
r13 = R13
r14 :: Reg64
r14 = R14
r15 :: Reg64
r15 = R15
