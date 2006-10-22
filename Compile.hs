module Compile where

import Data.Bits
import Data.Char

type Instruction = String

fixnum_mask  = 3
fixnum_tag   = 0
fixnum_shift = 2
char_mask    = 255
char_tag     = 15
char_shift   = 8
bool_mask    = 127
bool_tag     = 31
bool_shift   = 7
empty_list   = 47
wordsize     = 4

class Immediate a where
  toImmediate   :: a -> Int
  toImmediate   x = (toInt x `shiftL` shiftAmount x) .|. (tag x)
  fromImmediate :: Int -> a
  fromImmediate x = fromInt $ x `shiftR` (shiftAmount x)
  toRep :: a -> String
  toRep = show . toImmediate
  -- minimal definition
  toInt         :: a -> Int
  shiftAmount   :: a -> Int
  tag           :: a -> Int
  mask          :: a -> Int
  fromInt       :: Int -> a

instance Immediate Int where
  toInt       = id
  fromInt     = id
  shiftAmount = const fixnum_shift
  tag         = const fixnum_tag
  mask        = const fixnum_mask
 
instance Immediate Char where
  toInt       = ord
  fromInt     = chr
  shiftAmount = const char_shift
  tag         = const char_tag
  mask        = const char_mask

instance Immediate Bool where
  toInt True  = 1
  toInt False = 0
  fromInt 1   = True
  fromInt 0   = False
  fromInt _   = error "Not a boolean value"
  shiftAmount = const bool_shift
  tag         = const bool_tag
  mask        = const bool_mask

--compileProgram :: Immediate a => a -> [Instruction]
--compileProgram x = ["movl\t$" ++ toRep x ++ ", %eax"
--                   ,"ret"]

function :: String        -- ^ function name
         -> [Instruction] -- ^ function body
         -> [Instruction]
function name insts = [ "\t.text"
--                      , "\t.p2align 4,,15"
                      , ".globl " ++ name
                      , "\t.type\t" ++ name ++ ", @function"
                      , name ++ ":"
                      ] ++ map ('\t':) insts
                      ++ [ "\tret" ]

emitProgram expr = 
    [ ".globl L_helisp_entry"
    , "\t.type\tL_helisp_entry, @function"
    , "L_helisp_entry:" ]
    ++ map ('\t':) (emitExpr expr (-wordsize))
    ++ ["\tret"]
    ++ 
    [ ".globl helisp_entry"
    , "\t.type\thelisp_entry, @function"
    , "helisp_entry:"
--    , "\tmovl\t%esp, %ecx"
--    , "\tmovl\t4(%esp), %esp"
    , "\tcall\tL_helisp_entry"
--    , "\tmovl\t%ecx, %esp"
    , "\tret" ]


data Expr a = I a
            | Add1 (Expr a)
            | Sub1 (Expr a)
            | IntToChar (Expr a)
            | CharToInt (Expr a)
            | Nullp (Expr a)
            | Zerop (Expr a)
            | Intp (Expr a)
            | Boolp (Expr a)
            | Not (Expr a)
            | Add (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Eqp (Expr a) (Expr a)
            | GreaterThan (Expr a) (Expr a)
            | LessThan (Expr a) (Expr a)
            deriving (Eq, Show)


emitExpr (I x) _ = ["movl\t$" ++ toRep x ++ ", %eax"]
emitExpr (Add1 x) si = emitExpr x si
                    ++ ["addl\t$" ++ toRep (1::Int) ++ ", %eax"]
emitExpr (Sub1 x) si = emitExpr x si
                    ++ ["subl\t$" ++ toRep (1::Int) ++ ", %eax"]
emitExpr (IntToChar x) si = emitExpr x si
                         ++ [ "shl\t$" ++ count ++ ", %eax"
                            , "orl\t$" ++ show char_tag ++ ", %eax"]
  where
  count = show $ char_shift - fixnum_shift
emitExpr (CharToInt x) si = emitExpr x si
                         ++ [ "shr\t$" ++ count ++ ", %eax"]
  where
  count = show $ char_shift - fixnum_shift
emitExpr (Nullp x) si = emitExpr x si
                     ++ [ "cmpl\t$" ++ show empty_list ++ ", %eax"
                        , "movl\t$0, %eax"
                        , "sete\t$%al"
                        , "sall\t$" ++ show bool_shift ++ ", %eax"
                        , "orl\t$" ++ show bool_tag ++ ", %eax"]
emitExpr (Zerop x) si = emitExpr x si
                     ++ [ "cmpl\t$0, %eax"
                        , "movl\t$0, %eax"
                        , "sete\t%al"
                        , "sall\t$" ++ show bool_shift ++ ", %eax"
                        , "orl\t$" ++ show bool_tag ++ ", %eax"]
emitExpr (Intp x) si = emitExpr x si
                    ++ [ "andl\t$" ++ show fixnum_mask ++ ", %eax"
                       , "cmpl\t$" ++ show fixnum_tag  ++ ", %eax"
                       , "movl\t$0, %eax"
                       , "sete\t%al"
                       , "sall\t$" ++ show bool_shift ++ ", %eax"
                       , "orl\t$" ++ show bool_tag ++ ", %eax"]
emitExpr (Boolp x) si = emitExpr x si
                     ++ [ "andl\t$" ++ show bool_mask ++ ", %eax"
                        , "cmpl\t$" ++ show bool_tag  ++ ", %eax"
                        , "movl\t$0, %eax"
                        , "sete\t%al"
                        , "sall\t$" ++ show bool_shift ++ ", %eax"
                        , "orl\t$" ++ show bool_tag ++ ", %eax"]
emitExpr (Add x y) si = emitExpr y si
                        ++ [ "movl\t%eax, " ++ show si ++ "(%esp)" ]
                        ++ emitExpr x (si - wordsize)
                        ++ [ "addl\t" ++ show si ++ "(%esp), %eax" ]
emitExpr (Sub x y) si = emitExpr y si
                        ++ [ "movl\t%eax, " ++ show si ++ "(%esp)" ]
                        ++ emitExpr x (si - wordsize)
                        ++ [ "subl\t" ++ show si ++ "(%esp), %eax" ]
emitExpr (Mul x y) si = emitExpr y si
                        ++ [ "movl\t%eax, " ++ show si ++ "(%esp)" ]
                        ++ emitExpr x (si - wordsize)
                        ++ [ "mul\t" ++ show si ++ "(%esp), %eax" ]
emitExpr (Div x y) si = emitExpr y si
                        ++ [ "movl\t%eax, " ++ show si ++ "(%esp)" ]
                        ++ emitExpr x (si - wordsize)
                        ++ [ "div\t" ++ show si ++ "(%esp), %eax" ]
emitExpr (Eqp x y) si = emitExpr y si
                        ++ [ "movl\t%eax, " ++ show si ++ "(%esp)" ]
                        ++ emitExpr x (si - wordsize)
                        ++ [ "cmpl\t$" ++ show si  ++ "(%esp), %eax" 
                           , "movl\t$0, %eax"
                           , "sete\t%al"
                           , "movl\t%al, %eax" ]
emitExpr (LessThan x y) si = emitExpr y si
                             ++ [ "movl\t%eax, " ++ show si ++ "(%esp)" ]
                             ++ emitExpr x (si - wordsize)
                             ++ [ "cmpl\t$" ++ show si  ++ "(%esp), %eax" 
                                , "movl\t$0, %eax"
                                , "setl\t%al"
                                , "movl\t%al, %eax" ]
emitExpr (GreaterThan x y) si = emitExpr y si
                                ++ [ "movl\t%eax, " ++ show si ++ "(%esp)" ]
                                ++ emitExpr x (si - wordsize)
                                ++ [ "cmpl\t$" ++ show si  ++ "(%esp), %eax" 
                                   , "movl\t$0, %eax"
                                   , "setg\t%al"
                                   , "movl\t%al, %eax" ]


