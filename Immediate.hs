{-# OPTIONS_GHC -fglasgow-exts #-}
module Immediate where

import Data.Char
import Data.Bits
import Data.Int

type RegValue = Int64

fixnum_mask, fixnum_tag, fixnum_shift :: RegValue
fixnum_mask  = 3
fixnum_tag   = 0
fixnum_shift = 2
char_mask, char_tag, char_shift :: RegValue
char_mask    = 255
char_tag     = 15
char_shift   = 8
bool_mask, bool_tag, bool_shift :: RegValue
bool_mask    = 127
bool_tag     = 31
bool_shift   = 7
empty_list :: RegValue
empty_list   = 47
wordsize :: RegValue
wordsize     = 8

class (Show a, Eq a) => Imm a where
  toImm   :: a -> RegValue
  toImm   x = (toInt x `shiftL` shiftAmount x) .|. (tag x)
  fromImm :: RegValue -> a
  fromImm x = fromInt $ x `shiftR` (shiftAmount x)
  toRep :: a -> String
  toRep = show . toImm
  -- minimal definition
  toInt         :: a -> RegValue
  shiftAmount   :: a -> Int
  tag           :: a -> RegValue
  mask          :: a -> RegValue
  fromInt       :: RegValue -> a

instance Imm RegValue where
  toInt       = id
  fromInt     = id
  shiftAmount = const (fromIntegral fixnum_shift)
  tag         = const fixnum_tag
  mask        = const fixnum_mask

instance Imm Char where
  toInt       = fromIntegral . ord
  fromInt     = chr . fromIntegral
  shiftAmount = const (fromIntegral char_shift)
  tag         = const char_tag
  mask        = const char_mask

instance Imm Bool where
  toInt True  = 1
  toInt False = 0
  fromInt 1   = True
  fromInt 0   = False
  fromInt _   = error "Not a boolean value"
  shiftAmount = const (fromIntegral bool_shift)
  tag         = const bool_tag
  mask        = const bool_mask

data Nil = Nil
         deriving (Eq, Show, Read)

instance Imm Nil where
  toInt       = const empty_list
  fromInt 47  = Nil
  fromInt _   = error "Not the empty list"
  shiftAmount = const 0
  tag         = const empty_list
  mask        = const 255

