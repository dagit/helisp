{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SimpleTests where

import Intermediate
import Immediate
import TestHarness
import Data.Char

default (RegValue, Double)

integers :: [(Program, String)]
integers = map exprToProg $ 
           map (\x -> ((E (I x)), show x))
           [0::RegValue, 1, -1, 10, -10, 2736, -2736, empty_list, -empty_list,
            2^29 - 1, -(2^29 - 1), 2^30-1, -(2^30-1), (2^61-1), -(2^61)]

immediateConstants :: [(Program, String)]
immediateConstants = map exprToProg $
  [ (E (B False), "#f")
  , (E (B True), "#t")
  , (E (N Nil), "()")  ] 
  ++ map (\x -> (E (C x), [x])) [' '..'~']

add1 :: [(Program, String)]
add1 = map exprToProg $
    (map (\x -> (P (Add1 (E (I x))), show $ x + 1))
     [0, -1, 1, -100, 1000, 536870910, -536870912, 2^61-2, -(2^61)])
    ++ [(P (Add1 (P (Add1 (E (I 0))))), "2")
       , (P (Add1 (P (Add1 (P (Add1 (P (Add1 (P (Add1 (P (Add1 (E (I 12))))))))))))), "18")]
    
sub1 :: [(Program, String)]
sub1 = map exprToProg $
       (map (\x -> (P (Sub1 (E (I x))), show $ x - 1))
        [0, -1, 1, -100, 1000, 536870910, -536870912, 2^61-2, -(2^61-1)])
       ++ [(P (Sub1 (P (Sub1 (E (I 2))))), "0")
          , (P (Sub1 (P (Sub1 (P (Sub1 (P (Sub1 (P (Sub1 (P (Sub1 (E (I 18))))))))))))), "12")]

nottest :: [(Program, String)]       
nottest = map exprToProg $
    [ (P (Not (E (B True))), "#f")
    , (P (Not (E (B False))), "#t")
    , (P (Not (E (I 0))), "#f")
    , (P (Not (E (I 1))), "#f")
    , (P (Not (E (C 'a'))), "#f")
    , (P (Not (E (I (-1)))), "#f")
    ]

intnot :: [(Program, String)]
intnot = map exprToProg $
    [ (P (LogNot (E (I 0))), "-1")
    , (P (LogNot (E (I (-1)))), "0")
    , (P (LogNot (E (I 1))), "-2")
    , (P (LogNot (E (I (2^61-1)))), show $ -(2^61))
    , (P (LogNot (E (I (-(2^61))))), show $ 2^61-1)
    , (P (LogNot (P (LogNot (E (I 237463))))), "237463") ]

charConvert :: [(Program, String)]
charConvert = map exprToProg $
    (map (\x -> (P (CharToInt (E (C x))), show (ord x))) [' '..'~'])
    ++ (map (\x -> (P (IntToChar (E (I x))), [chr(fromIntegral x)])) [32..126])

nullp :: [(Program, String)]
nullp = map exprToProg $
        [ (P (Nullp (E (N Nil))), "#t")
        , (P (Nullp (E (B True))), "#f")
        , (P (Nullp (E (B False))), "#f")
        , (P (Nullp (P (Nullp (E (N Nil))))), "#f")
        , (P (Nullp (E (C 'a'))), "#f")
        , (P (Nullp (E (I 0))), "#f")
        , (P (Nullp (E (I empty_list))), "#f")
        , (P (Nullp (E (I (-10)))), "#f")]

zerop :: [(Program, String)]
zerop = map exprToProg $
        [ (P (Zerop (E (I 0))), "#t")
        , (P (Zerop (E (I 1))), "#f")
        , (P (Zerop (E (I (-1)))), "#f")]

intp :: [(Program, String)]
intp = map exprToProg $
       [ (P (Intp (E (I 0))), "#t")
       , (P (Intp (E (I 1))), "#t")
       , (P (Intp (E (I (-1)))), "#t")
       , (P (Intp (E (I 37287))), "#t")
       , (P (Intp (E (I (-23837)))), "#t")
       , (P (Intp (E (I (2^61-1)))), "#t")
       , (P (Intp (E (I (-(2^61))))), "#t")
       , (P (Intp (E (B True))), "#f")
       , (P (Intp (E (B False))), "#f")
       , (P (Intp (E (N Nil))), "#f")
       , (P (Intp (E (I empty_list))), "#t")
       ] 
       ++ (map (\x -> (P (Intp (E (C x))), "#f")) [' '..'~'])
       ++ (map (\x -> (P (Intp (P (CharToInt (E (C x))))), "#t")) [' '..'~'])
       ++ (map (\x -> (P (Intp (P (IntToChar (E (I x))))), "#f")) [32..(126::RegValue)])

boolp :: [(Program, String)]
boolp = map exprToProg $
        [ (P (Boolp (E (B True))), "#t")
        , (P (Boolp (E (B False))), "#t")
        , (P (Boolp (E (I 0))), "#f")
        , (P (Boolp (E (I 1))), "#f")
        , (P (Boolp (E (I (-1)))), "#f")
        , (P (Boolp (E (N Nil))), "#f")
        , (P (Boolp (E (C 'a'))), "#f")
        , (P (Boolp (P (Boolp (E (B True))))), "#t")
        , (P (Boolp (P (Boolp (E (C 'a'))))), "#t") ]

charp :: [(Program, String)]
charp = map exprToProg $
        [ (P (Charp (E (C 'a'))), "#t")
        , (P (Charp (E (C 'Z'))), "#t")
        , (P (Charp (E (I 0))), "#f")
        , (P (Charp (E (I 1))), "#f")
        , (P (Charp (E (I (-1)))), "#f")
        , (P (Charp (E (N Nil))), "#f")
        , (P (Charp (E (B True))), "#f")
        , (P (Charp (E (B False))), "#f")
        , (P (Charp (P (Boolp (E (B True))))), "#f")
        , (P (Charp (P (Charp (E (C 'a'))))), "#f") ]

iftest :: [(Program, String)]
iftest = map exprToProg $
         [ (If (E (B True)) (E (I 12)) (E (I 13)), "12")
         , (If (E (B False)) (E (I 12)) (E (I 13)), "13")
         , (If (E (I 0)) (E (I 12)) (E (I 13)), "12")
         , (If (E (N Nil)) (E (I 12)) (E (I 13)), "12")
         , (If (E (N Nil)) (E (I 43)) (E (N Nil)), "43")
         , (If (E (B True))
            (If (E (I 12))
             (E (I 13)) (E (I 4))) (E (I 17)), "13")
         , (If (E (B False)) (E (I 12)) (If (E (B False)) (E (I 13)) (E (I 4))), "4")
         , (If (E (C 'X')) (If (E (I 1)) (E (I 2)) (E (I 3))) (If (E (I 4)) (E (I 5)) (E (I 6))), "2")
         , (If (P (Not (P (Boolp (E (B True)))))) 
            (E (I 15)) 
            (P (Boolp (E (B False)))), "#t") 
         , (If (If (P (Charp (E (C 'a')))) 
                (P (Boolp (E (C 'b')))) 
                (P (Intp (E (C 'c'))))) 
            (E (I 119)) 
            (E (I (-23))), "-23")
         , (If 
            (If (If (P (Not (E (I 1)))) 
                 (P (Not (E (I 2)))) 
                 (P (Not (E (I 3))))) 
             (E (I 4)) 
             (E (I 5))) 
            (E (I 6)) 
            (E (I 7)), "6")
         , (If (P (Not 
                   (If 
                    (If (P (Not (E (I 1)))) 
                     (P (Not (E (I 2)))) 
                     (P (Not (E (I 3))))) 
                    (E (I 4)) 
                    (E (I 5))))) 
            (E (I 6)) 
            (E (I 7)), "7")
         , (P (Not 
               (If (P (Not (If (If (P (Not (E (I 1)))) 
                                (P (Not (E (I 2)))) 
                                (P (Not (E (I 3))))) 
                            (E (I 4)) 
                            (E (I 5))))) 
                (E (I 6)) 
                (E (I 6)))), "#f")
         , (If (P (Charp (E (I 12)))) 
            (E (I 13)) 
            (E (I 14)), "14")
         , (If (P (Charp (E (C 'a')))) 
            (E (I 13)) 
            (E (I 14)), "13")
         , (P (Add1 (If (P (Sub1 (E (I 1)))) 
                     (P (Sub1 (E (I 13)))) 
                     (E (I 14)))), "13") 
         , (If (P (Eqp (E (I 12)) (E (I 13)))) 
            (E (I 12)) 
            (E (I 13)), "13")
         , (If (P (Eqp (E (I 12)) (E (I 12)))) 
            (E (I 13)) 
            (E (I 14)), "13")
         , (If (P (LessThan (E (I 12)) (E (I 13)))) (E (I 12)) (E (I 13)), "12")
         , (If (P (LessThan (E (I 12)) (E (I 12)))) (E (I 13)) (E (I 14)), "14")
         , (If (P (LessThan (E (I 13)) (E (I 12)))) (E (I 13)) (E (I 14)), "14")
         , (If (P (LessThanEq (E (I 12)) (E (I 13)))) (E (I 12)) (E (I 13)), "12")
         , (If (P (LessThanEq (E (I 12)) (E (I 12)))) (E (I 12)) (E (I 13)), "12")
         , (If (P (LessThanEq (E (I 13)) (E (I 12)))) (E (I 13)) (E (I 14)), "14")
         , (If (P (GreaterThan (E (I 12)) (E (I 13)))) (E (I 12)) (E (I 13)), "13")
         , (If (P (GreaterThan (E (I 12)) (E (I 12)))) (E (I 12)) (E (I 13)), "13")
         , (If (P (GreaterThan (E (I 13)) (E (I 12)))) (E (I 13)) (E (I 14)), "13")
         , (If (P (GreaterThanEq (E (I 12)) (E (I 13)))) (E (I 12)) (E (I 13)), "13")
         , (If (P (GreaterThanEq (E (I 12)) (E (I 12)))) (E (I 12)) (E (I 13)), "12")
         , (If (P (GreaterThanEq (E (I 13)) (E (I 12)))) (E (I 13)) (E (I 14)), "13")
         ]

addtest :: [(Program, String)]
addtest = map exprToProg $
          [ (P (Add (E (I 1)) (E (I 2))), "3")
          , (P (Add (E (I 1)) (E (I (-2)))), "-1")
          , (P (Add (E (I (-1))) (E (I 2))), "1")
          , (P (Add (E (I (-1))) (E (I (-2)))), "-3")
          , (P (Add (E (I (2^61-1))) (E (I (-1)))), show $ 2^61-2)
          , (P (Add (E (I (2^61-2))) (E (I 1))), show $ 2^61-1)
          , (P (Add (E (I (-(2^61)))) (E (I 1))), show $ -(2^61-1))
          , (P (Add (E (I (-(2^61-1)))) (E (I (-1)))), show $ -(2^61))
          , (P (Add (E (I ((2^61-1)))) (E (I (-(2^61))))), "-1")
          , (P (Add (E (I 1)) (P (Add (E (I 2)) (E (I 3))))), "6")
          , (P (Add (E (I 1)) (P (Add (E (I 2)) (E (I (-3)))))), "0")
          , (P (Add (E (I 1)) (P (Add (E (I (-2))) (E (I 3))))), "2")
          , (P (Add (E (I 1)) (P (Add (E (I (-2))) (E (I (-3)))))), "-4")
          , (P (Add (E (I (-1))) (P (Add (E (I 2)) (E (I 3))))), "4")
          , (P (Add (E (I (-1))) (P (Add (E (I 2)) (E (I (-3)))))), "-2")
          , (P (Add (E (I (-1))) (P (Add (E (I (-2))) (E (I 3))))), "0")
          , (P (Add (E (I (-1))) (P (Add (E (I (-2))) (E (I (-3)))))), "-6")
          , (P (Add (P (Add (E (I 1)) (E (I 2)))) (E (I 3))), "6")
          , (P (Add (P (Add (E (I 1)) (E (I 2)))) (E (I (-3)))), "0")
          , (P (Add (P (Add (E (I 1)) (E (I (-2))))) (E (I 3))), "2")
          , (P (Add (P (Add (E (I 1)) (E (I (-2))))) (E (I (-3)))), "-4")
          , (P (Add (P (Add (E (I (-1))) (E (I 2)))) (E (I 3))), "4")
          , (P (Add (P (Add (E (I (-1))) (E (I 2)))) (E (I (-3)))), "-2")
          , (P (Add (P (Add (E (I (-1))) (E (I (-2))))) (E (I 3))), "0")
          , (P (Add (P (Add (E (I (-1))) (E (I (-2))))) (E (I (-3)))), "-6")
          , (P (Add (P (Add (P (Add (P (Add (P (Add (P (Add (P (Add (P (Add (E (I 1)) (E (I 2)))) (E (I 3)))) (E (I 4)))) (E (I 5)))) (E (I 6)))) (E (I 7)))) (E (I 8)))) (E (I 9))), "45")
          , (P (Add (E (I 1)) (P (Add (E (I 2)) (P (Add (E (I 3)) (P (Add (E (I 4)) (P (Add (E (I 5)) (P (Add (E (I 6)) (P (Add (E (I 7)) (P (Add (E (I 8)) (E (I 9))))))))))))))))), "45")
          ]

subtest :: [(Program, String)]
subtest = map exprToProg $
          [ (P (Sub (E (I 1)) (E (I 2))), "-1")
          , (P (Sub (E (I 1)) (E (I (-2)))), "3")
          , (P (Sub (E (I (-1))) (E (I 2))), "-3")
          , (P (Sub (E (I (-1))) (E (I (-2)))), "1")
          , (P (Sub (E (I (2^61-2))) (E (I (-1)))), show $ 2^61-1)
          , (P (Sub (E (I (2^61-1))) (E (I 1))), show $ 2^61-2)
          , (P (Sub (E (I (-(2^61-1)))) (E (I 1))), show $ -(2^61))
          , (P (Sub (E (I (-(2^61)))) (E (I (-1)))), show $ -(2^61-1))
          , (P (Sub (E (I 1)) (E (I (2^61-1)))), show $ -(2^61-2))
          , (P (Sub (E (I (-1))) (E (I (2^61-1)))), show $ -(2^61))
          , (P (Sub (E (I 1)) (E (I (-(2^61-2))))), show $ 2^61-1)
          , (P (Sub (E (I (-1))) (E (I (-(2^61))))), show $ (2^61-1))
          , (P (Sub (E (I (2^61-1))) (E (I (2^61-1)))), "0")
          , (P (Sub (E (I (-(2^61-1)))) (E (I (-(2^61))))), "1")
          , (P (Sub (E (I 1)) (P (Sub (E (I 2)) (E (I 3))))), "2")
          , (P (Sub (E (I 1)) (P (Sub (E (I 2)) (E (I (-3)))))), "-4")
          , (P (Sub (E (I 1)) (P (Sub (E (I (-2))) (E (I 3))))), "6")
          , (P (Sub (E (I 1)) (P (Sub (E (I (-2))) (E (I (-3)))))), "0")
          , (P (Sub (E (I (-1))) (P (Sub (E (I 2)) (E (I 3))))), "0")
          , (P (Sub (E (I (-1))) (P (Sub (E (I 2)) (E (I (-3)))))), "-6")
          , (P (Sub (E (I (-1))) (P (Sub (E (I (-2))) (E (I 3))))), "4")
          , (P (Sub (E (I (-1))) (P (Sub (E (I (-2))) (E (I (-3)))))), "-2")
          , (P (Sub (E (I 0)) (P (Sub (E (I (-2))) (E (I (-3)))))), "-1")
          , (P (Sub (P (Sub (E (I 1)) (E (I 2)))) (E (I 3))), "-4")
          , (P (Sub (P (Sub (E (I 1)) (E (I 2)))) (E (I (-3)))), "2")
          , (P (Sub (P (Sub (E (I 1)) (E (I (-2))))) (E (I 3))), "0")
          , (P (Sub (P (Sub (E (I 1)) (E (I (-2))))) (E (I (-3)))), "6")
          , (P (Sub (P (Sub (E (I (-1))) (E (I 2)))) (E (I 3))), "-6")
          , (P (Sub (P (Sub (E (I (-1))) (E (I 2)))) (E (I (-3)))), "0")
          , (P (Sub (P (Sub (E (I (-1))) (E (I (-2))))) (E (I 3))), "-2")
          , (P (Sub (P (Sub (E (I (-1))) (E (I (-2))))) (E (I (-3)))), "4")
          , (P (Sub (P (Sub (P (Sub (P (Sub (P (Sub (P (Sub (P (Sub (P (Sub (E (I 1)) (E (I 2)))) (E (I 3)))) (E (I 4)))) (E (I 5)))) (E (I 6)))) (E (I 7)))) (E (I 8)))) (E (I 9))), "-43")
          , (P (Sub (E (I 1)) (P (Sub (E (I 2)) (P (Sub (E (I 3)) (P (Sub (E (I 4)) (P (Sub (E (I 5)) (P (Sub (E (I 6)) (P (Sub (E (I 7)) (P (Sub (E (I 8)) (E (I 9))))))))))))))))), "5")
          ]

multest :: [(Program, String)]
multest = map exprToProg $
          [ (P (Mul (E (I 2)) (E (I 3))), "6")
          , (P (Mul (E (I 2)) (E (I (-3)))), "-6")
          , (P (Mul (E (I (-2))) (E (I 3))), "-6")
          , (P (Mul (E (I (-2))) (E (I (-3)))), "6")
          , (P (Mul (E (I (2^61-1))) (E (I 1))), show $ 2^61-1)
          , (P (Mul (E (I (2^61-1))) (E (I (-1)))), show $ -(2^61-1))
          , (P (Mul (E (I (-(2^61)))) (E (I 1))), show $ -(2^61))
          , (P (Mul (E (I (-(2^61-1)))) (E (I (-1)))), show $ (2^61-1))
          , (P (Mul (E (I 2)) (P (Mul (E (I 3)) (E (I 4))))), "24")
          , (P (Mul (P (Mul (E (I 2)) (E (I 3)))) (E (I 4))), "24")
          , (P (Mul (P (Mul (P (Mul (P (Mul (P (Mul (E (I 2)) (E (I 3)))) (E (I 4)))) (E (I 5)))) (E (I 6)))) (E (I 7))), "5040")
          , (P (Mul (E (I 2)) (P (Mul (E (I 3)) (P (Mul (E (I 4)) (P (Mul (E (I 5)) (P (Mul (E (I 6)) (E (I 7))))))))))), "5040")
          ]

andortest :: [(Program, String)]
andortest = map exprToProg $
            [ (P (LogOr (E (I 3)) (E (I 16))), "19")
            , (P (LogOr (E (I 3)) (E (I 5))), "7")
            , (P (LogOr (E (I 3)) (E (I 7))), "7")
            , (P (LogNot (P (LogOr (P (LogNot (E (I 7)))) (E (I 1))))), "6")
            , (P (LogNot (P (LogOr (E (I 1)) (P (LogNot (E (I 7))))))), "6")
            , (P (LogAnd (E (I 3)) (E (I 7))), "3")
            , (P (LogAnd (E (I 3)) (E (I 5))), "1")
            , (P (LogAnd (E (I 2346)) (P (LogNot (E (I 2346))))), "0")
            , (P (LogAnd (P (LogNot (E (I 2346)))) (E (I 2346))), "0")
            , (P (LogAnd (E (I 2376)) (E (I 2376))), "2376")
            ]

eqptest :: [(Program, String)]
eqptest = map exprToProg $
          [ (P (Eqp (E (I 12)) (E (I 13))), "#f")
          , (P (Eqp (E (I 12)) (E (I 12))), "#t")
          , (P (Eqp (E (I 16)) (P (Add (E (I 13)) (E (I 3))))), "#t")
          , (P (Eqp (E (I 16)) (P (Add (E (I 13)) (E (I 13))))), "#f")
          , (P (Eqp (P (Add (E (I 13)) (E (I 3)))) (E (I 16))), "#t")
          , (P (Eqp (P (Add (E (I 13)) (E (I 13)))) (E (I 16))), "#f")
          ]

lessthantest :: [(Program, String)]
lessthantest = map exprToProg $
               [ (P (LessThan (E (I 12)) (E (I 13))), "#t")
               , (P (LessThan (E (I 12)) (E (I 12))), "#f")
               , (P (LessThan (E (I 13)) (E (I 12))), "#f")
               , (P (LessThan (E (I 16)) (P (Add (E (I 13)) (E (I 1))))), "#f")
               , (P (LessThan (E (I 16)) (P (Add (E (I 13)) (E (I 3))))), "#f")
               , (P (LessThan (E (I 16)) (P (Add (E (I 13)) (E (I 13))))), "#t")
               , (P (LessThan (P (Add (E (I 13)) (E (I 1)))) (E (I 16))), "#t")
               , (P (LessThan (P (Add (E (I 13)) (E (I 3)))) (E (I 16))), "#f")
               , (P (LessThan (P (Add (E (I 13)) (E (I 13)))) (E (I 16))), "#f")
               ]

greaterthantest :: [(Program, String)]
greaterthantest = map exprToProg $
                  [ (P (GreaterThan (E (I 12)) (E (I 13))), "#f")
                  , (P (GreaterThan (E (I 12)) (E (I 12))), "#f")
                  , (P (GreaterThan (E (I 13)) (E (I 12))), "#t")
                  , (P (GreaterThan (E (I 16)) (P (Add (E (I 13)) (E (I 1))))), "#t")
                  , (P (GreaterThan (E (I 16)) (P (Add (E (I 13)) (E (I 3))))), "#f")
                  , (P (GreaterThan (E (I 16)) (P (Add (E (I 13)) (E (I 13))))), "#f")
                  , (P (GreaterThan (P (Add (E (I 13)) (E (I 1)))) (E (I 16))), "#f")
                  , (P (GreaterThan (P (Add (E (I 13)) (E (I 3)))) (E (I 16))), "#f")
                  , (P (GreaterThan (P (Add (E (I 13)) (E (I 13)))) (E (I 16))), "#t")
                  ]

lessthaneqtest :: [(Program, String)]
lessthaneqtest = map exprToProg $
                 [ (P (LessThanEq (E (I 12)) (E (I 13))), "#t")
                 , (P (LessThanEq (E (I 12)) (E (I 12))), "#t")
                 , (P (LessThanEq (E (I 13)) (E (I 12))), "#f")
                 , (P (LessThanEq (E (I 16)) (P (Add (E (I 13)) (E (I 1))))), "#f")
                 , (P (LessThanEq (E (I 16)) (P (Add (E (I 13)) (E (I 3))))), "#t")
                 , (P (LessThanEq (E (I 16)) (P (Add (E (I 13)) (E (I 13))))), "#t")
                 , (P (LessThanEq (P (Add (E (I 13)) (E (I 1)))) (E (I 16))), "#t")
                 , (P (LessThanEq (P (Add (E (I 13)) (E (I 3)))) (E (I 16))), "#t")
                 , (P (LessThanEq (P (Add (E (I 13)) (E (I 13)))) (E (I 16))), "#f")
                 ]

greaterthaneqtest :: [(Program, String)]
greaterthaneqtest = map exprToProg $
                    [ (P (GreaterThanEq (E (I 12)) (E (I 13))), "#f")
                    , (P (GreaterThanEq (E (I 12)) (E (I 12))), "#t")
                    , (P (GreaterThanEq (E (I 13)) (E (I 12))), "#t")
                    , (P (GreaterThanEq (E (I 16)) (P (Add (E (I 13)) (E (I 1))))), "#t")
                    , (P (GreaterThanEq (E (I 16)) (P (Add (E (I 13)) (E (I 3))))), "#t")
                    , (P (GreaterThanEq (E (I 16)) (P (Add (E (I 13)) (E (I 13))))), "#f")
                    , (P (GreaterThanEq (P (Add (E (I 13)) (E (I 1)))) (E (I 16))), "#f")
                    , (P (GreaterThanEq (P (Add (E (I 13)) (E (I 3)))) (E (I 16))), "#t")
                    , (P (GreaterThanEq (P (Add (E (I 13)) (E (I 13)))) (E (I 16))), "#t")
                    ]

divtest :: [(Program, String)]
divtest = map exprToProg $
          [ (P (Div (E (I 2)) (E (I 3))), "0")
          , (P (Div (E (I 2)) (E (I (-3)))), "0")
          , (P (Div (E (I (-(2^61)))) (E (I 1))), show $ -(2^61))
          , (P (Div (E (I (-(2^61-1)))) (E (I (-1)))), show $ (2^61-1))
          , (P (Div (E (I 8)) (P (Div (E (I 32)) (E (I 16))))), "4")
          , (P (Div (P (Div (E (I 32)) (E (I 4)))) (E (I 2))), "4")
          , (P (Div (P (Div (P (Div (P (Div (P (Div (E (I 5040)) (E (I 2)))) (E (I 3)))) (E (I 4)))) (E (I 5)))) (E (I 6))), "7")
          ]

lettest :: [(Program, String)]
lettest = map exprToProg $
          [ (Let [("x", (E (I 5)))] (Var "x"), "5")
          , (Let [("x", (P (Add (E (I 1)) (E (I 2)))))] (Var "x"), "3")
          , (Let [("x", (P (Add (E (I 1)) (E (I 2)))))]
             (Let [("y", (P (Add (E (I 3)) (E (I 4)))))]
              (P (Add (Var "x") (Var "y")))), "10")
          , (Let [("x", (P (Add (E (I 1)) (E (I 2)))))]
             (Let [("y", (P (Add (E (I 3)) (E (I 4)))))]
              (P (Sub (Var "y") (Var "x")))), "4")
          , (Let [ ("x", (P (Add (E (I 1)) (E (I 2)))))
                 , ("y", (P (Add (E (I 3)) (E (I 4))))) ]
             (P (Sub (Var "y") (Var "x"))), "4")
          , (Let [ ("x", Let [ ("y", (P (Add (E (I 1)) (E (I 2)))))] 
                    (P (Mul (Var "y") (Var "y")))) ] 
             (P (Add (Var "x") (Var "x"))), "18")
          , (Let [ ("x", (P (Add (E (I 1)) (E (I 2))))) ]
             (Let [ ("x", (P (Add (E (I 3)) (E (I 4))))) ]
              (Var "x")), "7")
          , (Let [("t", (Let [("t", (Let [("t", (Let [("t", (P (Add (E (I 1)) (E (I 2)))))] (Var "t")))] (Var "t")))] (Var "t")))] (Var "t"), "3")
          , (Let [("x", (E (I 12)))]
             (Let [("x", (P (Add (Var "x") (Var "x"))))]
              (Let [("x", (P (Add (Var "x") (Var "x"))))]
               (Let [("x", (P (Add (Var "x") (Var "x"))))]
                (P (Add (Var "x") (Var "x")))))), "192")
          ]

proctest :: [(Program, String)]
proctest = [ (Letrec [] (E (I 12)), "12")
           , (Letrec [] (Let [("x", (E (I 5)))] (P (Add (Var "x") (Var "x")))), "10") 
           , (Letrec [("f", (Lambda [] (E (I 5))))] (E (I 7)), "7")
           , (Letrec [("f", (Lambda [] (E (I 5))))] 
              (Let [("x", (E (I 12)))] (Var "x")), "12")
           , (Letrec [("f", (Lambda [] (E (I 5))))] (App "f" []), "5")
           , (Letrec [("f", (Lambda [] (E (I 5))))] 
              (Let [("x", (App "f" []))] (Var "x")), "5")
           , (Letrec [("f", (Lambda [] (E (I 5))))] 
              (P (Add (App "f" []) (E (I 6)))), "11")
           , (Letrec [("f", (Lambda [] (E (I 5))))]
              (P (Sub (E (I 20)) (App "f" []))), "15")
           , (Letrec [("f", (Lambda [] (E (I 5))))]
              (P (Add (App "f" []) (App "f" []))), "10")
           , (Letrec [("f", (Lambda [] (P (Add (E (I 5)) (E (I 7))))))
                     ,("g", (Lambda [] (E (I 13))))]
              (P (Add (App "f" []) (App "g" []))), "25")
           , (Letrec [("f", (Lambda ["x"] (P (Add (Var "x") (E (I 12))))))]
              (App "f" [(E (I 13))]), "25")
           , (Letrec [("f", (Lambda ["x"] (P (Add (Var "x") (E (I 12))))))]
              (App "f" [(App "f" [(E (I 10))])]), "34")
           , (Letrec [("f", (Lambda ["x"] (P (Add (Var "x") (E (I 12))))))]
              (App "f" [(App "f" [(App "f" [(E (I 0))])])]), "36")
           , (Letrec [("f", (Lambda ["x", "y"] (P (Add (Var "x") (Var "y")))))
                     ,("g", (Lambda ["x"] (P (Add (Var "x") (E (I 12))))))]
              (App "f" [(E (I 16))
                       ,(App "f" [(App "g" [(E (I 0))])
                                 ,(P (Add (E (I 1)) (App "g" [(E (I 0))])))])])
              , "41")
           , (Letrec [("f", (Lambda ["x"] 
                             (If (P (Zerop (Var "x")))
                              (E (I 1))
                              (P (Mul (Var "x") 
                                  (App "f" [P (Sub1 (Var "x"))]))))))]
              (App "f" [E (I 5)]), "120")
           , (Letrec [("e", (Lambda ["x"]
                             (If (P (Zerop (Var "x")))
                              (E (B True))
                              (App "o" [(P (Sub1 (Var "x")))]))))
                     ,("o", (Lambda ["x"]
                             (If (P (Zerop (Var "x")))
                              (E (B False))
                              (App "e" [(P (Sub1 (Var "x")))]))))]
              (App "e" [E (I (25))]), "#f")
           ]

-- can't yet handle this one, need tail call
nestedproc :: [(Program, String)]
nestedproc = [ (Letrec [("sum", Lambda ["n","ac"]
                         (If (P (Zerop (Var "n")))
                          (Var "ac")
                          (App "sum" [(P (Sub1 (Var "n"))), 
                                      (P (Add (Var "n") (Var "ac")))])))]
                (App "sum" [(E (I 1000000)), (E (I 0))]), "500000500000")
             , (Letrec [("e", (Lambda ["x"]
                               (If (P (Zerop (Var "x")))
                                (E (B True))
                                (App "o" [(P (Sub1 (Var "x")))]))))
                       ,("o", (Lambda ["x"]
                               (If (P (Zerop (Var "x")))
                                (E (B False))
                                (App "e" [(P (Sub1 (Var "x")))]))))]
                (App "e" [E (I 5000000)]), "#f")
              ]

allTests :: [(Program, String)]                         
allTests = integers ++ immediateConstants ++ add1 ++ sub1 ++ charConvert ++
           intp ++ zerop ++ nullp ++ boolp ++ charp ++ nottest ++ intnot ++
           addtest ++ subtest ++ multest ++ andortest ++ divtest ++ 
           eqptest ++ lessthantest ++ greaterthantest ++ lessthaneqtest ++ 
           greaterthaneqtest ++ iftest ++ lettest ++ proctest
