module Builtins where

import Types
import SExp

--cons,car,cdr so on should be in a type class so they have the same interface

cons :: SExp -> SExp
cons (Cons x y) = Cons x y

car :: SExp -> SExp
car (Cons x y) = x
car Nil = Nil
car x = error ("car called with: "++show x)
cdr :: SExp -> SExp
cdr (Cons x y) = y
cdr x = error ("cdr called with: "++show x)
--This takes a list of arguments, so we need to take the car then act
--on the list
primCar :: Env -> SExp -> SExp
primCar _ s = car (car s)

primCdr :: Env -> SExp -> SExp
primCdr _ s = cdr (car s)

primSymbolP :: SExp -> Bool
primSymbolP (E (Sym _)) = True
primSymbolP _ = False

primAtomP :: SExp -> Bool
primAtomP (E _) = True
primAtomP _ = False

primListP :: SExp -> Bool
primListP = not . primAtomP

primApply :: Env -> Func -> SExp -> SExp
primApply e (Prim f) s = f e s

--primLet :: Env -> SExp -> IO SExp

--takes a function, a list of SExp's and returns Func applied to
-- the SExp's
-- primMapCar :: Env -> Func -> SExp -> SExp
-- primMapCar _ _ Nil = Nil
-- primMapCar e (Prim f) (Cons x y) = do { r <- f e x;
--                                         rs <- primMapCar e (Prim f) y;
--                                         return (Cons r rs) }
primMapCar :: Env -> Func -> SExp -> SExp
primMapCar _ _ Nil = Nil
primMapCar e (Prim f) (Cons x y) = Cons (f e x) (primMapCar e (Prim f) y)

{-
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
-}
{- things are complicated for us...
(defun foldl (f z list) ...)
which means...
that the first SExp is '(f z list), so lets tear it apart... -}
-- so for example (+ 0 (1 2)) is how we are called from primPlus2 when
-- trying to add 1 and 2
-- (+ 0 (1 2)) => (+ 1 (2)) => (+ 3 ())
primFoldl :: Env -> Func -> SExp -> SExp -> SExp
primFoldl e (Prim f) z list
          | list == Nil = z
          | otherwise = primFoldl e (Prim f) (f e l) xs
          where
          x = car list
          xs = cdr list
          l  = (Cons z (Cons x Nil))
--           | otherwise = primFoldl e (Cons sym
--                                      (Cons (f e (Cons (Cons z (Cons x Nil)) Nil))
--                                       (Cons (cdr list) Nil)))
--           where
--           sym = car s
--           Prim f = symbolFunction (unsafeLookupSymbol e sym)
--           z = car (cdr s)
--           list = car (cdr (cdr s))
--           x = car list

primPlus :: Env -> SExp -> SExp
--primPlus _ xs = error ("xs is "++(show xs))
primPlus _ xs = E (Numb (x+y))
         where
         (E (Numb x)) = car xs
         (E (Numb y)) = car (cdr xs)
--         (Cons (E (Numb x)) (Cons (E (Numb y)) _))= car xs

primPlus2 :: Env -> SExp -> SExp
primPlus2 e s = primFoldl e (Prim primPlus) (E (Numb (I 0))) s
--primPlus2 _ s = error ("primPlus2 called with: "++show s)
-- primPlus2 e s = primFoldl e (Cons (E (Sym "+")) 
--                              (Cons (E (Numb (I 0)))
--                               (Cons s Nil)))

