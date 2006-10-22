module Types where

data Atom = Numb Number
          | Str String
          | Sym String
            deriving (Eq, Ord)

instance Show Atom where
         show (Numb n) = show n
         show (Str s) = show s
         show (Sym s) = concatMap (\x -> if x=='.' then "\\." else [x]) s

data Number = I Integer
            | F Float
              deriving (Eq, Ord)

instance Num Number where
         (I x) + (I y) = (I (x + y))
         (F x) + (F y) = (F (x + y))
         (I x) * (I y) = (I (x * y))
         (F x) * (F y) = (F (x * y))
         (I x) - (I y) = (I (x - y))
         (F x) - (F y) = (F (x - y))
         negate (I x)  = (I (negate x))
         negate (F x)  = (F (negate x))
         abs (I x)     = (I (abs x))
         abs (F x)     = (F (abs x))
         signum (I x)  = (I (signum x))
         signum (F x)  = (F (signum x))
         fromInteger x = (I x)

instance Show Number where
         show (I i) = show i
         show (F f) = show f
