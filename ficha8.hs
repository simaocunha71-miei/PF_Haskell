module Ficha8 where

import Data.Char

-- Ficha 8 Programacao Funcional
--------------------------------------------------------------------------------------- exercicio 1 ----------------------------------------------------------------------------------------------
data Frac = F Integer Integer 
--num :: Frac -> Integer
--num (F a b) = a

--den :: Frac -> Integer
--den (F a b) = b

--------------------------------------------------------------------------------------------------------
-- em alternativa

--data Frac2 = F{num :: Integer
--            ,den :: Integer} deriving Show
--------------------------------------------------------------------------------------------------------
-- alinea A

-- dada uma fraccao calcula uma fraccao equivalente, irredutıvel, e com o denominador positivo. 
-- Por exemplo, normaliza (F (-33) (-51)) deve retornar F 11 17 e normaliza (F 50 (-5)) deve retornar F (-10) 1.
-- Sugere-se que comece por definir primeiro a funcao mdc :: Integer -> Integer -> Integer 
-- que calcula o maximo divisor comum entre dois n´umeros, baseada na seguinte propriedade (atribuida a Euclides):
-- mdc x y == mdc (x+y) y == mdc x (y+x)

normaliza :: Frac -> Frac
normaliza (F i j) = (F ((signum b) * a) (abs b) )
                  where
                    a     = div i (mdc (abs i) (abs j))
                    b     = div j (mdc (abs i) (abs j))

mdc :: Integer -> Integer -> Integer -- só funciona para numeros nao negativos
mdc a b | a == 0   = b
        | b == 0   = a
        | a > b    = mdc (a-b) b
        | otherwise = mdc a (b-a)

-- alinea B
instance Eq Frac where
    (F n1 d1) == (F n2 d2) = n1 * d2 == n2 * n1

{-

*Ficha8> :i Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
    -- Defined in ‘GHC.Classes’

-}

-- basta resolver em ordem a uma destas funçoes para que se faça este exercicio    

-- alinea C
instance Ord Frac where
    f1 <= f2 = n1 * d2 <= n2 * n1
            where
                (F n1 d1) = normaliza f1
                (F n2 d2) = normaliza f2
{-
*Ficha8> :i Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
    -- Defined in ‘GHC.Classes’
-}

-- basta resolver em ordem a uma destas funçoes para que se faça este exercicio    

-- alinea D
instance Show Frac where
    show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

{-
*Ficha8> :i Show
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
    -- Defined in ‘GHC.Show’
-}

-- basta resolver em ordem a uma destas funçoes para que se faça este exercicio    

-- alinea E
instance Num Frac where
    (F n1 d1) + (F n2 d2) = normaliza (F ((n1*d2)+(n2*d1)) (d1*d2))
    (F n1 d1) - (F n2 d2) = normaliza (F ((n1*d2)-(n2*d1)) (d1*d2))
    (F n1 d1) * (F n2 d2) = normaliza (F (n1*n2) (d1*d2))
    abs (F n d)           = (F (abs n) (abs d))
    signum (F n d)        = (F (signum(n*d)) (1))
    fromInteger x         = F x 1
{-
*Ficha8> :i Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
    -- Defined in ‘GHC.Num’

-}

-- temos de definir várias funçoes (ao contrário das instâncias acima escritas)
