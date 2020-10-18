module Ficha8 where

import Data.Char

-- Ficha 8 Programacao Funcional
--------------------------------------------------------------------------------------- exercicio 1 ----------------------------------------------------------------------------------------------
data Frac = F Integer Integer

-- alinea A
-- dada uma fraccao calcula uma fraccao equivalente, irredutıvel, e com o denominador positivo. 
-- Por exemplo, normaliza (F (-33) (-51)) deve retornar F 11 17 e normaliza (F 50 (-5)) deve retornar F (-10) 1.
-- Sugere-se que comece por definir primeiro a funcao mdc :: Integer -> Integer -> Integer 
-- que calcula o maximo divisor comum entre dois n´umeros, baseada na seguinte propriedade (atribuida a Euclides):
-- mdc x y == mdc (x+y) y == mdc x (y+x)
normaliza :: Frac -> Frac
normaliza (F i j) = undefined