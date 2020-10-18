module Ficha1 where
import Data.Char

-- 50 questoes 1º teste de Programação Funcional

--------------------------------------------------------------------------------------- exercicio 1 ----------------------------------------------------------------------------------------------
-- constrói a lista dos números inteiros compreendidos entre dois limites

-- Por exemplo, enumFromTo 1 5 corresponde a lista [1,2,3,4,5]

enumFromTo_my :: Int -> Int -> [Int]
enumFromTo_my a b | a > b = []
                  | otherwise = a : enumFromTo_my (a+1) b

--------------------------------------------------------------------------------------- exercicio 2 ----------------------------------------------------------------------------------------------
-- constrói a lista dos números inteiros compreendidos entre dois limites e espaçados de um valor constante

-- Por exemplo, enumFromThenTo 1 3 10 corresponde a lista [1,3,5,7,9]


enumFromThenTo_my :: Int -> Int -> Int -> [Int]
enumFromThenTo_my a b c | a > c = []
                        | otherwise = a : enumFromThenTo_my b (2 * b - a) c 

--------------------------------------------------------------------------------------- exercicio 3 ----------------------------------------------------------------------------------------------
-- concatena duas listas (++)

-- (++) [1,2,3] [10,20,30] corresponde a lista [1,2,3,10,20,30]

concatena :: [a] -> [a] -> [a]
concatena [] []   = []
concatena l []    = l
concatena [] l    = l
concatena (h:t) l = h : concatena t l

--------------------------------------------------------------------------------------- exercicio 4 ----------------------------------------------------------------------------------------------
-- dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posicao (assume-se que o primeiro elemento se encontra na posicao 0) - (!!)

-- Por exemplo, (!!) [10,20,30] 1 corresponde a 20

calcula_elemento_pos :: [a] -> Int -> a
calcula_elemento_pos (h:t) a = if a == 0 then h 
                                         else calcula_elemento_pos t (a-1)

--------------------------------------------------------------------------------------- exercicio 5 ----------------------------------------------------------------------------------------------
-- dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa

-- Por exemplo, reverse [10,20,30] corresponde a [30,20,10]

reverse_my :: [a] -> [a]
reverse_my []    = []
reverse_my (h:t) = reverse_my t ++ [h]

{-
Funcao auxiliar:

(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l

-}

--------------------------------------------------------------------------------------- exercicio 6 ----------------------------------------------------------------------------------------------
-- dado um inteiro n e uma lista l calcula a lista com os (no maximo) n primeiros elementos de l.
-- A lista resultado so tera menos de que n elementos se a lista l tiver menos do que n elementos.
-- Nesse caso a lista calculada e igual a lista fornecida

-- Por exemplo, take 2 [10,20,30] corresponde a [10,20]

take_my :: Int -> [a] -> [a]
take_my 0 _     = []
take_my _ []    = []
take_my a (h:t) = h : take_my (a - 1) t

--------------------------------------------------------------------------------------- exercicio 7 ----------------------------------------------------------------------------------------------
-- que dado um inteiro n e uma lista l calcula a lista sem os (no maximo) n primeiros elementos de l.
-- Se a lista fornecida tiver n elementos ou menos, a lista resultante sera vazia.

-- Por exemplo, drop 2 [10,20,30] corresponde a [30]

drop_my :: Int -> [a] -> [a]
drop_my 0 l     = l
drop_my _ []    = []
drop_my n (h:t) = drop_my (n - 1) t

--------------------------------------------------------------------------------------- exercicio 8 ----------------------------------------------------------------------------------------------
-- constroi uma lista de pares a partir de duas listas

-- Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)]

zip_my :: [a] -> [b] -> [(a,b)]
zip_my [a] [b] = [(a,b)]
zip_my (h:t) (x:xs) = (h,x) : zip_my t xs 
zip_my _ _ = []

--------------------------------------------------------------------------------------- exercicio 9 ----------------------------------------------------------------------------------------------
-- testa se um elemento ocorre numa lista

-- Por exemplo, elem 20 [10,20,30] corresponde a True 
-- enquanto que elem 2 [10,20,30] corresponde a False

elem_my :: Eq a => a -> [a] -> Bool
elem_my _ [] = False
elem_my a (h:t) = if a == h then True 
                            else elem_my a t 

--------------------------------------------------------------------------------------- exercicio 10 ----------------------------------------------------------------------------------------------
-- dado um inteiro n e um elemento x constroi uma lista com n elementos, todos iguais a x

-- Por exemplo, replicate 3 10 corresponde a [10,10,10]

replicate_my :: Int -> a -> [a]
replicate_my 0 a = []
replicate_my i a = a : replicate_my (i-1) a 

--------------------------------------------------------------------------------------- exercicio 11 ----------------------------------------------------------------------------------------------
-- dado um elemento e uma lista, constroi uma lista em que o elemento fornecido e intercalado entre os elementos da lista fornecida

-- Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30]

intersperce_my :: a -> [a] -> [a]
intersperce_my _ [] = []
intersperce_my a [b] = [b]
intersperce_my a (h:t) = h:a:intersperce_my a t

--------------------------------------------------------------------------------------- exercicio 12 ----------------------------------------------------------------------------------------------
-- agrupa elementos iguais e consecutivos de uma lista

-- Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]]

--group_my :: Eq a => [a] -> [[a]]
--group_my [] = [[]]
--group_my (h:t) = (h:my_takeWhile (== h) t) : group_my (my_dropWhile (== h) t)

{-
-- determina os primeiros elementos da lista que satisfazem um dado predicado; por exemplo
--                                                                              takeWhile odd [1,3,4,5,6,6] == [1,3]

my_takeWhile :: (a->Bool) -> [a] -> [a]
my_takeWhile _ []    = []
my_takeWhile f (h:t) = if f h then h: my_takeWhile f t 
                              else []


-- elimina os primeiros elementos da lista que satisfazem um dado predicado; por exemplo:
--                                                                           dropWhile odd [1,3,4,5,6,6] == [4,5,6,6]

my_dropWhile :: (a->Bool) -> [a] -> [a]
my_dropWhile _ []    = []
my_dropWhile f (h:t) = if f h then my_dropWhile f t
                              else (h:t)
-}

--------------------------------------------------------------------------------------- exercicio 13 ----------------------------------------------------------------------------------------------
-- concatena as listas de uma lista

-- Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]

concat_my :: [[a]] -> [a]
concat_my []    = []
concat_my (h:t) = h ++ concat_my t

{-
Funcao auxiliar:

(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l

-}

--------------------------------------------------------------------------------------- exercicio 14 ----------------------------------------------------------------------------------------------
-- calcula a lista dos prefixos de uma lista

-- Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]

inits_my :: [a] -> [[a]]
inits_my [] = [[]]
inits_my l = inits_my (init_my l) ++ [l] 




init_my :: [a] -> [a]
init_my [a] = []
init_my (h:t) = h: init_my t

{-
(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l
-}


--------------------------------------------------------------------------------------- exercicio 15 ----------------------------------------------------------------------------------------------
-- calcula a lista dos sufixos de uma lista

-- Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]]

tails_my :: [a] -> [[a]]
tails_my [] = [[]]
tails_my l = l : tails_my (tail_my l) 




tail_my :: [a] -> [a]
tail_my [a] = []
tail_my (h:t) = t



--------------------------------------------------------------------------------------- exercicio 16 ----------------------------------------------------------------------------------------------
-- testa se uma lista e prefixo de outra

-- Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True 
-- enquanto que isPrefixOf [10,30] [10,20,30] corresponde a False

isPrefixOf_my :: Eq a => [a] -> [a] -> Bool
isPrefixOf_my l1 l2 = l1 `elem_my` inits_my l2

{-

elem_my :: Eq a => a -> [a] -> Bool
elem_my _ [] = False
elem_my a (h:t) = if a == h then True 
                            else elem_my a t 

inits_my :: [a] -> [[a]]
inits_my [] = [[]]
inits_my l = inits_my (init_my l) ++ [l] 


init_my :: [a] -> [a]
init_my [a] = []
init_my (h:t) = h: init_my t


(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l


-}

--------------------------------------------------------------------------------------- exercicio 17 ----------------------------------------------------------------------------------------------
--  testa se uma lista e sufixo de outra

-- Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True 
-- enquanto que isSuffixOf [10,30] [10,20,30] corresponde a False

isSuffixOf_my :: Eq a => [a] -> [a] -> Bool
isSuffixOf_my l1 l2 = l1 `elem_my` tails_my l2

{-

elem_my :: Eq a => a -> [a] -> Bool
elem_my _ [] = False
elem_my a (h:t) = if a == h then True 
                            else elem_my a t 

tails_my :: [a] -> [[a]]
tails_my [] = [[]]
tails_my l = l : tails_my (tail_my l) 


tail_my :: [a] -> [a]
tail_my [a] = []
tail_my (h:t) = t


-}

--------------------------------------------------------------------------------------- exercicio 18 ----------------------------------------------------------------------------------------------
-- testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa

-- Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True 
-- enquanto que isSubsequenceOf [40,20] [10,20,30,40] corresponde a False

isSubsequenceOf_my :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf_my [] _        = True
isSubsequenceOf_my _ []        = False
isSubsequenceOf_my (h:t) (a:b) = (h == a && isSubsequenceOf_my t b) || 
                                            isSubsequenceOf_my (h:t) b 

--------------------------------------------------------------------------------------- exercicio 19 ----------------------------------------------------------------------------------------------
-- calcula a lista de posicoes em que um dado elemento ocorre numa lista

-- Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6]

elemIndices_my :: Eq a => a -> [a] -> [Int]
elemIndices_my _ [] = []
elemIndices_my a (h:t) | a == h    = 0 : map (+1) (elemIndices_my a t)
                       | otherwise =     map (+1) (elemIndices_my a t)

--------------------------------------------------------------------------------------- exercicio 20 ----------------------------------------------------------------------------------------------
-- calcula uma lista com os mesmos elementos da recebida, sem repeticoes

-- Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3]

nub_my :: Eq a => [a] -> [a]
nub_my [] = []
nub_my (h:t) = if elem h t then nub_my t 
                           else h : nub_my t

--------------------------------------------------------------------------------------- exercicio 21 ----------------------------------------------------------------------------------------------
-- retorna a lista resultante de remover (a primeira ocorrencia de) um dado elemento de uma lista

-- Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2] 
-- Se nao existir nenhuma ocorrencia a funcao devera retornar a lista recebida

delete_my :: Eq a => a -> [a] -> [a]
delete_my _ [] = []
delete_my a (h:t) = if a == h then t 
                              else h : delete_my a t 

--------------------------------------------------------------------------------------- exercicio 22 ----------------------------------------------------------------------------------------------
-- retorna a lista resultante de remover (as primeiras ocorrencias) dos elementos da segunda lista da primeira (\\)

-- Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1]

remove_ocorrencias :: Eq a => [a] -> [a] -> [a] 
remove_ocorrencias l [] = l
remove_ocorrencias [] _ = []
remove_ocorrencias l (h:t) = remove_ocorrencias (delete_my h l) t

{-

delete_my :: Eq a => a -> [a] -> [a]
delete_my _ [] = []
delete_my a (h:t) = if a == h then t 
                              else h : delete_my a t 

-}

--------------------------------------------------------------------------------------- exercicio 23 ----------------------------------------------------------------------------------------------
-- retorna a lista resultante de acrescentar a primeira lista os elementos da segunda que nao ocorrem na primeira

-- Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5]

union_my :: Eq a => [a] -> [a] -> [a]
union_my l [] = l
union_my [] _ = []
union_my l (h:t) = if elem_my h l then  union_my l t
                                  else (union_my l t) ++ [h]

{-

elem_my :: Eq a => a -> [a] -> Bool
elem_my _ [] = False
elem_my a (h:t) = if a == h then True 
                            else elem_my a t 

(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l

-}

--------------------------------------------------------------------------------------- exercicio 24 ----------------------------------------------------------------------------------------------
-- retorna a lista resultante de remover da primeira lista os elementos que nao pertencem a segunda

-- Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3]

intersect_my :: Eq a => [a] -> [a] -> [a]
intersect_my [] _ = []
intersect_my (h:t) l | h `elem` l = h : intersect_my t l
                     | otherwise  =     intersect_my t l

{-

elem_my :: Eq a => a -> [a] -> Bool
elem_my _ [] = False
elem_my a (h:t) = if a == h then True 
                            else elem_my a t 

-}

--------------------------------------------------------------------------------------- exercicio 25 ----------------------------------------------------------------------------------------------
-- dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista

-- Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40]

insert_my ::  Ord a => a -> [a] -> [a] 
insert_my _ [] = []
insert_my a (h:t) | a < h     = a : h : t 
                  | a == h    = h : t 
                  | otherwise = h : insert_my a t   

--------------------------------------------------------------------------------------- exercicio 26 ---------------------------------------------------------------------------------------------
-- junta todas as strings da lista numa so, separando-as por um espaco

-- Por exemplo, unwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional"

unwords_my :: [String] -> String
unwords_my []    = ""
unwords_my (h:t) = h ++ unwords_my t

{-

(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l

-}

--------------------------------------------------------------------------------------- exercicio 27 ---------------------------------------------------------------------------------------------
-- junta todas as strings da lista numa so, separando-as pelo caracter ’\n’

-- Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n"

unlines_my :: [String] -> String
unlines_my []    = ""
unlines_my (h:t) = h ++ "\n" ++ unlines_my t

{-

(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l

-}

--------------------------------------------------------------------------------------- exercicio 28 ---------------------------------------------------------------------------------------------
-- dada uma lista nao vazia, retorna a posicao onde se encontra o maior elemento da lista. 
-- As posicoes da lista comecam em 0, i.e., a funcao devera retornar 0 se o primeiro elemento da lista for o maior

pMaior_my :: Ord a => [a] -> Int
pMaior_my [a] = 0
pMaior_my (h:t) | h > (t !! (pMaior_my t)) = 1
                | otherwise                = 1 + pMaior_my t

{-

(!!) :: [a] -> Int -> a
(!!) (h:t) a = if a == 0 then h 
                         else (!!) t (a-1)

-} 

--------------------------------------------------------------------------------------- exercicio 29 ---------------------------------------------------------------------------------------------
-- testa se uma lista tem elementos repetidos

-- Por exemplo, temRepetidos [11,21,31,21] corresponde a True 
-- enquanto que temRepetidos [11,2,31,4]   corresponde a False

temRepetidos_my :: Eq a => [a] -> Bool
temRepetidos_my [a]     = False
temRepetidos_my (h:x:t) = if h `elem` (x:t) then True else temRepetidos_my (x:t)

{-

elem_my :: Eq a => a -> [a] -> Bool
elem_my _ [] = False
elem_my a (h:t) = if a == h then True 
                            else elem_my a t 

-}

--------------------------------------------------------------------------------------- exercicio 30 ---------------------------------------------------------------------------------------------
-- determina a lista dos algarismos de uma dada lista de caracteres

-- Por exemplo, algarismos "123xp5" corresponde a "1235"

algarismos_my :: [Char] -> [Char]
algarismos_my []    = []
algarismos_my (h:t) = if h `elem` ['0'..'9'] then h : algarismos_my t 
                                             else algarismos_my t

--------------------------------------------------------------------------------------- exercicio 31 ---------------------------------------------------------------------------------------------
-- determina os elementos de uma lista que ocorrem em posicoes ımpares 
-- Considere que o primeiro elemento da lista ocorre na posicao 0 e por isso par

-- Por exemplo, posImpares [10,11,7,5] corresponde a [11,5]

posImpares_my :: [a] -> [a]
posImpares_my []      = []
posImpares_my [a]     = []
posImpares_my (h:x:t) = x : posImpares_my t

--------------------------------------------------------------------------------------- exercicio 32 ---------------------------------------------------------------------------------------------
-- determina os elementos de uma lista que ocorrem em posicoes pares 
-- Considere que o primeiro elemento da lista ocorre na posicao 0 e por isso par

-- Por exemplo, posPares [10,11,7,5] corresponde a [10,7]

posPares_my :: [a] -> [a]
posPares_my []      = []
posPares_my [a]     = [a]
posPares_my (h:x:t) = h : posPares_my t

--------------------------------------------------------------------------------------- exercicio 33 ---------------------------------------------------------------------------------------------
--  testa se uma lista esta ordenada por ordem crescente

-- Por exemplo, isSorted [1,2,2,3,4,5] corresponde a True,
-- enquanto que isSorted [1,2,4,3,4,5] corresponde a False

isSorted_my :: Ord a => [a] -> Bool
isSorted_my []      = True
isSorted_my [a]     = True
isSorted_my (h:x:t) = if h <= x then isSorted_my (x:t) else False

--------------------------------------------------------------------------------------- exercicio 34 ---------------------------------------------------------------------------------------------
-- calcula o resultado de ordenar uma lista.
-- Assuma, se precisar, que existe definida a funcao insert :: Ord a => a -> [a] -> [a] que 
-- dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista

iSort_my :: Ord a => [a] -> [a]
iSort_my [] = []
iSort_my (h:t) = insert_my h (iSort_my t)

{-

insert_my ::  Ord a => a -> [a] -> [a] 
insert_my _ [] = []
insert_my a (h:t) | a < h     = a : h : t 
                  | a == h    = h : t 
                  | otherwise = h : insert_my a t 

-}

--------------------------------------------------------------------------------------- exercicio 35 ---------------------------------------------------------------------------------------------
-- dadas duas strings, retorna True se e so se a primeira for menor do que a segunda, segundo a ordem lexicografica (i.e., do dicionario)

-- Por exemplo, menor "sai" "saiu"              corresponde a True 
-- enquanto que menor "programacao" "funcional" corresponde a False

menor :: String -> String -> Bool
menor "" ""           = False
menor _ ""            = False
menor "" _            = True
menor (h1:t1) (h2:t2) = if (ord h1) <= (ord h2) then menor t1 t2 
                                                else False

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a
-- Considere ainda que nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero


--------------------------------------------------------------------------------------- exercicio 36 ---------------------------------------------------------------------------------------------
-- testa se um elemento pertence a um multi-conjunto
-- Por exemplo, elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a True 
-- enquanto que elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a False.

elemMSet_my :: Eq a => a -> [(a,Int)] -> Bool
elemMSet_my _ [] = False
elemMSet_my a ((b,d):t) = if a == b then True else elemMSet_my a t 

--------------------------------------------------------------------------------------- exercicio 37 ---------------------------------------------------------------------------------------------
-- calcula o tamanho de um multiconjunto

-- Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a 7

lengthMSet_my ::[(a,Int)] -> Int
lengthMSet_my [] = 0
lengthMSet_my ((a,b):t) = b + lengthMSet_my t

--------------------------------------------------------------------------------------- exercicio 38 ---------------------------------------------------------------------------------------------
-- converte um multi-conjuto na lista dos seus elementos

-- Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a "bbaaaac"

converteMSet_my :: [(a,Int)] -> [a]
converteMSet_my [] = []
converteMSet_my ((a,b):t) = (replicate_my b a) ++ converteMSet_my t 

{-

replicate_my :: Int -> a -> [a]
replicate_my 0 a = []
replicate_my i a = a : replicate_my (i-1) a 

(++) :: [a] -> [a] -> [a]
(++) [] []   = []
(++) l []    = l
(++) [] l    = l
(++) (h:t) l = h : (++) t l

-}

--------------------------------------------------------------------------------------- exercicio 39 ---------------------------------------------------------------------------------------------
