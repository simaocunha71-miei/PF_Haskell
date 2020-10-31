module Gera_Aleatorio where

import System.Random
import Data.Char

--------------------------------------------------- Gerador de questoes aleatorias - 50 questoes de Programacao Funcional ----------------------------------------------

-- Dado um inteiro, associa esse inteiro a uma certa questao em forma de lista de strings
associa_questao :: Int -> [String]
associa_questao i = case i of 
                         1 -> ["constroi a lista dos numeros inteiros compreendidos entre dois limites", 
                               "Por exemplo, enumFromTo 1 5 corresponde a lista [1,2,3,4,5]",
                               "enumFromTo :: Int -> Int -> [Int]"]

                         2 -> ["constroi a lista dos números inteiros compreendidos entre dois limites e espaçados de um valor constante",
                               "Por exemplo, enumFromThenTo 1 3 10 corresponde a lista [1,3,5,7,9]",
                               "enumFromThenTo :: Int -> Int -> Int -> [Int]"]

                         3 -> ["junta duas listas",
                               "(++) [1,2,3] [10,20,30] corresponde a lista [1,2,3,10,20,30]" ,
                               "(++) :: [a] -> [a] -> [a]"]

                         4 -> ["dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posicao (assume-se que o primeiro elemento se encontra na posicao 0)",
                               "Por exemplo, (!!) [10,20,30] 1 corresponde a 20",
                               "(!!) :: [a] -> Int -> a"]

                         5 -> ["dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa",
                               "Por exemplo, reverse [10,20,30] corresponde a [30,20,10]",
                               "reverse :: [a] -> [a]"]

                         6 -> ["dado um inteiro n e uma lista l calcula a lista com os (no maximo) n primeiros elementos de l ",
                               "A lista resultado so tera menos de que n elementos se a lista l tiver menos do que n elementos ",
                               "Nesse caso a lista calculada e igual a lista fornecida",
                               "Por exemplo, take 2 [10,20,30] corresponde a [10,20]",
                               "take :: Int -> [a] -> [a]"]

                         7 -> ["que dado um inteiro n e uma lista l calcula a lista sem os (no maximo) n primeiros elementos de l ",
                               "Se a lista fornecida tiver n elementos ou menos, a lista resultante sera vazia ",
                               "Por exemplo, drop 2 [10,20,30] corresponde a [30]",
                               "drop :: Int -> [a] -> [a]"]

                         8 -> ["constroi uma lista de pares a partir de duas listas",
                               "Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)]",
                               "zip :: [a] -> [b] -> [(a,b)]"]

                         9 -> ["testa se um elemento ocorre numa lista",
                               "Por exemplo, elem 20 [10,20,30] corresponde a True",
                               "enquanto que elem 2 [10,20,30] corresponde a False",
                               "elem :: Eq a => a -> [a] -> Bool"]

                         10 -> ["dado um inteiro n e um elemento x constroi uma lista com n elementos, todos iguais a x",
                                "Por exemplo, replicate 3 10 corresponde a [10,10,10]" ,
                                "replicate :: Int -> a -> [a]"]

                         11 -> ["dado um elemento e uma lista, constroi uma lista em que o elemento fornecido e intercalado entre os elementos da lista fornecida",
                                "Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30]",
                                "intersperce :: a -> [a] -> [a]"]

                         12 -> ["agrupa elementos iguais e consecutivos de uma lista",
                                "Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]]",
                                "group :: Eq a => [a] -> [[a]]"]

                         13 -> ["concatena as listas de uma lista" ,
                                "Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]" ,
                                "group :: Eq a => [a] -> [[a]]"]

                         14 -> ["calcula a lista dos prefixos de uma lista",
                                "Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]" ,
                                "inits :: [a] -> [[a]]"]

                         15 -> ["calcula a lista dos sufixos de uma lista",
                                "Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]]",
                                "tails :: [a] -> [[a]]"]

                         16 -> ["testa se uma lista e prefixo de outra",
                                "Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True ",
                                "enquanto que isPrefixOf [10,30] [10,20,30] corresponde a False",
                                "isPrefixOf :: Eq a => [a] -> [a] -> Bool"]

                         17 -> ["testa se uma lista e sufixo de outra",
                                "Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True" ,
                                "enquanto que isSuffixOf [10,30] [10,20,30] corresponde a False" ,
                                "isSuffixOf :: Eq a => [a] -> [a] -> Bool"]

                         18 -> ["testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa",
                                "Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True " ,
                                "enquanto que isSubsequenceOf [40,20] [10,20,30,40] corresponde a False" ,
                                "isSubsequenceOf :: Eq a => [a] -> [a] -> Bool"]

                         19 -> ["calcula a lista de posicoes em que um dado elemento ocorre numa lista",
                                "Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6]",
                                "elemIndices :: Eq a => a -> [a] -> [Int]" ]

                         20 -> ["calcula uma lista com os mesmos elementos da recebida, sem repeticoes",
                                "Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3]" ,
                                "nub :: Eq a => [a] -> [a]"]

                         21 -> ["retorna a lista resultante de remover (a primeira ocorrencia de) um dado elemento de uma lista",
                                "Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2] " ,
                                "Se nao existir nenhuma ocorrencia a funcao devera retornar a lista recebida",
                                "delete :: Eq a => a -> [a] -> [a]"]

                         22 -> ["retorna a lista resultante de remover (as primeiras ocorrencias) dos elementos da segunda lista da primeira",
                                "Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1]" ,
                                "remove_ocorrencias :: Eq a => [a] -> [a] -> [a]"]

                         23 -> ["retorna a lista resultante de acrescentar a primeira lista os elementos da segunda que nao ocorrem na primeira",
                                "Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5]" ,
                                "union :: Eq a => [a] -> [a] -> [a]"]

                         24 -> ["retorna a lista resultante de remover da primeira lista os elementos que nao pertencem a segunda",
                                "Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3]" ,
                                "intersect :: Eq a => [a] -> [a] -> [a]"]

                         25 -> ["dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista",
                                "Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40]",
                                "insert ::  Ord a => a -> [a] -> [a]"]

                         26 -> ["junta todas as strings da lista numa so, separando as por um espaco" ,
                                "Por exemplo, unwords ['Programacao', 'Funcional'] corresponde a 'Programacao Funcional' ",
                                "unwords :: [String] -> String"]

                         27 -> ["junta todas as strings da lista numa so, separando-as pelo caracter '\n' ",
                                "Por exemplo, unlines ['Prog', 'Func'] corresponde a 'Prog\nFunc\n' " ,
                                "unlines :: [String] -> String"]

                         28 -> ["dada uma lista nao vazia, retorna a posicao onde se encontra o maior elemento da lista" ,
                                "As posicoes da lista comecam em 0, i.e., a funcao devera retornar 0 se o primeiro elemento da lista for o maior" ,
                                "pMaior :: Ord a => [a] -> Int"]

                         29 -> ["testa se uma lista tem elementos repetidos",
                                "Por exemplo, temRepetidos [11,21,31,21] corresponde a True",
                                "enquanto que temRepetidos [11,2,31,4]   corresponde a False",
                                "temRepetidos :: Eq a => [a] -> Bool"]

                         30 -> ["determina a lista dos algarismos de uma dada lista de caracteres",
                                "Por exemplo, algarismos '123xp5' corresponde a '1235' ",
                                "algarismos :: [Char] -> [Char]"]

                         31 -> ["determina os elementos de uma lista que ocorrem em posicoes ımpares ",
                                "Considere que o primeiro elemento da lista ocorre na posicao 0 e por isso par",
                                "Por exemplo, posImpares [10,11,7,5] corresponde a [11,5]",
                                "posImpares :: [a] -> [a]"]

                         32 -> ["determina os elementos de uma lista que ocorrem em posicoes pares  ",
                                "Considere que o primeiro elemento da lista ocorre na posicao 0 e por isso par",
                                "Por exemplo, posPares [10,11,7,5] corresponde a [10,7]",
                                "posPares :: [a] -> [a]"]

                         33 -> ["testa se uma lista esta ordenada por ordem crescente ",
                                "Por exemplo, isSorted [1,2,2,3,4,5] corresponde a True,",
                                "enquanto que isSorted [1,2,4,3,4,5] corresponde a False",
                                "isSorted :: Ord a => [a] -> Bool"]

                         34 -> ["calcula o resultado de ordenar uma lista. ",
                                "Assuma, se precisar, que existe definida a funcao insert :: Ord a => a -> [a] -> [a] que ",
                                "dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista",
                                "iSort :: Ord a => [a] -> [a]"]

                         35 -> ["dadas duas strings, retorna True se e so se a primeira for menor do que a segunda, segundo a ordem lexicografica (i.e., do dicionario) ",
                                "Por exemplo, menor 'sai' 'saiu'              corresponde a True  ",
                                "enquanto que menor 'programacao' 'funcional' corresponde a False",
                                "menor :: String -> String -> Bool"]

                         36 -> ["Considere que se usa o tipo [(a,Int)] para representar multiConjuntos de elementos de a ",
                                "Considere ainda que nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero",
                                "testa se um elemento pertence a um multiConjunto",
                                "Por exemplo, elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a True ",
                                "enquanto que elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a False.",
                                "elemMSet :: Eq a => a -> [(a,Int)] -> Bool"]

                         37 -> ["Considere que se usa o tipo [(a,Int)] para representar multiConjuntos de elementos de a ",
                                "Considere ainda que nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero",
                                "calcula o tamanho de um multiconjunto" ,
                                "Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a 7",
                                "lengthMSet_my ::[(a,Int)] -> Int"]

                         38 -> ["Considere que se usa o tipo [(a,Int)] para representar multiConjuntos de elementos de a ",
                                "Considere ainda que nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero",
                                "converte um multiConjunto na lista dos seus elementos",
                                "Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a 'bbaaaac'",
                                "converteMSet :: [(a,Int)] -> [a]"]

                         39 -> ["Considere que se usa o tipo [(a,Int)] para representar multiConjuntos de elementos de a ",
                                "Considere ainda que nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero",
                                "acrescenta um elemento a um multiConjunto",
                                "Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),(’a’,4), (’c’,2)]",
                                "insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)] "]

                         40 -> ["Considere que se usa o tipo [(a,Int)] para representar multiConjuntos de elementos de a ",
                                "Considere ainda que nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero",
                                "remove um elemento a um multiConjunto. ",
                                "Se o elemento nao existir, deve ser retornado o multiConjuntorecebido",
                                "Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),(’a’,4)] ",
                                "removeMSet ::   Eq a => a -> [(a,Int)] -> [(a,Int)]"]

                         41 -> ["Considere que se usa o tipo [(a,Int)] para representar multiConjuntos de elementos de a ",
                                "Considere ainda que nestas listas nao ha pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero",
                                "dada uma lista ordenada por ordem crescente, calcula o multiConjunto dos seus elementos" ,
                                "Por exemplo, constroiMSet 'aaabccc' corresponde a [(’a’,3), (’b’,1), (’c’,3)]",
                                "constroiMSet :: Ord a => [a] -> [(a,Int)] "]

                         42 -> ["divide uma lista de Eithers em duas listas ",
                                "partitionEithers :: [Either a b] -> ([a],[b])" ]

                         43 -> ["colecciona os elementos do tipo a de uma lista ",
                                "catMaybes :: [Maybe a] -> [a]" ]

                         44 -> ["Considere o seguinte tipo para representar movimentos de um robot: ",
                                "data Movimento = Norte | Sul | Este | Oeste deriving Show",
                                "dada uma posicao inicial (coordenadas) e uma lista de movimentos, calcula a posicao final do robot depois de efectuar essa sequencia de movimentos",
                                "posicao :: (Int,Int) -> [Movimento] -> (Int,Int)"]

                         45 -> ["Considere o seguinte tipo para representar movimentos de um robot: ",
                                "data Movimento = Norte | Sul | Este | Oeste deriving Show",
                                "dadas as posicoes inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o robot passe de uma posicao para a outra",
                                "caminho :: (Int,Int) -> (Int,Int) -> [Movimento]"]

                         46 -> ["Considere o seguinte tipo para representar movimentos de um robot: ",
                                "data Movimento = Norte | Sul | Este | Oeste deriving Show",
                                "testa se uma lista de movimentos so e composta por movimentos verticais (Norte ou Sul)",
                                "vertical :: [Movimento] -> Bool"]

                         47 -> ["Considere o seguinte tipo para representar a posição de um robot numa grelha: ",
                                "data Posicao = Pos Int Int deriving Show",
                                "dada uma lista nao vazia de posicoes, determina a que esta mais perto da origem (note que as coordenadas de cada ponto sao numeros inteiros)",
                                "maisCentral :: [Posicao] -> Posicao"]

                         48 -> ["Considere o seguinte tipo para representar a posição de um robot numa grelha: ",
                                "data Posicao = Pos Int Int deriving Show",
                                "dada uma posicao e uma lista de posicoes, selecciona da lista as posicoes adjacentes a posicao dada",
                                "vizinhos_my :: Posicao -> [Posicao] -> [Posicao]"]

                         49 -> ["Considere o seguinte tipo para representar a posição de um robot numa grelha:",
                                "data Posicao = Pos Int Int deriving Show",
                                "testa se todas as posicoes de uma dada lista tem a mesma ordenada",
                                "mesmaOrdenada :: [Posicao] -> Bool"]

                         50 -> ["Considere o seguinte tipo para representar o estado de um semáforo:",
                                "data Semaforo = Verde | Amarelo | Vermelho deriving Show",
                                "testa se o estado dos semaforos de um cruzamento e seguro, i.e., nao ha mais do que semaforo nao vermelho",
                                "interseccaoOK :: [Semaforo] -> Bool"]

                         _  -> ["ERRO!!! Questao nao encontrada"]

-- Dada uma questao, imprime-a em várias linhas
escreve_questao_por_linhas :: [String] -> IO ()
escreve_questao_por_linhas [] = do
                                putStrLn (" ")
escreve_questao_por_linhas (h:t) = do
                                   putStrLn h
                                   escreve_questao_por_linhas t

-- Funcao principal
gera_questao :: IO ()
gera_questao = do
    i <- randomRIO (1,50)
    let questao = associa_questao i
    escreve_questao_por_linhas questao