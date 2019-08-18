-- ProblemaDelViajante.hs
-- Problema del viajante.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 22 de Enero de 2011
-- ---------------------------------------------------------------------

module ProblemaDelViajante where

-- ---------------------------------------------------------------------
-- Descripción del problema                                           --
-- ---------------------------------------------------------------------

-- Dado un grafo no dirigido con pesos encontrar una camino en el grafo
-- que visite todos los nodos exactamente una vez y cuyo coste sea
-- mínimo. 

-- Notaciones:
-- + Los vértices del grafo son 1,2,...,n.
-- + p(i,j) es el peso del arco que une i con j. Se supone que p(i,j)=0,
--   si i=j y que p(i,j)=infinito si no hay ningún arco que una i con
--   j. 
-- + El vértice inicial y final es el n.
-- + c(i,S) es el camino más corto que comienza en i, termina en n y
--   pasa exactamente una vez por cada uno de los vértices del conjunto
--   S. 

-- Relación de recurrencia:
-- + c(i,vacio) = p(i,n), si i != n
-- + c(i,S) = min {p(i,j)+c(j,S-{j} |j en S}, si i != n, i no en S.

-- La solución es c(n,{1,...,n-1}.

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Dinamica

-- Nota: Elegir una implementación de los grafos.
import GrafoConVectorDeAdyacencia
-- import GrafoConMatrizDeAdyacencia

-- ---------------------------------------------------------------------
-- Implementación de conjuntos de enteros como números enteros        --
-- ---------------------------------------------------------------------

-- Los conjuntos se representan por números enteros.
type Conj = Int

-- (conj2Lista c) es la lista de los elementos del conjunto c. Por
-- ejemplo, 
--   conj2Lista 24  ==  [3,4]
--   conj2Lista 30  ==  [1,2,3,4]
--   conj2Lista 22  ==  [1,2,4]
conj2Lista :: Conj -> [Int]
conj2Lista s = c2l s 0
  where 
    c2l 0 _             = []
    c2l n i | odd n     = i : c2l (n `div` 2) (i+1)
            | otherwise = c2l (n `div` 2) (i+1)

-- maxConj es el máximo número que puede pertenecer al conjunto. Depende
-- de la implementación de Haskell. Por ejemplo,
--    maxConj  ==  29
maxConj :: Int
maxConj = 
  truncate (logBase 2 (fromIntegral maxInt)) - 1
  where maxInt = maxBound::Int

-- vacio es el conjunto vacío.
vacio :: Conj
vacio = 0

-- (esVacio c) se verifica si c es el conjunto vacío.
esVacio :: Conj -> Bool
esVacio n = n == 0

-- (conjCompleto n) es el conjunto de los números desde 1 hasta n.
conjCompleto :: Int -> Conj
conjCompleto n
  | (n>=0) && (n<=maxConj) = 2^(n+1)-2
  | otherwise = error ("conjCompleto:" ++ show n)

-- (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. 
inserta :: Int -> Conj -> Conj
inserta i s
  | (i>=0) && (i<=maxConj) = d'*e+m
  | otherwise              = error ("inserta: elemento ilegal =" ++
                                    show i)
  where (d,m) = divMod s e
        e     = 2^i
        d'    = if odd d then d else d+1

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. 
elimina :: Int -> Conj -> Conj
elimina i s = d'*e+m
  where (d,m) = divMod s e
        e = 2^i
        d' = if odd d then d-1 else d

-- ---------------------------------------------------------------------
-- Ejemplos de grafos                                                 --
-- ---------------------------------------------------------------------

-- Se usarán los siguientes ejemplos de grafos, generados a partir de la
-- lista ded adyaciencia, donde se ha puesto un peso de 100 entre los
-- nodos que no están unidos por un arco.

-- e1 es el grafo (de la página 192):
-- 
--       4       5
--    +----- 2 -----+
--    |      |1     |
--    |  1   |   8  |
--    1----- 3 -----5
--    |        \2  /
--    |  6     2\ /5
--    +----- 4 --6
-- 
ej1 :: Grafo Int Int
ej1 = creaGrafo ND (1,6) [(i,j,(v1!!(i-1))!!(j-1)) |i<-[1..6],j<-[1..6]]
v1 :: [[Int]]
v1 = [[  0,  4,  1,  6,100,100],
      [  4,  0,  1,100,  5,100],
      [  1,  1,  0,100,  8,  2],
      [  6,100,100,  0,100,  2],
      [100,  5,  8,100,  0,  5],
      [100,100,  2,  2,  5,  0]]

ej2 :: Grafo Int Int
ej2 = creaGrafo ND (1,6) [(i,j,(v'!!(i-1))!!(j-1)) |i<-[1..6],j<-[1..6]]
v' :: [[Int]]
v' =  [[  0,  3, 10, 11,  7, 25],
       [  3,  0,  6, 12,  8, 26],
       [ 10,  6,  0,  9,  4, 20],
       [ 11, 12,  9,  0,  5, 15],
       [  7,  8,  4,  5,  0, 18],
       [ 25, 26, 20, 15, 18,  0]]

-- ---------------------------------------------------------------------
-- Solución mediante programación dinámica                            --
-- ---------------------------------------------------------------------

-- Los índices de la matriz de cálculo son de la forma (i,S) y sus
-- valores (v,xs) donde xs es el camino mínimo desde i hasta n visitando
-- cada vértice de S exactamente una vez y v es el coste de xs.
type IndicePV = (Int,Conj)
type ValorPV  = (Int,[Int])

-- (viajante g) es el par (v,xs) donde xs es el camino de menor coste
-- que pasa exactamente una vez por todos los nodos del grafo g
-- empezando en su último nodo y v es su coste. Por ejemplo,
--    ghci> viajante ej1
--    (20,[6,4,1,3,2,5,6])
--    ghci> viajante ej2
--    (56,[6,3,2,1,5,4,6])
viajante :: Grafo Int Int -> (Int,[Int])
viajante g = valor t (n,conjCompleto (n-1))
  where n = length (nodos g)
        t = dinamica (calculaPV g n) (cotasPV n)

-- (calculaPV g n t (i,k)) es el valor del camino mínimo en el grafo g
-- desde i hasta n visitando cada nodo del conjunto k exactamente una
-- vez calculado usando la tabla t.
calculaPV :: Grafo Int Int -> Int -> Tabla IndicePV ValorPV 
           -> IndicePV -> ValorPV 
calculaPV g n t (i,k) 
  | esVacio k = (peso i n g,[i,n])
  | otherwise = minimum [sumaPrim (valor t (j, elimina j k))
                                  (peso i j g)
                        | j <- conj2Lista k]
  where sumaPrim (v,xs) v' = (v+v',i:xs)

-- (cotasPV n) son las cotas de la matriz de cálculo del problema del
-- viajante en un grafo con n nodos.
cotasPV :: Int -> ((Int,Conj),(Int,Conj))
cotasPV n = ((1,vacio),(n,conjCompleto n))

ejCalculo = [(i,valor t (i,conjCompleto (i-1))) | i <- [1..6]]
  where t = dinamica (calculaPV ej1 6) (cotasPV 6)
