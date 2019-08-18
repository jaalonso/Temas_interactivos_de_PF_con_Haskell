-- ArbolBusquedaBinariaOptimal.hs
-- Árbol de búsqueda binario optimal.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 22 de Enero de 2011
-- ---------------------------------------------------------------------

module ArbolBusquedaBinariaOptimal where

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Dinamica
import Data.Array

-- ---------------------------------------------------------------------
-- Descripción del problema                                           --
-- ---------------------------------------------------------------------

-- Para cada clave c(i), sea p(i) la probabilidad de acceso de
-- c(i). Entonces, un árbol binario de búsqueda es optimal (ABBO) si la
-- media del número de comparaciones para todas las claves 
--    a(T) = suma(t(i)*p(i)) 
-- donde t(i) es la distancia de la clave c(i) a la raíz (es
-- decir, el número de comparaciones necesarias para llegar a c(i)), es
-- mínima.  

-- ---------------------------------------------------------------------
-- El algoritmo                                                       --
-- ---------------------------------------------------------------------

-- Sea c(i,j) el mínimo valor a(T) cuando el árbol T contiene las claves
-- c(i), ..., c(j).

-- Relación de recurrencia para calcular c(i,j):
--    * Si i>j, entonces c(i,j) = 0,
--    * Si i=j, entonces c(i,j) = p(i),
--    * Si i<j, entonces
--      c(i,j) = min {(c(i,k-1) + sum {p(l) | i <= l <= k-1]) +
--                    (c(k+1,j) + sum {p(l) | k+1 <= l <= j}) +
--                    p(k)
--                    | i <= k <= j} 

-- El tercer caso puede simplificarse
--      c(i,j) = min {c(i,k-1) + c(k+1,j) | i <= k <= j} + 
--               sum {p(l) | i <= l <= j})

-- ---------------------------------------------------------------------
-- Solución mediante programación dinámica                            --
-- ---------------------------------------------------------------------

-- En la matriz de cálculo del ABBO el valor (v,k) correspondiente al
-- índice (i,j) indica que v es el mínimo valor a(T) cuando el árbol T
-- contiene las claves c(i), ..., c(j) y que la división óptima se
-- obtiene dividiendo las claves en dos mediante c(k). 
type Indice = (Int,Int)
type Valor  = (Float,Int)

-- (ABB a) es el tipo de los árboles binarios de búsqueda sobre a.
data ABB a = Vacio
           | Nodo a (ABB a) (ABB a)
  deriving Show

-- (abbo cs ps) es el par formado por un ABBO correspondiente a la lista
-- de claves cs cuyas correspondientes probabilidades de acceso son los
-- elementos de la lista ps y por su valor. Por ejemplo,
--    ghci> abbo ([1,3,4,8,10,11,15],[0.22,0.18,0.20,0.05,0.25,0.02,0.08])
--    (Nodo 4 (Nodo 1 Vacio 
--                    (Nodo 3 Vacio Vacio)) 
--            (Nodo 10 
--                  (Nodo 8 Vacio Vacio) 
--                  (Nodo 15 
--                        (Nodo 11 Vacio Vacio) 
--                        Vacio)),
--    2.15)
abbo :: ([Int], [Float]) -> (ABB Int, Float)
abbo pb = (solucion c t (1,n) , fst (valor t (1,n)))
    where (cs,ps) = pb
          n       = length ps
          c       = listArray (1,n) cs
          p       = listArray (1,n) ps
          t       = dinamica (calcula p) (cotas n) 

-- (calcula p t (i,j)) es el valor del índice (i,j) donde p es el vector
-- de probabilidades y t es la tabla calculada hasta el momento.
calcula :: Array Int Float -> Tabla Indice Valor -> Indice -> Valor
calcula p t (i,j) 
  | i > j     = (0.0,0)
  | i == j    = (p!i,i)
  | otherwise = suma1 (minimum [(fst(valor t (i,k-1)) 
                                + fst(valor t (k+1,j)), k) 
                                | k <- [i..j]])
                      (sumaSegmento i j p)
  where suma1 (x,y) z = (x+z,y)

-- (sumaSegmento i j p) es la suma de los valores de los elementos del
-- vector p desde la posición i a la j. Por ejemplo,
--    ghci> sumaSegmento 2 4 (array (1,5) [(i,fromIntegral i/2) | i <- [1..5]])
--    4.5
sumaSegmento :: Int -> Int -> Array Int Float -> Float
sumaSegmento i j p = sum [p!l | l <- [i..j]]

-- (cotas n) son las cotas de la matriz revesaria para resolver el
-- problema del árbol de búsqueda minimal óptimo con n claves.
cotas :: Int -> ((Int,Int),(Int,Int))
cotas n = ((1,0),(n+1,n))

-- (solucion cs c (i,j)) es el ABBO correspondiente a las claves
-- c(i),...,c(j) a partir de la tabla de cálculo t.
solucion :: Array Int Int -> Tabla Indice Valor -> Indice -> ABB Int
solucion cs t (i,j)
  | i > j     = Vacio
  | i == j    = Nodo c Vacio Vacio
  | otherwise = Nodo c (solucion cs t (i,k-1))
                       (solucion cs t (k+1,j))
  where (_,k) = valor t (i,j)
        c     = cs ! k



