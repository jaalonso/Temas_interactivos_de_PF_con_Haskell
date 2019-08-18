-- BusquedaEnEspaciosDeEstados.hs
-- Búsqueda en espacios de estados.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Noviembre de 2010
-- ---------------------------------------------------------------------

module BusquedaEnEspaciosDeEstados (buscaEE) where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las pilas.
import PilaConListas
-- import PilaConTipoDeDatoAlgebraico

import Data.Array
import Data.List (sort)

-- ---------------------------------------------------------------------
-- Descripción de los problemas de espacios de estados                --
-- ---------------------------------------------------------------------

-- Las características de los problemas de espacios de estados son:
-- + un conjunto de las posibles situaciones o nodos que constituye
--   el espacio de estados; estos son las potenciales soluciones que se
--   necesitan explorar;
-- + un conjunto de movimientos de un nodo a otros nodos, llamados los
--   sucesores del nodo; 
-- + un nodo inicial;
-- + un nodo objetivo, que es la solución.

-- ---------------------------------------------------------------------
-- El problema del 8 puzzle                                           --
-- ---------------------------------------------------------------------

-- Para el 8-puzzle se usa un cajón cuadrado en el que hay situados 8 bloques
-- cuadrados.  El cuadrado restante está sin rellenar. Cada bloque tiene un
-- número. Un bloque adyacente al hueco puede deslizarse hacia él. El juego
-- consiste en transformar la posición inicial en la posición final mediante
-- el deslizamiento de los bloques.  En particular, consideramos el estado
-- inicial y final siguientes:
--
--      +---+---+---+                   +---+---+---+
--      | 2 | 6 | 3 |                   | 1 | 2 | 3 | 
--      +---+---+---+                   +---+---+---+ 
--      | 5 |   | 4 |                   | 8 |   | 4 | 
--      +---+---+---+                   +---+---+---+ 
--      | 1 | 7 | 8 |                   | 7 | 6 | 5 | 
--      +---+---+---+                   +---+---+---+ 
--                      
--      Estado inicial                  Estado final

-- Una posición es un par de enteros.
type Posicion = (Int,Int)

-- Un tablero es un vector de posiciones, en el que el índice indica el
-- elemento que ocupa la posición.
type Tablero  = Array Int Posicion

-- inicial8P es el estado inicial del 8 puzzle. En el ejemplo es
--      +---+---+---+
--      | 2 | 6 | 3 | 
--      +---+---+---+ 
--      | 5 |   | 4 | 
--      +---+---+---+ 
--      | 1 | 7 | 8 | 
--      +---+---+---+ 
inicial8P :: Tablero 
inicial8P = array (0,8) [(2,(1,3)),(6,(2,3)),(3,(3,3)),
                         (5,(1,2)),(0,(2,2)),(4,(3,2)),
                         (1,(1,1)),(7,(2,1)),(8,(3,1))]

-- final8P es el estado final del 8 puzzle. En el ejemplo es
--      +---+---+---+
--      | 1 | 2 | 3 | 
--      +---+---+---+ 
--      | 8 |   | 4 | 
--      +---+---+---+ 
--      | 7 | 6 | 5 | 
--      +---+---+---+ 
final8P :: Tablero
final8P = array (0,8) [(1,(1,3)),(2,(2,3)),(3,(3,3)),
                       (8,(1,2)),(0,(2,2)),(4,(3,2)),
                       (7,(1,1)),(6,(2,1)),(5,(3,1))]

-- (distancia p1 p2) es la distancia Manhatan entre las posiciones p1 y
-- p2. Por ejemplo,
--    distancia (2,7) (4,1)  ==  8
distancia :: Posicion -> Posicion -> Int
distancia (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- (adyacente p1 p2) se verifica si las posiciones p1 y p2 son
-- adyacentes. Por ejemplo,
--    adyacente (3,2) (3,1)  ==  True
--    adyacente (3,2) (1,2)  ==  False
adyacente :: Posicion -> Posicion -> Bool
adyacente p1 p2 = distancia p1 p2 == 1

-- (todosMovimientos t) es la lista de los tableros obtenidos
-- aplicándole al tablero t todos los posibles movimientos; es decir,
-- intercambiando la posición del hueco con sus adyacentes. Por ejemplo, 
--    ghci> inicial8P
--    array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                 (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]
--    ghci> todosMovimientos inicial8P
--    [array (0,8) [(0,(3,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(2,2)),
--                  (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(1,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(2,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(2,3)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(1,2)),(6,(2,2)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(2,1)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(1,2)),(6,(2,3)),(7,(2,2)),(8,(3,1))]]
todosMovimientos :: Tablero -> [Tablero]
todosMovimientos t = 
    [t//[(0,t!i),(i,t!0)] | i<-[1..8], adyacente (t!0) (t!i)] 

-- Los nodos del espacio de estados son listas de tableros [t_n,...,t_1]
-- tal que t_i es un sucesor de t_(i-1).
data Tableros = Est [Tablero] deriving (Eq, Show)

-- (sucesores8P e) es la lista de sucesores del estado e. Por ejemplo,
--    ghci> sucesores8P (Est [inicial8P])
--    [Est [array (0,8) [(0,(3,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(2,2)),
--                       (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--          array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]],
--    Est [array (0,8) [(0,(1,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(2,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--         array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]],
--    Est [array (0,8) [(0,(2,3)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,2)),(7,(2,1)),(8,(3,1))],
--         array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]],
--    Est [array (0,8) [(0,(2,1)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,2)),(8,(3,1))],
--         array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]]]
sucesores8P :: Tableros -> [Tableros]
sucesores8P (Est(n@(t:ts))) = 
    filter (noEn ts) [ Est (t':n) | t' <- todosMovimientos t]
    where noEn ts (Est(t:_)) = not (elem (elems t) (map elems ts))

-- (esFinal8P e) se verifica si e es un estado final del 8 puzzle.
esFinal8P :: Tableros -> Bool
esFinal8P (Est (n:_)) = elems n == elems final8P

-- (buscaEE8P) es la lista de las soluciones del problema del 8
-- puzzle. Por ejemplo,
--    ghci> buscaEE8P
--      C-c C-cInterrupted.
-- No termina.
buscaEE8P :: [[Posicion]]
buscaEE8P = map elems ls
    where ((Est ls):_) = buscaEE sucesores8P esFinal8P (Est [inicial8P])

