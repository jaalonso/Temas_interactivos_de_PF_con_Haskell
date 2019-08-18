-- BEE_Reinas.hs
-- El problema de las n reinas mediante BEE.
-- José A. Alonso Jiménez
-- Sevilla,  8 de agosto de 2019
-- ---------------------------------------------------------------------

module BEE_Reinas where

import BusquedaEnEspaciosDeEstados

-- El problema de las n reinas consiste en colocar n reinas en un
-- tablero cuadrado de dimensiones n por n de forma que no se encuentren
-- más de una en la misma línea: horizontal, vertical o diagonal.

-- Las posiciones de las reinas en el tablero se representan por su
-- columna y su fila.
type Columna = Int
type Fila    = Int

-- Una solución del problema de las n reinas es una lista de
-- posiciones. 
type SolNR = [(Columna,Fila)]

-- (valida sp p) se verifica si la posición p es válida respecto de la
-- solución parcial sp; es decir, la reina en la posición p no amenaza a
-- ninguna de las reinas de la sp (se supone que están en distintas
-- columnas). Por ejemplo, 
--    valida [(1,1)] (2,2)  ==  False
--    valida [(1,1)] (2,3)  ==  True
valida :: SolNR -> (Columna,Fila) -> Bool
valida solp (c,r) = and [test s | s <- solp]
  where test (c',r') = c'+r'/=c+r && c'-r'/=c-r && r'/=r

-- Los nodos del problema de las n reinas son ternas formadas por la
-- columna de la última reina colocada, el número de columnas del
-- tablero y la solución parcial de las reinas colocadas anteriormente. 
type NodoNR = (Columna,Columna,SolNR)

-- (sucesoresNR e) es la lista de los sucesores del estado e en el
-- problema de las n reinas. Por ejemplo,
--    ghci> sucesoresNR (1,4,[])
--    [(2,4,[(1,1)]),(2,4,[(1,2)]),(2,4,[(1,3)]),(2,4,[(1,4)])]
sucesoresNR :: NodoNR -> [NodoNR]
sucesoresNR (c,n,solp) =
  [(c+1,n,solp++[(c,r)]) | r <- [1..n] , valida solp (c,r)]

-- (esFinalNR e) se verifica si e es un estado final del problema de las
-- n reinas. 
esFinalNR :: NodoNR -> Bool
esFinalNR (c,n,solp) = c > n

-- (buscaEE_NR n) es la primera solución del problema de las n reinas,
-- por búsqueda en espacio de estados. Por ejemplo,
--    ghci> buscaEE_NR 8
--    [(1,1),(2,5),(3,8),(4,6),(5,3),(6,7),(7,2),(8,4)]
buscaEE_NR :: Columna -> SolNR
buscaEE_NR n = s
  where ((_,_,s):_) = buscaEE sucesoresNR esFinalNR (1,n,[])

-- (nSolucionesNR n) es el número de soluciones del problema de las n
-- reinas, por búsqueda en espacio de estados. Por ejemplo,  
--    nSolucionesNR 8  ==  92
nSolucionesNR :: Columna -> Int
nSolucionesNR n = 
  length (buscaEE sucesoresNR 
                  esFinalNR 
                  (1,n,[]))

