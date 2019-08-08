-- RecorridoEnProfundidad.hs
-- Recorrido en profundidad
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 24 de Octubre de 2010 (Revisión del 25 de Abril de 2012)
-- ---------------------------------------------------------------------

module RecorridoEnProfundidad where

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

-- Nota: Elegir una implementación de los grafos.
import GrafoConVectorDeAdyacencia
-- import GrafoConMatrizDeAdyacencia

import Data.Ix

-- ---------------------------------------------------------------------
-- Ejemplo de grafo                                                   --
-- ---------------------------------------------------------------------

-- g es el grafo
--    +---> 2 <---+
--    |           |
--    |           |
--    1 --> 3 --> 6 --> 5
--    |                 |
--    |                 |
--    +---> 4 <---------+

ejG :: Grafo Integer Integer
ejG = creaGrafo D (1,6) 
                  [(1,2,0),(1,3,0),(1,4,0),(3,6,0),(5,4,0),(6,2,0),(6,5,0)]

-- ---------------------------------------------------------------------
-- Recorrido en profundidad                                            --
-- ---------------------------------------------------------------------

-- (recorridoEnProfundidad i g) es el recorrido en profundidad del grafo g
-- desde el vértice i. Por ejemplo,
--    recorridoEnProfundidad 1 ejG  ==  [1,2,3,6,5,4]
recorridoEnProfundidad :: (Num p, Ix v) => v -> Grafo v p -> [v]
recorridoEnProfundidad i g = rp [i] []
  where 
    rp [] vis    = vis
    rp (c:cs) vis 
        | c `elem` vis = rp cs vis
        | otherwise    = rp (adyacentes g c ++ cs) (vis ++ [c])

-- Traza del cálculo de (recorridoEnProfundidad 1 ejG)
--    recorridoEnProfundidad 1 ejG
--    = rp [1]     []
--    = rp [2,3,4] [1]
--    = rp [3,4]   [1,2]
--    = rp [6,4]   [1,2,3]
--    = rp [2,5,4] [1,2,3,6]
--    = rp [5,4]   [1,2,3,6]
--    = rp [4,4]   [1,2,3,6,5]
--    = rp [4]     [1,2,3,6,5,4]
--    = rp []      [1,2,3,6,5,4]
--    = [1,2,3,6,5,4]

-- ---------------------------------------------------------------------
-- Recorrido en profundidad con acumuladores                           --
-- ---------------------------------------------------------------------

-- (recorridoEnProfundidad' i g) es el recorrido en profundidad del
-- grafo g desde el vértice i, usando la lista de los visitados como
-- acumulador. Por ejemplo, 
--    recorridoEnProfundidad' 1 ejG  ==  [1,2,3,6,5,4]
recorridoEnProfundidad' :: (Num p, Ix v) => v -> Grafo v p -> [v]
recorridoEnProfundidad' i g = reverse (rp [i] [])
  where
    rp [] vis     = vis
    rp (c:cs) vis 
        | c `elem` vis = rp cs vis
        | otherwise    = rp (adyacentes g c ++ cs) (c:vis)

-- Traza del cálculo de (recorridoEnProfundidad' 1 ejG)
--    RecorridoEnProfundidad' 1 ejG
--    = reverse (rp [1]     [])
--    = reverse (rp [2,3,4] [1])
--    = reverse (rp [3,4]   [2,1])
--    = reverse (rp [6,4]   [3,2,1])
--    = reverse (rp [2,5,4] [6,3,2,1])
--    = reverse (rp [5,4]   [6,3,2,1])
--    = reverse (rp [4,4]   [5,6,3,2,1])
--    = reverse (rp [4]     [4,5,6,3,2,1])
--    = reverse (rp []      [4,5,6,3,2,1])
--    = reverse [4,5,6,3,2,1]
--    = [1,2,3,6,5,4]
