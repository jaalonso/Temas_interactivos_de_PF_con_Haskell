-- RecorridoEnAnchura.hs
-- Recorrido en anchura
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 2 de Noviembre de 2010 (Revisión del 25 de Abril de 2012)
-- ---------------------------------------------------------------------

module RecorridoEnAnchura where

import Data.Ix

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

-- Nota: Elegir una implementación de los grafos.
import GrafoConVectorDeAdyacencia
-- import GrafoConMatrizDeAdyacencia

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
-- Recorrido en anchura con colas                                      --
-- ---------------------------------------------------------------------

-- (recorridoEnAnchura i g) es el recorrido en anchura del grafo g
-- desde el vértice i, usando colas. Por ejemplo, 
--    recorridoEnAnchura 1 ejG  ==  [1,4,3,2,6,5]
recorridoEnAnchura :: (Num p, Ix v) => v -> Grafo v p -> [v]
recorridoEnAnchura i g = reverse (ra [i] [])
  where 
    ra [] vis    = vis
    ra (c:cs) vis 
        | c `elem` vis = ra cs vis
        | otherwise    = ra (cs ++ adyacentes g c) (c:vis)

-- Traza del cálculo de (recorridoEnProfundidad 1 ejG)
--    RecorridoEnAnchura 1 ejG
--    = ra [1]     []
--    = ra [2,3,4] [1]
--    = ra [3,4]   [2,1]
--    = ra [4,6]   [3,2,1]
--    = ra [6]     [4,3,2,1]
--    = ra [2,5]   [6,4,3,2,1]
--    = ra [5]     [6,4,3,2,1]
--    = ra [4]     [5,6,4,3,2,1]
--    = ra []      [5,6,4,3,2,1]
--    = [1,2,3,4,6,5]
