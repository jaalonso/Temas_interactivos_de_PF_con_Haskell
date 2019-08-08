-- AlgoritmoDeKruskal.hs
-- Algoritmo de Kruskal
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 20 de Noviembre de 2010 (Revisión del 25 de Abril de 2012)
-- ---------------------------------------------------------------------

module AlgoritmoDeKruskal where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Seleccionar una implementación del TAD grafo.
import GrafoConVectorDeAdyacencia
-- import GrafoConMatrizDeAdyacencia

-- Nota: Seleccionar una implementación del TAD tabla.
-- import TablaConFunciones
import TablaConListasDeAsociacion
-- import TablaConMatrices

import Data.List
import Data.Ix

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

g1 :: Grafo Int Int    
g1 = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                        (2,4,55),(2,5,32),
                        (3,4,61),(3,5,44),
                        (4,5,93)]

g2 :: Grafo Int Int    
g2 = creaGrafo D (1,5) [(1,2,13),(1,3,11),(1,5,78),
                        (2,4,12),(2,5,32),
                        (3,4,14),(3,5,44),
                        (4,5,93)]

-- ---------------------------------------------------------------------
-- Algoritmo de Kruskal                                               --
-- ---------------------------------------------------------------------

-- (kruskal g) es el árbol de expansión mínimo del grafo g calculado
-- mediante el algoritmo de Kruskal. Por ejemplo,
--    kruskal g1  ==  [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
--    kruskal g2  ==  [(32,2,5),(13,1,2),(12,2,4),(11,1,3)]
kruskal :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)]
kruskal g = kruskal' cola                           -- Cola de prioridad
                     (tabla [(x,x) | x <- nodos g]) -- Tabla de raices
                     []                             -- Árbol de expansión
                     (length (nodos g) - 1)         -- Aristas por
                                                    -- colocar
  where
    cola = sort [(p,x,y) | (x,y,p) <- aristas g]
    kruskal' ((p,x,y):as) t ae n 
      | n==0        = ae
      | actualizado = kruskal' as t' ((p,x,y):ae) (n-1)
      | otherwise   = kruskal' as t  ae           n
      where (actualizado,t') = buscaActualiza (x,y) t

-- (raiz t n) es la raíz de n en la tabla t. Por ejemplo,
--    raiz (tabla [(1,1),(3,1),(4,3),(5,4),(2,6),(6,6)]) 5  == 1
--    raiz (tabla [(1,1),(3,1),(4,3),(5,4),(2,6),(6,6)]) 2  == 6
raiz:: Eq n => Tabla n n -> n -> n
raiz t x | v == x    = v
         | otherwise = raiz t v
  where v = valor t x

-- (buscaActualiza a t) es el par formado por False y la tabla t, si los
-- dos vértices de la arista a tienen la misma raíz en t y el par
-- formado por True y la tabla obtenida añadiéndole a t la arista
-- formada por el vértice de a de mayor raíz y la raíz del vértice de
-- a de menor raíz. Por ejemplo,
--    ghci> let t = tabla [(1,1),(2,2),(3,1),(4,1)]
--    ghci> buscaActualiza (2,3) t
--    (True,Tbl [(1,1),(2,1),(3,1),(4,1)])
--    ghci> buscaActualiza (3,4) t
--    (False,Tbl [(1,1),(2,2),(3,1),(4,1)])
buscaActualiza :: (Eq n, Ord n) => (n,n) -> Tabla n n -> (Bool,Tabla n n)
buscaActualiza (x,y) t 
  | x' == y'  = (False, t) 
  | y' <  x'  = (True, modifica (x,y') t)
  | otherwise = (True, modifica (y,x') t)
  where x' = raiz t x 
        y' = raiz t y

-- ---------------------------------------------------------------------
-- El algoritmo de Prim                                                  --
-- ---------------------------------------------------------------------

-- (prim g) es el árbol de expansión mínimo del grafo g calculado
-- mediante el algoritmo de Prim. Por ejemplo,
--    prim g1  ==  [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
--    prim g2  ==  [(32,2,5),(12,2,4),(13,1,2),(11,1,3)]
prim :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)]
prim g = prim' [n]              -- Nodos colocados
               ns               -- Nodos por colocar 
               []               -- Árbol de expansión
               (aristas g)      -- Aristas del grafo
  where
    (n:ns) = nodos g
    prim' _ [] ae _  = ae
    prim' t r  ae as = prim' (v':t) (delete v' r) (e:ae) as
      where e@(_,_, v') = minimum [(c,u,v)| (u,v,c) <- as,
                                             elem u t, 
                                             elem v r]
