-- Escalada_Prim.hs
-- El algoritmo de Prim del árbol de expansión mínimo por escalada
-- José A. Alonso Jiménez
-- Sevilla,  9 de agosto de 2019
-- ---------------------------------------------------------------------

module Escalada_Prim where

import BusquedaEnEscalada

-- Nota: Hay que elegir una implementación de grafos.

-- Implementaciones del TAD grafo.
-- import GrafoConVectorDeAdyacencia
import GrafoConMatrizDeAdyacencia

import Data.Array
import Data.List

g1 :: Grafo Int Int    
g1 = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                         (2,4,55),(2,5,32),
                         (3,4,61),(3,5,44),
                         (4,5,93)]

-- Una arista esta formada dos nodos junto con su peso.
type Arista a b = (a,a,b)

-- Un nodo (NodoAEM (p,t,r,aem)) está formado por el peso p de la última
-- arista añadida el árbol de expansión mínimo (aem), la lista t
-- de nodos del grafo que están en el aem, la lista r de nodos del
-- grafo que no están en el aem y el aem. 
type NodoAEM a b = (b,[a],[a],[Arista a b])

-- (sucesoresAEM g n) es la lista de los sucesores del nodo n en el
-- grafo g. Por ejemplo,
--    ghci> sucesoresAEM g1 (0,[1],[2..5],[])
--    [(12,[2,1],[3,4,5],[(1,2,12)]),
--     (34,[3,1],[2,4,5],[(1,3,34)]),
--     (78,[5,1],[2,3,4],[(1,5,78)])]
sucesoresAEM
  :: (Ix a, Num b, Eq b) => Grafo a b -> NodoAEM a b -> [NodoAEM a b]
sucesoresAEM g (_,t,r,aem)
        = [(peso x y g, y:t, delete y r, (x,y,peso x y g):aem)
           | x <- t , y <- r, aristaEn g (x,y)]

-- (esFinalAEM n) se verifica si n es un estado final; es decir, si no
-- queda ningún elemento en la lista de nodos sin colocar en el árbol de
-- expansión mínimo.
esFinalAEM :: NodoAEM a b -> Bool
esFinalAEM (_,_,[],_) = True
esFinalAEM _          = False

-- (prim g) es el árbol de expansión mínimo del grafo g, por el
-- algoritmo de Prim como búsqueda en escalada. Por ejemplo,
--    prim g1 == [(2,4,55),(1,3,34),(2,5,32),(1,2,12)]
prim :: (Ix a, Num b, Ord b) => Grafo a b -> [Arista a b]
prim g = sol
  where [(_,_,_,sol)] = buscaEscalada (sucesoresAEM g) 
                                      esFinalAEM
                                      (0,[n],ns,[])
        (n:ns) = nodos g
