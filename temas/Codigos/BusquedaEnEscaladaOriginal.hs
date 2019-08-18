-- BusquedaEnEscalada.hs
-- Búsqueda en escalada.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Noviembre de 2010
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las colas de prioridad y
-- otra de grafos.

-- Implementaciones de colas de prioridad:
-- import ColaDePrioridadConListas
import ColaDePrioridadConMonticulos

-- Implementaciones del TAD grafo.
-- import GrafoConVectorDeAdyacencia
import GrafoConMatrizDeAdyacencia

import Data.Array
import Data.List

-- ---------------------------------------------------------------------
-- El patrón de búsqueda en escalada                                  --
-- ---------------------------------------------------------------------

-- (buscaEscalada s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e), obtenidas buscando por escalada.
buscaEscalada :: Ord nodo => 
                 (nodo -> [nodo])   -- sucesores
                 -> (nodo -> Bool)  -- es final
                 -> nodo            -- nodo actual
                 -> [nodo]          -- soluciones
buscaEscalada sucesores esFinal x = (busca' (inserta x vacia) )
    where
      busca' c  
          | esVacia c           = [] 
          | esFinal (primero c) = [primero c]
          | otherwise           = 
              busca' (foldr inserta vacia (sucesores x))
              where x = primero c

-- ---------------------------------------------------------------------
-- El problema del cambio de monedas                                  --
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Algoritmo de Prim del árbol de expansión mínimo                    --
-- ---------------------------------------------------------------------

g1 :: Grafo Int Int    
g1 = creaGrafo True (1,5) [(1,2,12),(1,3,34),(1,5,78),
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
sucesoresAEM :: (Ix a,Num b) => (Grafo a b) -> (NodoAEM a b)
                           -> [(NodoAEM a b)]
sucesoresAEM g (_,t,r,aem)
        = [(peso x y g, (y:t), delete y r, (x,y,peso x y g):aem)
           | x <- t , y <- r, aristaEn g (x,y)]

-- (esFinalAEM n) se verifica si n es un estado final; es decir, si no
-- queda ningún elemento en la lista de nodos sin colocar en el árbol de
-- expansión mínimo.
esFinalAEM (_,_,[],_) = True
esFinalAEM _          = False

-- (prim g) es el árbol de expansión mínimo del grafo g, por el
-- algoritmo de Prim como búsqueda en escalada. Por ejemplo,
--    prim g1 == [(2,4,55),(1,3,34),(2,5,32),(1,2,12)]
prim g = sol
    where [(_,_,_,sol)] = buscaEscalada (sucesoresAEM g) 
                                        esFinalAEM
                                        (0,[n],ns,[])
          (n:ns) = nodos g
