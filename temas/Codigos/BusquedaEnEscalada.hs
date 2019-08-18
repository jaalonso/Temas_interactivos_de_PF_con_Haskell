-- BusquedaEnEscalada.hs
-- Búsqueda en escalada.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Noviembre de 2010
-- ---------------------------------------------------------------------

module BusquedaEnEscalada where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las colas de prioridad y
-- otra de grafos.

-- Implementaciones de colas de prioridad:
-- import ColaDePrioridadConListas
import ColaDePrioridadConMonticulos

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
buscaEscalada sucesores esFinal x = busca' (inserta x vacia) where
  busca' c  
    | esVacia c           = [] 
    | esFinal (primero c) = [primero c]
    | otherwise           = busca' (foldr inserta vacia (sucesores (primero c)))
