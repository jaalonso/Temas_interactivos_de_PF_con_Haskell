-- Escalada_Monedas.hs
-- Problema de las monedas por búsqueda en escalada.
-- José A. Alonso Jiménez
-- Sevilla,  9 de agosto de 2019
-- ---------------------------------------------------------------------

module Escalada_Monedas where

import BusquedaEnEscalada

-- El problema del cambio de monedas consiste en determinar cómo
-- conseguir una cantidad usando el menor número de monedas disponibles. 

-- Las monedas son números enteros.
type Moneda = Int

-- monedas es la lista del tipo de monedas disponibles. Se supone que
-- hay un número infinito de monedas de cada tipo.
monedas :: [Moneda]
monedas = [1,2,5,10,20,50,100]

-- Las soluciones son listas de monedas.
type Soluciones = [Moneda]

-- Los estados son pares formados por la cantidad que falta y la lista
-- de monedas usadas.
type NodoMonedas = (Int, [Moneda])

-- (sucesoresMonedas e) es la lista de los sucesores del estado e en el
-- problema de las monedas. Por ejemplo,
--   ghci> sucesoresMonedas (199,[])
--   [(198,[1]),(197,[2]),(194,[5]),(189,[10]),
--    (179,[20]),(149,[50]),(99,[100])]
sucesoresMonedas :: NodoMonedas -> [NodoMonedas]
sucesoresMonedas (r,p) = 
  [(r-c,c:p) | c <- monedas, r-c >= 0]

-- (esFinalMonedas e) se verifica si e es un estado final del problema
-- de las monedas.
esFinalMonedas :: NodoMonedas -> Bool
esFinalMonedas (v,_) = v==0

-- (cambio n) es la solución del problema de las monedas por búsqueda en
-- escalada. Por ejemplo,
--    cambio 199  ==  [2,2,5,20,20,50,100]
cambio :: Int -> Soluciones
cambio n = 
  snd (head (buscaEscalada sucesoresMonedas 
                           esFinalMonedas 
                           (n,[])))

