-- BEE_Mochila.hs
-- El problema de la mochila mediante BEE.
-- José A. Alonso Jiménez
-- Sevilla,  8 de agosto de 2019
-- ---------------------------------------------------------------------

module BEE_Mochila where

import BusquedaEnEspaciosDeEstados
import Data.List (sort)

-- Se tiene una mochila de capacidad de peso p y una lista de n objetos
-- para colocar en la mochila. Cada objeto i tiene un peso w_i y un
-- valor v_i. Considerando la posibilidad de colocar el mismo objeto
-- varias veces en la mochila, el problema consiste en determinar la
-- forma de colocar los objetos en la mochila sin sobrepasar la
-- capacidad de la mochila colocando el máximmo valor posible.

-- Los pesos son número enteros
type Peso = Int

-- Los valores son números reales.
type Valor = Float

-- Los objetos son pares formado por un peso y un valor
type Objeto = (Peso,Valor)

-- Una solución del problema de la mochila es una lista de objetos.
type SolMoch = [Objeto]

-- Los estados del problema de la mochila son 5-tupla de la forma
-- (v,p,l,o,s) donde v es el valor de los objetos colocados, p es el
-- peso de los objetos colocados, l es el límite de la capacidad de la
-- mochila, o es la lista de los objetos colocados (ordenados de forma
-- creciente según sus pesos) y s es la solución parcial.
type NodoMoch = (Valor,Peso,Peso,[Objeto],SolMoch)


-- (sucesoresMoch e) es la lista de los sucesores del estado e en el
-- problema de la mochila.
sucesoresMoch :: NodoMoch -> [NodoMoch]
sucesoresMoch (v,p,limite,objetos,solp) =
  [( v+v',
     p+p',
     limite,
     [o | o@(p'',_) <- objetos, p''>=p'], 
     (p',v'):solp )
  | (p',v') <- objetos, 
    p+p' <= limite]

-- (esObjetivoMoch e) se verifica si e es un estado final el problema de
-- la mochila.
esObjetivoMoch :: NodoMoch -> Bool
esObjetivoMoch (_,p,limite,((p',_):_),_) = p+p'>limite

-- (buscaEE_Mochila os l) es la solución del problema de la mochila para
-- la lista de objetos os y el límite de capacidad l. Por ejemplo,
--    > buscaEE_Mochila [(2,3),(3,5),(4,6),(5,10)] 8
--    ([(5,10.0),(3,5.0)],15.0)
--    > buscaEE_Mochila [(2,3),(3,5),(5,6)] 10
--    ([(3,5.0),(3,5.0),(2,3.0),(2,3.0)],16.0)
--    > buscaEE_Mochila [(8,15),(15,10),(3,6),(6,13),(2,4),(4,8),(5,6),(7,7)] 35
--    ([(6,13.0),(6,13.0),(6,13.0),(6,13.0),(6,13.0),(3,6.0),(2,4.0)],75.0)
--    > buscaEE_Mochila [(2,2.8),(3,4.4),(5,6.1)] 10
--    ([(3,4.4),(3,4.4),(2,2.8),(2,2.8)],14.4)
buscaEE_Mochila :: [Objeto] -> Peso -> (SolMoch,Valor)
buscaEE_Mochila objetos limite = (sol,v) 
  where 
    (v,_,_,_,sol) = 
      maximum (buscaEE sucesoresMoch 
                       esObjetivoMoch  
                       (0,0,limite,sort objetos,[]))

mochila :: [Objeto] -> Peso -> [(SolMoch, Valor)]
mochila objetos limite = 
  [(sol,v) | (v,w,_,_,sol) <- buscaEE sucesoresMoch 
                                      esObjetivoMoch  
                                      (0,0,limite,sort objetos,[]),
                              (w==10) || (w==9) ||(w==8) ]
