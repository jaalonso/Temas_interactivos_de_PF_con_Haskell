-- ColaDePrioridadConListas.hs
-- Implementación de las colas de prioridad mediante listas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

module ColaDePrioridadConListas
    (CPrioridad,
     vacia,   -- Ord a => CPrioridad a 
     inserta, -- Ord a => a -> CPrioridad a -> CPrioridad a 
     primero, -- Ord a => CPrioridad a -> a
     resto,   -- Ord a => CPrioridad a -> CPrioridad a
     esVacia, -- Ord a => CPrioridad a -> Bool 
     valida   -- Ord a => CPrioridad a -> Bool
    ) where

-- Colas de prioridad mediante listas.
newtype CPrioridad a = CP [a]
  deriving (Eq, Show)

-- Ejemplo de cola de prioridad
--    ghci> cp1
--    CP [1,2,3,7,9]
cp1 :: CPrioridad Int
cp1 = foldr inserta vacia [3,1,7,2,9]

-- (valida c) se verifica si c es una cola de prioridad válida. Por
-- ejemplo, 
--    valida (CP [1,3,5])  ==  True
--    valida (CP [1,5,3])  ==  False
valida :: Ord a => CPrioridad a -> Bool
valida (CP xs) = ordenada xs
  where ordenada (x:y:zs) = x <= y && ordenada (y:zs)
        ordenada _        = True

-- vacia es la cola de prioridad vacía. Por ejemplo,
--    vacia  ==  CP []
vacia :: Ord a => CPrioridad a 
vacia = CP []

-- (inserta x c) es la cola obtenida añadiendo el elemento x a la cola
-- de prioridad c. Por ejemplo,  
--    cp1            ==  CP [1,2,3,7,9]
--    inserta 5 cp1  ==  CP [1,2,3,5,7,9]
inserta :: Ord a => a -> CPrioridad a -> CPrioridad a 
inserta x (CP q) = CP (ins x q)
  where ins y []                   = [y]
        ins y r@(e:r') | y < e     = y:r
                       | otherwise = e:ins y r'

-- Nota. inserta usa O(n) pasos.

-- (primero c) es el primer elemento de la cola de prioridad c. Por
-- ejemplo, 
--    cp1          ==  CP [1,2,3,7,9]
--    primero cp1  ==  1
primero :: Ord a => CPrioridad a -> a
primero (CP(x:_)) = x
primero _         = error "primero: cola de prioridad vacia"

-- (resto c) es la cola de prioridad obtenida eliminando el primer
-- elemento de la cola de prioridad c. Por ejemplo,  
--    cp1        ==  CP [1,2,3,7,9]
--    resto cp1  ==  CP [2,3,7,9]
resto :: Ord a => CPrioridad a -> CPrioridad a
resto (CP (_:xs)) = CP xs
resto _           = error "resto: cola de prioridad vacia"

-- Nota. resto usa O(1) pasos.

-- (esVacia c) se verifica si la cola de prioridad c es vacía. Por
-- ejemplo,   
--    esVacia cp1    ==  False
--    esVacia vacia  ==  True
esVacia :: Ord a => CPrioridad a -> Bool 
esVacia (CP xs) = null xs



