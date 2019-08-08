-- ConjuntoConListasOrdenadasSinDuplicados.hs
-- Implementación de conjuntos mediante listas ordenadas sin repeticiones.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

module ConjuntoConListasOrdenadasSinDuplicados
    (Conj,
     vacio,     -- Conj a                       
     esVacio,   -- Conj a -> Bool               
     pertenece, -- Ord a => a -> Conj a -> Bool  
     inserta,   -- Ord a => a -> Conj a -> Conj a
     elimina    -- Ord a => a -> Conj a -> Conj a
    ) where

-- Los conjuntos como listas ordenadas sin repeticiones.
newtype Conj a = Cj [a]
    deriving Eq

-- Procedimiento de escritura de los conjuntos.
instance (Show a) => Show (Conj a) where
  showsPrec _ (Cj s) = showConj s 

showConj :: Show a => [a] -> String -> String
showConj []     cad = showString "{}" cad
showConj (x:xs) cad = showChar '{' (shows x (showl xs cad))
  where showl []     cs = showChar '}' cs
        showl (y:ys) cs = showChar ',' (shows y (showl ys cs))

-- Ejemplo de conjunto:
--    ghci> c1
--    {0,1,2,3,5,7,9}
c1 :: Conj Int
c1 = foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]

-- vacio es el conjunto vacío. Por ejemplo,
--    ghci> vacio
--    {}
vacio :: Conj a                         
vacio = Cj []

-- (esVacio c) se verifica si c es el conjunto vacío. Por ejemplo, 
--    esVacio c1     ==  False
--    esVacio vacio  ==  True
esVacio :: Conj a -> Bool                
esVacio (Cj xs) = null xs

-- (pertenece x c) se verifica si x pertenece al conjunto c. Por ejemplo, 
--    c1              ==  {0,1,2,3,5,7,9}
--    pertenece 3 c1  ==  True
--    pertenece 4 c1  ==  False
pertenece :: Ord a => a -> Conj a -> Bool 
pertenece x (Cj s) = x `elem` takeWhile (<= x) s

-- (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. Por ejemplo,
--    c1            ==  {0,1,2,3,5,7,9}
--    inserta 5 c1  ==  {0,1,2,3,5,7,9}
--    inserta 4 c1  ==  {0,1,2,3,4,5,7,9}
inserta :: Ord a => a -> Conj a -> Conj a
inserta x (Cj s) = Cj (agrega x s)
  where agrega z []                    = [z]                
        agrega z s'@(y:ys) | z > y      = y : agrega z ys
                           | z < y      = z : s'
                           | otherwise  = s'

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
--    c1            ==  {0,1,2,3,5,7,9}
--    elimina 3 c1  ==  {0,1,2,5,7,9}
elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Cj s) = Cj (elimina' x s)
  where elimina' _ []                   = []
        elimina' z s'@(y:ys) | z > y     = y : elimina' z ys
                             | z < y     = s'
                             | otherwise = ys
