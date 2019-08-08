-- ColaConDosListas.hs
-- Implementación de las colas mediante dos listas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

-- En esta implementación, una cola c se representa mediante un par de
-- listas (xs,ys) de modo que los elementos de c son, en ese orden, los
-- elementos de la lista xs++(reverse ys).

-- Al dividir la lista en dos parte e invertir la segunda de ellas,
-- esperamos hacer más eficiente las operaciones sobre las colas.

-- Impondremos también una restricción adicional sobre la
-- representación: las colas serán representadas mediante pares (xs,ys)
-- tales que si xs es vacía, entonces ys será también vacía. Esta
-- restricción ha de ser conservada por los programas que crean colas.

module ColaConDosListas
    (Cola,
     vacia,   -- Cola a
     inserta, -- a -> Cola a -> Cola a
     primero, -- Cola a -> a
     resto,   -- Cola a -> Cola a
     esVacia, -- Cola a -> Bool
     valida   -- Cola a -> Bool
    ) where

-- Las colas como pares listas.
newtype Cola a = C ([a],[a])
    -- deriving Show

-- Procedimiento de escritura de colas como pares de listas.
instance (Show a) => Show (Cola a) where
  showsPrec _ (C (xs,ys)) cad =
    showString "C " (showList (xs ++ reverse ys) cad)

-- Ejemplo de cola: c1 es la cola obtenida añadiéndole a la cola 
-- vacía los números del 1 al 10. Por ejemplo,
--    ghci> c1
--    C [10,9,8,7,6,5,4,3,2,1]
c1 :: Cola Int
c1 = foldr inserta vacia [1..10]

-- vacia es la cola vacía. Por ejemplo,
--    ghci> vacia
--    C []
vacia :: Cola a
vacia  = C ([],[])

-- (inserta x c) es la cola obtenida añadiendo x al final de la cola
-- c. Por ejemplo,
--    inserta 12 c1  ==  C [10,9,8,7,6,5,4,3,2,1,12]
inserta :: a -> Cola a -> Cola a
inserta y (C (xs,ys)) = C (normaliza (xs,y:ys))

-- (normaliza p) es la cola obtenida al normalizar el par de listas
-- p. Por ejemplo,  
--    normaliza ([],[2,5,3])   ==  ([3,5,2],[])
--    normaliza ([4],[2,5,3])  ==  ([4],[2,5,3])
normaliza :: ([a],[a]) -> ([a],[a])
normaliza ([], ys) = (reverse ys, [])
normaliza p        = p

-- (primero c) es el primer elemento de la cola c. Por ejemplo,
--    primero c1  ==  10
primero  :: Cola a -> a
primero (C (x:_,_)) = x
primero _           = error "primero: cola vacia"

-- (resto c) es la cola obtenida eliminando el primer elemento de la
-- cola c. Por ejemplo,
--    resto c1  ==  C [9,8,7,6,5,4,3,2,1]
resto  :: Cola a -> Cola a
resto (C ([],[]))   = error "resto: cola vacia"
resto (C (_:xs,ys)) = C (normaliza (xs,ys))

-- (esVacia c) se verifica si c es la cola vacía. Por ejemplo,
--    esVacia c1     ==  False
--    esVacia vacia  ==  True
esVacia :: Cola a -> Bool
esVacia (C (xs,_)) = null xs

-- (valida c) se verifica si la cola c es válida; es decir, si
-- su primer elemento es vacío entonces también lo es el segundo. Por
-- ejemplo, 
--    valida (C ([2],[5]))  ==  True
--    valida (C ([2],[]))   ==  True
--    valida (C ([],[5]))   ==  False
valida:: Cola a -> Bool
valida (C (xs,ys)) = not (null xs) || null ys

-- ---------------------------------------------------------------------
-- Igualdad de colas                                                  --
-- ---------------------------------------------------------------------

-- (elementos c) es la lista de los elementos de la cola c en el orden de
-- la cola. Por ejemplo, 
--    elementos (C ([3,2],[5,4,7]))  ==  [3,2,7,4,5]
elementos :: Cola a -> [a]
elementos (C (xs,ys)) = xs ++ reverse ys

-- (igualColas c1 c2) se verifica si las colas c1 y c2 son iguales. Por
-- ejemplo, 
--    igualColas (C ([3,2],[5,4,7])) (C ([3],[5,4,7,2]))   ==  True
--    igualColas (C ([3,2],[5,4,7])) (C ([],[5,4,7,2,3]))  ==  False
igualColas :: Eq a => Cola a -> Cola a -> Bool
igualColas c c' = 
  valida c && valida c' && elementos c == elementos c'

instance (Eq a) => Eq (Cola a) where
  (==) = igualColas
