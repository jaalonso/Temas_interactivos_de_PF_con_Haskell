-- TablaConMatrices.hs
-- Tablas mediante matrices.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

module TablaConMatrices 
  (Tabla,
   tabla,     -- Eq i => [(i,v)] -> Tabla i v           
   valor,     -- Eq i => Tabla i v -> i -> v            
   modifica,  -- Eq i => (i,v) -> Tabla i v -> Tabla i v
   tieneValor -- Ix i => Tabla i v -> i -> Bool
  ) where

import Data.Array (Array, Ix, array, (//), (!), indices, bounds, inRange)

-- Las tablas como matrices.
newtype Tabla i v = Tbl (Array i v)
  deriving (Show, Eq)

-- Ejemplos de tablas:
--    ghci> t1
--    Tbl (array (1,6) [(1,1),(2,2),(3,0),(4,-1),(5,-2),(6,-3)])
--    ghci> t2
--    Tbl (array (1,3) [(1,5),(2,4),(3,7)])
t1, t2 :: Tabla Int Int
t1 = tabla [(i,f i) | i <- [1..6] ] 
  where f x | x < 3     = x
            | otherwise = 3-x
t2 = tabla [(1,5),(2,4),(3,7)]
    
-- (tabla ivs) es la tabla correspondiente a la lista de asociación
-- ivs (que es una lista de pares formados por los índices y los
-- valores). Por ejemplo,
--    ghci> tabla [(1,5),(3,7),(2,4)]
--    Tbl (array (1,3) [(1,5),(2,4),(3,7)])
tabla :: Ix i => [(i,v)] -> Tabla i v
tabla ivs = Tbl (array (m,n) ivs)
  where indices = [i | (i,_) <- ivs]
        m       = minimum indices
        n       = maximum indices

-- (valor t i) es el valor del índice i en la tabla t. Por ejemplo, 
--    valor t1 6  ==  -3
--    valor t2 2  ==   4
--    valor t2 5  ==  *** Exception: Index (5) out of range ((1,3))
valor :: Ix i => Tabla i v -> i -> v
valor (Tbl t) i = t ! i

-- (modifica (i,x) t) es la tabla obtenida modificando en la tabla t el
-- valor de i por x. Por ejemplo, 
--    valor t1 6                   ==  -3
--    valor (modifica (6,9) t1) 6  ==  9
modifica :: Ix i => (i,v) -> Tabla i v -> Tabla i v
modifica (i,v) (Tbl t)
  | i `elem` indices t = Tbl (t // [(i,v)])
  | otherwise          = Tbl t

-- (cotas t) son las cotas de la tabla t. Por ejemplo,
--    t2        ==  Tbl (array (1,3) [(1,5),(2,4),(3,7)])
--    cotas t2  ==  (1,3)
cotas :: Ix i => Tabla i v -> (i,i)
cotas (Tbl t) = bounds t

-- (tieneValor t x) se verifica si x es una clave de la tabla t. Por ejemplo,
--    tieneValor t2 3  ==  True
--    tieneValor t2 4  ==  False
tieneValor :: Ix i => Tabla i v -> i -> Bool
tieneValor t = inRange (cotas t)




