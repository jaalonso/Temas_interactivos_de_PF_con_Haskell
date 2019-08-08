-- PilaConTipoDeDatoAlgebraico.hs
-- Implementación de las pilas mediante tipos de datos algebraicos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

module PilaConTipoDeDatoAlgebraico 
    (Pila,
     vacia,    -- Pila a
     apila,    -- a -> Pila a -> Pila a
     cima,     -- Pila a -> a
     desapila, -- Pila a -> Pila a
     esVacia   -- Pila a -> Bool
    ) where

-- Tipo de dato algebraico de las pilas:
data Pila a = Vacia
            | P a (Pila a)
              deriving Eq

-- Procedimiento de escritura de pilas.
instance (Show a) => Show (Pila a) where
  showsPrec _ Vacia cad   = showChar '-' cad
  showsPrec _ (P x s) cad = shows x (showChar '|' (shows s cad))

-- Ejemplo de pila:
--    ghci> p1
--    1|2|3|-
p1 :: Pila Int
p1 = apila 1 (apila 2 (apila 3 vacia))

-- vacia es la pila vacía. Por ejemplo,
--    ghci> vacia
--    -
vacia :: Pila a
vacia = Vacia

-- (apila x p) es la pila obtenida añadiendo x encima de la pila p. Por
-- ejemplo, 
--    apila 4 p1  =>  4|1|2|3|-
apila :: a -> Pila a -> Pila a
apila x p = P x p

-- (cima p) es la cima de la pila p. Por ejemplo,
--    cima p1  ==  1
cima :: Pila a -> a
cima Vacia   = error "la pila vacia no tiene cima"
cima (P x _) =  x

-- (desapila p) es la pila obtenida suprimiendo la cima de la pila
-- p. Por ejemplo,
--    desapila p1  =>  2|3|-
desapila :: Pila a -> Pila a
desapila Vacia   = error "no se puede desapila la pila vacia"
desapila (P _ p) = p

-- (esVacia p) se verifica si p es la pila vacía. Por ejemplo,
--    esVacia p1         ==  False
--    esVacia vacia  ==  True
esVacia :: Pila a -> Bool
esVacia Vacia = True
esVacia _     = False
