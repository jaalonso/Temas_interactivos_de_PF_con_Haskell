-- PolRepDispersa.hs
-- Implementación de polinomios mediante listas dispersas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 20 de Septiembre de 2010
-- ---------------------------------------------------------------------

module PolRepDispersa
  ( Polinomio,
    polCero,   -- Polinomio a                                         
    esPolCero, -- Num a =>  Polinomio a -> Bool                       
    consPol,   -- Num a => Int -> a -> Polinomio a -> Polinomio a   
    grado,     -- Polinomio a -> Int                                  
    coefLider, -- Num a => Polinomio a -> a                           
    restoPol   -- Polinomio a -> Polinomio a                          
  ) where

-- ---------------------------------------------------------------------
-- TAD de los polinomios mediante listas dispersas.                      --
-- ---------------------------------------------------------------------

-- Representaremos un polinomio mediante una lista de pares (grado,coef),
-- ordenados en orden decreciente según el grado. Por ejemplo, el
-- polinomio 
--    6x^4 -5x^2 + 4x -7 
-- se representa por
--    [(4,6),(2,-5),(1,4),(0,-7)]. 

newtype Polinomio a = Pol [(Int,a)] 
  deriving Eq
    
-- ---------------------------------------------------------------------
-- Escritura de los polinomios                                        --
-- ---------------------------------------------------------------------

instance (Num t, Show t, Eq t) => Show (Polinomio t) where
  show pol 
    | esPolCero pol         = "0"
    | n == 0 && esPolCero p = show a 
    | n == 0                = concat [show a, " + ", show p] 
    | n == 1 && esPolCero p = show a ++ "*x"
    | n == 1                = concat [show a, "*x + ", show p] 
    | a == 1 && esPolCero p = "x^" ++ show n
    | esPolCero p           = concat [show a, "*x^", show n] 
    | a == 1                = concat ["x^", show n, " + ", show p] 
    | otherwise             = concat [show a, "*x^", show n, " + ", show p] 
    where n = grado pol
          a = coefLider pol
          p = restoPol pol

-- ---------------------------------------------------------------------
-- Ejemplos de polinomios                                             --
-- ---------------------------------------------------------------------

-- Ejemplos de polinomios con coeficientes enteros:
ejPol1, ejPol2, ejPol3:: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)

-- Comprobación de escritura:
--    > ejPol1
--    3*x^4 + -5*x^2 + 3
--    > ejPol2
--    x^5 + 5*x^2 + 4*x
--    > ejPol3
--    6*x^4 + 2*x

-- ---------------------------------------------------------------------
-- Implementación de la especificación                                --
-- ---------------------------------------------------------------------

-- polCero es el polinomio cero. Por ejemplo,
--    ghci> polCero
--    0
polCero :: Num a => Polinomio a
polCero = Pol []

-- (esPolCero p) se verifica si p es el polinomio cero. Por ejemplo,
--    esPolCero polCero  ==  True
--    esPolCero ejPol1   ==  False
esPolCero :: Num a => Polinomio a -> Bool
esPolCero (Pol []) = True
esPolCero _        = False

-- (consPol n b p) es el polinomio bx^n+p. Por ejemplo,
--    ejPol2               ==  x^5 + 5*x^2 + 4*x
--    consPol 3 0 ejPol2   ==  x^5 + 5*x^2 + 4*x
--    consPol 3 2 polCero  ==  2*x^3
--    consPol 6 7 ejPol2   ==  7*x^6 + x^5 + 5*x^2 + 4*x
--    consPol 4 7 ejPol2   ==  x^5 + 7*x^4 + 5*x^2 + 4*x
--    consPol 5 7 ejPol2   ==  8*x^5 + 5*x^2 + 4*x
consPol :: (Num a, Eq a) => Int -> a -> Polinomio a -> Polinomio a
consPol _ 0 p = p
consPol n b p@(Pol xs) 
    | esPolCero p = Pol [(n,b)]
    | n > m       = Pol ((n,b):xs)
    | n < m       = consPol m c (consPol n b (Pol (tail xs)))
    | b+c == 0    = Pol (tail xs)
    | otherwise   = Pol ((n,b+c) : tail xs)
    where 
      c = coefLider p
      m = grado p

-- (grado p) es el grado del polinomio p. Por ejemplo,
--    ejPol3        ==  6*x^4 + 2*x
--    grado ejPol3  ==  4
grado:: Polinomio a -> Int
grado (Pol [])        = 0
grado (Pol ((n,_):_)) = n

-- (coefLider p) es el coeficiente líder del polinomio p. Por ejemplo,
--    ejPol3            ==  6*x^4 + 2*x
--    coefLider ejPol3  ==  6
coefLider:: Num t => Polinomio t -> t
coefLider (Pol [])        = 0
coefLider (Pol ((_,b):_)) = b

-- (restoPol p) es el resto del polinomio p. Por ejemplo,
--    ejPol3           ==  6*x^4 + 2*x
--    restoPol ejPol3  ==  2*x
--    ejPol2           ==  x^5 + 5*x^2 + 4*x
--    restoPol ejPol2  ==  5*x^2 + 4*x
restoPol :: Num t => Polinomio t -> Polinomio t
restoPol (Pol [])     = polCero
restoPol (Pol [_])    = polCero
restoPol (Pol (_:xs)) = Pol xs


