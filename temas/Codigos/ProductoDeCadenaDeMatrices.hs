-- ProductoDeCadenaDeMatrices.hs
-- Producto de cadenas de matrices.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 22 de Enero de 2011
-- ---------------------------------------------------------------------

module ProductoDeCadenaDeMatrices where

-- ---------------------------------------------------------------------
-- Descripción del problema                                           --
-- ---------------------------------------------------------------------

-- Para multiplicar una matriz de orden m*p y otra de orden p*n se
-- necesitan mnp multiplicaciones de elementos.

-- El problema del producto de una cadena de matrices (en inglés,
-- "matrix chain multiplication") consiste en dada una sucesión de
-- matrices encontrar la manera de multiplicarlas usando el menor número
-- de productos de elementos.   

-- Ejemplo: Dada la sucesión de matrices
--    A (30 x 1), B (1 x 40), C (40 x 10), D (10 x 25)
-- las productos necesarios en las posibles asociaciones son
--    ((AB)C)D  30 x  1 x 40 + 30 x 40 x 10 + 30 x 10 x 25 = 20700
--    A{B{CD))  40 x 10 x 25 +  1 x 40 x 25 + 30 x  1 x 25 = 11750
--    (AB)(CD)  30 x  1 x 40 + 40 x 10 x 25 + 30 x 40 x 25 = 41200
--    A((BC)D)   1 x 40 x 10 +  1 x 10 x 25 + 30 x  1 x 25 =  1400 
--    (A(BC))D  1  x 40 x 10 + 30 x  1 x 10 + 30 x 10 x 25 =  8200 

-- ---------------------------------------------------------------------
-- El algoritmo                                                       --
-- ---------------------------------------------------------------------

-- Sea ds=[d_0,...,d_n)] una sucesión de números naturales.

-- A_1,...,A_n es una cadena de matrices de tipo ds si para cada i, A_i es
-- una matriz de orden d_(i-1)xd_i.

-- c(i,j) es el mínimo número de multiplicaciones para multiplicar la
-- cadena Ai,...,Aj (1<=i<=j<=n).

-- Relación de recurrencia de c(i,j):
-- + c(i,i) = 0
-- + c(i,j) = min { c(i,k)+c(k+1,j)+d_(i-1)*d_k*d_j | i<=k<=j}

-- La solución del problema es c(1,n).

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Dinamica

-- ---------------------------------------------------------------------
-- Solución mediante programación dinámica                            --
-- ---------------------------------------------------------------------

-- Cadena representa el producto de una cadena de matrices. Por ejemplo,
--    ghci> P (A 1) (P (A 2) (A 3))
--    (A1*(A2*A3))
--    ghci> P (P (A 1) (A 2)) (A 3)
--    ((A1*A2)*A3)
data Cadena = A Int 
            | P Cadena Cadena

instance Show Cadena where
  show (A x)     = "A" ++ show x
  show (P p1 p2) = concat ["(", show p1, "*", show p2, ")"]

-- Los índices de la matriz de cálculo son de la forma (i,j) y sus
-- valores (v,k) donde v es el mínimo número de multiplicaciones
-- necesarias para multiplicar la cadena Ai,...,Aj y k es la posición
-- donde dividir la cadena de forma óptima.
type IndicePCM = (Int,Int)
type ValorPCM  = (Int,Int)

-- (pcm ds) es la el par formado por el número de multiplicaciones
-- elementales de la cadena óptima para multiplicar las matrices A1, A2, ...
-- tales que sus dimensiones son (d1*d2), (d2*d3), ... donde [d1,d2,...]
-- es ds y la cadena. Por ejemplo,  
--    ghci> pcm [30,1,40,10,25]
--    (1400,(A1*((A2*A3)*A4)))
pcm :: [Int] -> (Int, Cadena)
pcm ds = (v, cadena t 1 n)
  where n     = length ds - 1
        t     = dinamica (calculaPCM ds) (cotasPCM n)
        (v,_) = valor t (1,n)

-- (calculaPCM ds t (i,j)) es el valor del índice (i,j) calculado a
-- partir de la lista ds de dimensiones de las matrices y la tabla t de
-- valores previamente calculados.
calculaPCM :: [Int] -> Tabla IndicePCM ValorPCM -> IndicePCM -> ValorPCM
calculaPCM ds t (i,j) 
  | i == j    = (0,i)
  | otherwise = minimum [(fst(valor t (i,k)) 
                          + fst(valor t (k+1,j)) 
                          + ds!!(i-1) * ds!!k * ds!!j, k) 
                         | k <- [i..j-1]]

-- (cotasPCM n) son las cotas de los índices para el producto de una
-- cadena de n matrices.
cotasPCM :: Int -> (IndicePCM,IndicePCM)
cotasPCM n = ((1,1),(n,n)) 

-- (cadena t i j) es la cadena que resultar de agrupar las matrices
-- Ai,...,Aj según los valores de la tabla t.
cadena :: Tabla IndicePCM ValorPCM -> Int -> Int -> Cadena
cadena t i j 
  | i == j-1  = P (A i) (A j)
  | k == i    = P (A i) (cadena t (i+1) j)
  | k == j-1  = P (cadena t i (j-1)) (A j)
  | otherwise = P (cadena t i (k-1)) (cadena t k j)
  where (_,k) = valor t (i,j)

-- (pcm' ds) es la lista de los índices y valores usados en el cálculo
-- de la cadena óptima para multiplicar las matrices A1, A2, ... tales
-- que sus dimensiones son (d1*d2), (d2*d3), ... donde [d1,d2,...] es
-- ds. Por ejemplo,   
--    ghci> pcm' [30,1,40,10,25]
--    [((1,1),(0,1)),((1,2),(1200,1)),((1,3),(700,1)),((1,4),(1400,1)),
--     ((2,2),(0,2)),((2,3),(400,2)),((2,4),(650,3)),
--     ((3,3),(0,3)),((3,4),(10000,3)),
--     ((4,4),(0,4))]
pcm' :: [Int] -> [((Int, Int), ValorPCM)]
pcm' ds = [((i,j),valor t (i,j)) | i <- [1..n], j <- [i..n]] 
    where n = length ds - 1
          t = dinamica (calculaPCM ds) (cotasPCM n)

-- ---------------------------------------------------------------------
-- Solución mediante divide y vencerás                                --
-- ---------------------------------------------------------------------

-- (pcmDyV ds) es la el par formado por el número de multiplicaciones
-- elementales de la cadena óptima para multiplicar las matrices 
-- A1, A2, ...tales que sus dimensiones son (d1*d2), (d2*d3), ... donde
-- [d1,d2,...] es ds y la cadena, calculada mediante divide y
-- vencerás. Por ejemplo,   
--    ghci> pcmDyV [30,1,40,10,25]
--    (1040,(A1*((A2*A3)*A4)))
pcmDyV :: [Int] -> (Int, Cadena)
pcmDyV ds = cadenaDyV ds 1 n
  where n = length ds - 1
         
-- (cadenaDyV ds i j) es el par formado por el número de
-- multiplicaciones elementales de la cadena óptima para multiplicar las
-- matrices  Ai, ..., Aj tales que sus dimensiones son 
-- (di*d_(i+1)), ... (d_(j-1)*dj), donde [d1,d2,...] es ds y la cadena,
-- calculada mediante divide y vencerás. Por ejemplo,   
--    cadenaDyV [30,1,40,10,25] 1 4  ==  (1040,(A1*((A2*A3)*A4)))
--    cadenaDyV [30,1,40,10,25] 2 4  ==  (290,((A2*A3)*A4))
cadenaDyV :: [Int] -> Int -> Int -> (Int, Cadena)
cadenaDyV ds i j 
  | i == j    = (0, A i)
  | i == j-1  = (ds!!1*ds!!2, P (A i) (A j))
  | k == i    = (v, P (A i) (subcadena (i+1) j))
  | k == j-1  = (v, P (subcadena i (j-1)) (A j))
  | otherwise = (v, P (subcadena i (k-1)) (subcadena k j))
  where (v,k) = minimum [(valor' i k' 
                          + valor' (k'+1) j 
                          + ds!!(i-1) * ds!!k' * ds!!j
                         , k') 
                         | k' <- [i..j-1]]
        valor' p q    = fst (cadenaDyV ds p q)
        subcadena p q = snd (cadenaDyV ds p q)

-- ---------------------------------------------------------------------
-- Comparación de las soluciones                                      --
-- ---------------------------------------------------------------------

--    ghci> :set +s
--    ghci> fst (pcm [1..20])
--    2658
--    (0.80 secs, 39158964 bytes)
--    ghci> fst (pcmDyV [1..20])
--    1374
--    (2871.47 secs, 133619742764 bytes)
