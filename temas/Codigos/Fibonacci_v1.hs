-- Fibonacci.hs
-- Fibonacci como ejemplo de programación dinámica.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 23 de Diciembre de 2010
-- ---------------------------------------------------------------------

module Fibonacci where

import Dinamica

-- ---------------------------------------------------------------------
-- Fibonacci como ejemplo de programación dinámica.                   --
-- ---------------------------------------------------------------------

-- (fib n) es el n-ésimo término de la sucesión de Fibonacci, calculado
-- mediante programación dinámica. Por ejemplo, 
--    fib 8  ==  21
fib :: Int -> Int
fib n = valor t n
  where t = dinamica calculaFib (cotasFib n) 

-- (calculaFib t i) es el valor de i-ésimo término de la sucesión de
-- Fibonacci calculado mediante la tabla t que contiene los
-- anteriores. Por ejemplo,
--    calculaFib (tabla []) 0                                     ==  0
--    calculaFib (tabla [(0,0)]) 1                                ==  1
--    calculaFib (tabla [(0,0),(1,1)]) 2                          ==  1
--    calculaFib (tabla [(0,0),(1,1),(2,1)]) 3                    ==  2
--    calculaFib (tabla [(0,0),(1,1),(2,1),(3,2)]) 4              ==  3
--    calculaFib (tabla [(0,0),(1,1),(2,1),(3,2),(4,3)]) 5        ==  5
--    calculaFib (tabla [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5)]) 6  ==  8
-- Además,
--    ghci> dinamica calculaFib (0,8)
--    Tbl [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5),(6,8),(7,13),(8,21)]
calculaFib :: Tabla Int Int -> Int -> Int
calculaFib t i
  | i <= 1    = i
  | otherwise = valor t (i-1) + valor t (i-2)

-- (cotasFib n) son las cotas del vector que se necesita para calcular
-- el n-ésimo término de la sucesión de Fibonacci mediante programación
-- dinámica. 
cotasFib :: Int -> (Int,Int)
cotasFib n = (0,n)

-- ---------------------------------------------------------------------
-- Fibonacci mediante divide y vencerás                               --
-- ---------------------------------------------------------------------

-- (fibR n) es el n-ésimo término de la sucesión de Fibonacci calculado
-- mediante divide y vencerás. Por ejemplo, 
--    fibR 8  ==  21
fibR :: Int -> Int
fibR 0 = 0
fibR 1 = 1
fibR n = fibR (n-1) + fibR (n-2)

-- Comparación
--    ghci> fib 20
--    6765
--    (0.01 secs, 524824 bytes)
--    ghci> fibR 20
--    6765
--    (0.06 secs, 2165236 bytes)
--    ghci> fib 30
--    832040
--    (0.01 secs, 0 bytes)
--    ghci> fibR 30
--    832040
--    (6.46 secs, 222602404 bytes)

-- ---------------------------------------------------------------------
-- Fibonacci mediante programación dinámica con listas infinitas      --
-- ---------------------------------------------------------------------

-- fibs es la lista de los términos de la sucesión de Fibonacci. Por
-- ejemplo, 
--    take 10 fibs  ==  [0,1,1,2,3,5,8,13,21,34]
fibs :: [Int]
fibs = 0:1:[x+y | (x,y) <- zip fibs (tail fibs)]

-- (fib' n) es el n-ésimo término de la sucesión de Fibonacci, calculado
-- a partir de fibs. Por ejemplo, 
--    fib' 8  ==  21
fib' :: Int -> Int
fib' n = fibs!!n

-- Comparaciones:
--    ghci> fib 30
--    832040
--    (0.02 secs, 524808 bytes)
--    ghci> fib' 30
--    832040
--    (0.01 secs, 542384 bytes)
