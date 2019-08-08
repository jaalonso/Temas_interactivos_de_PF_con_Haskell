-- GrafoConMatrizDeAdyacencia.hs
-- Representación del TAD grafo mediante matriz de adyacencia.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 24 de Octubre de 2010 (Revisión del 25 de Abril de 2012)
-- ---------------------------------------------------------------------

module GrafoConMatrizDeAdyacencia
  (Orientacion (..),
   Grafo,
   creaGrafo,  -- (Ix v,Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> 
               --                 Grafo v p
   dirigido,   -- (Ix v,Num p) => (Grafo v p) -> Bool
   adyacentes, -- (Ix v,Num p) => (Grafo v p) -> v -> [v]
   nodos,      -- (Ix v,Num p) => (Grafo v p) -> [v]
   aristas,    -- (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
   aristaEn,   -- (Ix v,Num p) => (Grafo v p) -> (v,v) -> Bool
   peso        -- (Ix v,Num p) => v -> v -> (Grafo v p) -> p
  ) where

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.Array

-- Orientacion es D (dirigida) ó ND (no dirigida).
data Orientacion = D | ND
  deriving (Eq, Show)

-- (Grafo v p) es un grafo con vértices de tipo v y pesos de tipo p.
data Grafo v p = G Orientacion (Array (v,v) (Maybe p))
  deriving (Eq, Show)

-- (creaGrafo d cs as) es un grafo (dirigido o no, según el valor de o),
-- con el par de cotas cs y listas de aristas as (cada arista es un trío
-- formado por los dos vértices y su peso). Ver el ejemplo a continuación.
creaGrafo :: (Ix v,Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo D cs as =
  G D (matrizVacia cs // [((v1,v2),Just c) | (v1,v2,c) <- as])
creaGrafo ND cs as =
  G ND (matrizVacia cs // ([((v1,v2),Just c) | (v1,v2,c) <- as] ++
                           [((v2,v1),Just c) | (v1,v2,c) <- as, v1 /= v2]))

matrizVacia :: Ix v => (v,v) -> Array (v,v) (Maybe p)
matrizVacia (l,u) =
  listArray ((l,l),(u,u)) (repeat Nothing)

-- ejGrafoND es el grafo
--             12
--        1 -------- 2
--        | \78     /|
--        |  \   32/ |
--        |   \   /  |
--      34|     5    |55
--        |   /   \  |
--        |  /44   \ |
--        | /     93\|
--        3 -------- 4
--             61
-- representado mediante una matriz de adyacencia.
--    ghci> ejGrafoND
--    G ND array ((1,1),(5,5)) 
--               [((1,1),Nothing),((1,2),Just 12),((1,3),Just 34),
--                ((1,4),Nothing),((1,5),Just 78),((2,1),Just 12),
--                ((2,2),Nothing),((2,3),Nothing),((2,4),Just 55),
--                ((2,5),Just 32),((3,1),Just 34),((3,2),Nothing),
--                ((3,3),Nothing),((3,4),Just 61),((3,5),Just 44),
--                ((4,1),Nothing),((4,2),Just 55),((4,3),Just 61),
--                ((4,4),Nothing),((4,5),Just 93),((5,1),Just 78),
--                ((5,2),Just 32),((5,3),Just 44),((5,4),Just 93),
--                ((5,5),Nothing)]
ejGrafoND :: Grafo Int Int
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]

-- ejGrafoD es el mismo grafo que ejGrafoND pero orientando las aristas;
-- es decir,
--    ghci> ejGrafoD
--    G D (array ((1,1),(5,5)) 
--               [((1,1),Nothing),((1,2),Just 12),((1,3),Just 34),
--                ((1,4),Nothing),((1,5),Just 78),((2,1),Nothing),
--                ((2,2),Nothing),((2,3),Nothing),((2,4),Just 55),
--                ((2,5),Just 32),((3,1),Nothing),((3,2),Nothing),
--                ((3,3),Nothing),((3,4),Just 61),((3,5),Just 44),
--                ((4,1),Nothing),((4,2),Nothing),((4,3),Nothing),
--                ((4,4),Nothing),((4,5),Just 93),((5,1),Nothing),
--                ((5,2),Nothing),((5,3),Nothing),((5,4),Nothing),
--                ((5,5),Nothing)])
ejGrafoD :: Grafo Int Int
ejGrafoD = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                              (2,4,55),(2,5,32),
                              (3,4,61),(3,5,44),
                              (4,5,93)]

-- (dirigido g) se verifica si g es dirigido. Por ejemplo,
--    dirigido ejGrafoD   ==  True
--    dirigido ejGrafoND  ==  False
dirigido :: (Ix v,Num p) => Grafo v p -> Bool
dirigido (G o _) = o == D

-- (nodos g) es la lista de todos los nodos del grafo g. Por ejemplo,
--    nodos ejGrafoND  ==  [1,2,3,4,5]
--    nodos ejGrafoD   ==  [1,2,3,4,5]
nodos :: (Ix v,Num p) => Grafo v p -> [v]
nodos (G _ g) = range (l,u) 
  where ((l,_),(u,_)) = bounds g

-- (adyacentes g v) es la lista de los vértices adyacentes al nodo v en
-- el grafo g. Por ejemplo,
--    adyacentes ejGrafoND 4  ==  [2,3,5]
--    adyacentes ejGrafoD  4  ==  [5]
adyacentes :: (Ix v,Num p,Eq p) => Grafo v p -> v -> [v]
adyacentes (G o g) v = 
  [v' | v' <- nodos (G o g), (g!(v,v')) /= Nothing]

-- (aristaEn g a) se verifica si a es una arista del grafo g. Por
-- ejemplo,
--    aristaEn ejGrafoND (5,1)  ==  True
--    aristaEn ejGrafoND (4,1)  ==  False
aristaEn :: (Ix v,Num p,Eq p) => Grafo v p -> (v,v) -> Bool
aristaEn (G _o g) (x,y)= (g!(x,y)) /= Nothing

-- (peso v1 v2 g) es el peso de la arista que une los vértices v1 y v2
-- en el grafo g. Por ejemplo,
--    peso 1 5 ejGrafoND  ==  78
--    peso 1 5 ejGrafoD   ==  78
peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
peso x y (G _ g)  = w where (Just w) = g!(x,y)

-- (aristas g) es la lista de las aristas del grafo g. Por ejemplo, 
--    ghci> aristas ejGrafoD
--    [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),
--     (3,5,44),(4,5,93)] 
--    ghci> aristas ejGrafoND
--    [(1,2,12),(1,3,34),(1,5,78),(2,1,12),(2,4,55),(2,5,32),
--     (3,1,34),(3,4,61),(3,5,44),(4,2,55),(4,3,61),(4,5,93),
--     (5,1,78),(5,2,32),(5,3,44),(5,4,93)]
aristas :: (Ix v,Num p, Eq p) => Grafo v p -> [(v,v,p)]
aristas g@(G _ e) = [(v1,v2,extrae(e!(v1,v2))) 
                     | v1 <- nodos g, 
                       v2 <- nodos g,
                       aristaEn g (v1,v2)]
  where extrae (Just w) = w
