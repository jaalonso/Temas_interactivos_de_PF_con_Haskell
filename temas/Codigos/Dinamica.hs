-- Dinamica.hs
-- Patrón de la programación dinámica.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 23 de Diciembre de 2010
-- ---------------------------------------------------------------------

module Dinamica (module Tabla, dinamica)  where

-- Hay que elegir una implementación de TAD Tabla
-- import TablaConFunciones as Tabla
import TablaConListasDeAsociacion as Tabla
-- import TablaConMatrices as Tabla

import Data.Array

dinamica :: Ix i => (Tabla i v -> i -> v) -> (i,i) -> Tabla i v
dinamica calcula cotas = t
  where t = tabla [(i,calcula t i) | i <- range cotas]
