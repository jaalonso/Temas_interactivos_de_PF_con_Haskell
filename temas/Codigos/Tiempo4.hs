module Tiempo where

import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.Monad (replicateM)

tiempo1 :: a -> IO NominalDiffTime
tiempo1 e = do
  inicial <- getCurrentTime
  final  <- e `seq` getCurrentTime
  return (diffUTCTime final inicial)

tiempo :: a -> IO NominalDiffTime
tiempo e = do
  xs <- replicateM 100 (tiempo1 e)
  return (sum xs)
