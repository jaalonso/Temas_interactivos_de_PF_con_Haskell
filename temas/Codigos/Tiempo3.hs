import System.Microtimer (time, time_)
import Control.Exception (evaluate)
import Control.Monad (replicateM)

ejemplo1 = time (evaluate (sum [1..10^5]))
ejemplo2 = time_ (evaluate (sum [1..10^5]))

tiempo e = do
  xs <- replicateM 100 (time_ (evaluate e))
  -- mapM_ print xs
  return (sum xs / 100, maximum xs, minimum xs)
