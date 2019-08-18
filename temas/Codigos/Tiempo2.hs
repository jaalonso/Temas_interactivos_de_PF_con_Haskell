{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

tiempo e =
  do start <- getTime Monotonic
     evaluate e
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end

tiempo2 e =
  do start <- getTime Monotonic
     evaluate e
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end
     print start
     print end

