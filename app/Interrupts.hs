module Interrupts where

import States
import Utils (addPC)
import Control.Monad.State

-- TODO: Implement this
hlt :: State8080M State8080
hlt = addPC 1
    
di :: State8080M State8080
di = do
  s <- get
  put s{inte = 0}
  addPC 1

ei :: State8080M State8080
ei = do
  s <- get
  put s{inte = 1}
  addPC 1