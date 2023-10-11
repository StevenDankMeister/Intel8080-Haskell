module Interrupts where

import States
import Utils (addPC)

-- TODO: Implement this
hlt :: State8080M State8080
hlt = addPC 1
    