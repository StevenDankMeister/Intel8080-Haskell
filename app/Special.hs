{-# LANGUAGE OverloadedRecordDot #-}

module Special where

import Control.Monad.State
import Data.Bits (Bits (xor), shiftL, shiftR, (.|.))
import States
import Utils

stc :: State8080M State8080
stc = do
  s <- get
  let ccodes = s.ccodes{cy = 1}
  put s{ccodes = ccodes}
  addPC 1

cmc :: State8080M State8080
cmc = do
  s <- get
  let ccodes = s.ccodes{cy = s.ccodes.cy `xor` 0x1}
  put s{ccodes = ccodes}
  addPC 1

daa :: State8080M State8080
daa = do
  s <- get
  let a_lower_masked = maskLower4Bytes s.a
  let a_s1 = if a_lower_masked > 9 || s.ccodes.ac == 1 then s.a + 6 else s.a

  let a_s1_upper_masked = maskUpper4Bytes a_s1 `shiftR` 4
  let a_s1_lower_masked = maskLower4Bytes a_s1
  let a_s2 =
        if a_s1_upper_masked > 9 || s.ccodes.cy == 1
          then ((a_s1_upper_masked + 6) `shiftL` 4) .|. a_s1_lower_masked
          else a_s1

  let ac = if a_lower_masked + 6 > 0xf then 1 else 0
  let cy = if a_s1_upper_masked + 6 > 0xf then 1 else 0

  let ccodes = s.ccodes{ac = ac, cy = cy}

  put s{a = a_s2, ccodes = ccodes}

  addPC 1

pchl :: State8080M State8080
pchl = do
  s <- get
  setPC $ concatBytesBE s.h s.l
