{-# LANGUAGE OverloadedRecordDot #-}

module BitwiseInstructions where

import ArithInstructions (getCCodes)
import Control.Monad.State
import Data.Binary (Word8)
import Data.Bits (Bits (bit, shiftR, xor), complement, shiftL, shiftR, (.&.), (.^.), (.|.))
import Macros (getRecFieldValue_T, getState_T)
import States
import Utils

rlc :: State8080M State8080
rlc = do
  s <- get
  let prev_7 = (s.a .&. 0x80) `shiftR` 7
  let a = (s.a `shiftL` 1) .|. prev_7
  let ccodes = s.ccodes{cy = prev_7}

  put s{a = a, ccodes = ccodes}
  addPC 1

ral :: State8080M State8080
ral = do
  s <- get
  let prev_7 = (s.a .&. 0x80) `shiftR` 7
  let a = ((s.a `shiftL` 1) .&. 0xfe) .|. s.ccodes.cy
  let ccodes = s.ccodes{cy = prev_7}

  put s{a = a, ccodes = ccodes}
  addPC 1

rar :: State8080M State8080
rar = do
  s <- get
  let prev_0 = s.a .&. 0x01
  let prev_7 = s.a .&. 0x80
  let a = ((s.a `shiftR` 1) .&. 0x7f) .|. prev_7
  let ccodes = s.ccodes{cy = prev_0}

  put s{a = a, ccodes = ccodes}
  addPC 1

bitwiseAI :: (Word8 -> Word8 -> Word8) -> Word8 -> State8080M State8080
bitwiseAI operator im = do
  bitwiseA operator im
  -- bitwiseA already increases pc with 1
  -- so just increase with one more
  addPC 1

bitwiseA :: (Word8 -> Word8 -> Word8) -> Word8 -> State8080M State8080
bitwiseA operator reg = do
  s <- get
  let (res, cc) = bitwiseOp operator s.a reg
  -- Keep previous aux carry
  let cc' = cc{ac = s.ccodes.ac}
  put s{a = res, ccodes = cc'}
  addPC 1

-- TODO: maybe change this to a function that uses state
bitwiseOp :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> (Word8, CCState)
bitwiseOp op operand1 operand2 = (res, cc)
 where
  res = op operand1 operand2
  cc = getCCodes False False res

cma :: State8080M State8080
cma = do
  s <- get
  put s{a = complement s.a}
  addPC 1
