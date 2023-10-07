{-# LANGUAGE OverloadedRecordDot #-}

module BitwiseInstructions where

import Control.Monad.State
import Data.Binary (Word8)
import Data.Bits (Bits (bit, xor), (.&.), (.^.), (.|.))
import States
import Utils

ani :: Word8 -> State8080M State8080
ani byte = do
  s <- get

  let (res, cc) = bitwiseOp (.&.) s.a byte

  put s{a = res, pc = s.pc + 2, ccodes = s.ccodes{si = cc.si, cy = cc.cy, z = cc.z, p = cc.p}}
  return s

ori :: State8080M State8080
ori = do
  s <- get

  let immediate = getNNextByte s.program s.pc 1
  let (res, cc) = bitwiseOp (.|.) s.a immediate

  put s{a = res, pc = s.pc + 2, ccodes = s.ccodes{si = cc.si, cy = cc.cy, z = cc.z, p = cc.p}}
  return s

xri :: State8080M State8080
xri = do
  s <- get

  let immediate = getNNextByte s.program s.pc 1
  let (res, cc) = bitwiseOp xor s.a immediate

  put s{a = res, pc = s.pc + 2, ccodes = s.ccodes{si = cc.si, cy = cc.cy, z = cc.z, p = cc.p}}
  return s

bitwiseOp :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> (Word8, CCState)
bitwiseOp op operand1 operand2 = (res, ccstate)
 where
  res = op operand1 operand2
  sign = getSign res
  carry = 0
  zero = if res == 0 then 1 else 0
  p = getParity res
  ccstate = CCState{si = sign, cy = carry, z = zero, p = p, ac = 0}
