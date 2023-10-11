{-# LANGUAGE OverloadedRecordDot #-}

module BitwiseInstructions where

import Control.Monad.State
import Data.Binary (Word8)
import Data.Bits (Bits (bit, xor), (.&.), (.^.), (.|.))
import States
import Utils
import ArithInstructions (getCCodes)

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

oram :: State8080M State8080
oram = do
  s <- get

  let adr = concatBytesBE s.h s.l
  let byte = getByteAtAdr s.program adr

  let (res, cc) = bitwiseOp (.|.) s.a byte
  put s{a = res, pc = s.pc + 2, ccodes = s.ccodes{si = cc.si, cy = cc.cy, z = cc.z, p = cc.p}}
  return s

xraa :: State8080M State8080
xraa = do
  addPC 1
  s <- get
  let (res, cc) = bitwiseOp xor s.a s.a
  let cc' = cc{ac = s.ccodes.ac}

  put s{a = res, ccodes = cc'}
  return s

-- TODO: maybe change this to a function that uses state
bitwiseOp :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> (Word8, CCState)
bitwiseOp op operand1 operand2 = (res, cc)
 where
  res = op operand1 operand2
  cc = getCCodes False False res
