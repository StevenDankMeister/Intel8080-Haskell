{-# LANGUAGE OverloadedRecordDot #-}

module ArithInstructions where

import Control.Monad.State
import Data.Binary (Word16, Word32, Word8)
import Data.Bits (Bits (complementBit, popCount), (.&.))
import States
import Utils

dcrB :: State8080M State8080
dcrB = do
  s <- get
  let b = s.b - 0x01

  let z = (if b == 0 then 0x01 else 0x0) :: Word8
  let si = b .&. 0x01

  let p = fromIntegral (complementBit (popCount b `mod` 2) 0)

  let b_lower = s.b .&. 0x0f
  let ac = (if b_lower < 0x01 then 1 else 0) :: Word8 -- Maybe just hardcode b_lower == 0?
  let cc = s.ccodes{z = z, si = si, p = p, ac = ac}
  put s{b = s.b - 1, ccodes = cc, pc = s.pc + 1}
  return s

inxH :: State8080M State8080
inxH = do
  s <- get
  let hl = concatBytesBE s.h s.l + 1
  let (h, l) = word16ToWord8s hl
  put s{h = h, l = l, pc = s.pc + 1}
  return s

inxD :: State8080M State8080
inxD = do
  s <- get
  let de = concatBytesBE s.d s.e + 1
  let (d, e) = word16ToWord8s de
  put s{d = d, e = e, pc = s.pc + 1}
  return s

dadB :: State8080M State8080
dadB = do
  s <- get
  let hl = fromIntegral (concatBytesBE s.h s.l) :: Word32
  let be = fromIntegral (concatBytesBE s.b s.c) :: Word32
  let res = hl + be
  let carry = if res > 0xffff then 1 else 0

  let res' = fromIntegral res :: Word16

  let (h, l) = word16ToWord8s res'
  put s{h = h, l = l, pc = s.pc + 1, ccodes = s.ccodes{cy = carry}}
  return s

dadH :: State8080M State8080
dadH = do
  s <- get
  let hl = fromIntegral (concatBytesBE s.h s.l) :: Word32
  let res = hl + hl
  let carry = if res > 0xffff then 1 else 0

  let res' = fromIntegral res :: Word16

  let (h, l) = word16ToWord8s res'

  put s{h = h, l = l, pc = s.pc + 1, ccodes = s.ccodes{cy = carry}}
  return s

dadD :: State8080M State8080
dadD = do
  s <- get
  let hl = fromIntegral (concatBytesBE s.h s.l) :: Word32
  let de = fromIntegral (concatBytesBE s.d s.e) :: Word32
  let res = hl + de
  let carry = if res > 0xffff then 1 else 0

  let res' = fromIntegral res :: Word16

  let (h, l) = word16ToWord8s res'
  put s{h = h, l = l, pc = s.pc + 1, ccodes = s.ccodes{cy = carry}}
  return s
