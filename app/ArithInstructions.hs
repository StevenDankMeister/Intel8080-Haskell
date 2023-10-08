{-# LANGUAGE OverloadedRecordDot #-}

module ArithInstructions where

import Control.Monad.State
import Data.Binary (Word16, Word32, Word8)
import Data.Bits (Bits (complementBit, popCount, (.|.)), (.&.))
import States
import Utils

dcrB :: State8080M State8080
dcrB = do
  s <- get
  let b = s.b - 0x01

  let z = getZero b
  let si = getSign b
  let p = getParity b
  let b_lower = fromIntegral $ s.b .&. 0x0f
  -- TODO: Does this actually work???
  let ac = (if b_lower == 0 then 1 else 0)
  let cc = s.ccodes{z = z, si = si, p = p, ac = ac}
  put s{b = b, ccodes = cc, pc = s.pc + 1}

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

  let (h, l) = word16ToWord8s $ fromIntegral res
  put s{h = h, l = l, pc = s.pc + 1, ccodes = s.ccodes{cy = carry}}
  return s

dadH :: State8080M State8080
dadH = do
  s <- get
  let hl = fromIntegral (concatBytesBE s.h s.l) :: Word32
  let res = hl + hl
  let carry = if res > 0xffff then 1 else 0

  let (h, l) = word16ToWord8s $ fromIntegral res

  put s{h = h, l = l, pc = s.pc + 1, ccodes = s.ccodes{cy = carry}}
  return s

dadD :: State8080M State8080
dadD = do
  s <- get
  let hl = fromIntegral (concatBytesBE s.h s.l) :: Word32
  let de = fromIntegral (concatBytesBE s.d s.e) :: Word32
  let res = hl + de
  let carry = if res > 0xffff then 1 else 0

  let (h, l) = word16ToWord8s $ fromIntegral res
  put
    s
      { h = h
      , l = l
      , pc = s.pc + 1
      , ccodes = s.ccodes{cy = carry}
      }
  return s

addI :: Word8 -> State8080M State8080
addI byte = do
  s <- get
  let a = fromIntegral s.a :: Word16
  let byte_dw = fromIntegral byte :: Word16
  let res = a + byte_dw
  let res' = fromIntegral res :: Word8

  let a_lower = a .|. 0xf
  let byte_lower = byte_dw .|. 0xf

  let aux_carry = if a_lower + byte_lower > 0xf then 1 else 0
  let carry = if res > 0xff then 1 else 0
  let sign = getSign $ fromIntegral res'
  let zero = getZero $ fromIntegral res'
  let parity = getParity res'

  let ccodess = CCState{ac = aux_carry, cy = carry, si = sign, z = zero, p = parity}

  put s{a = res', pc = s.pc + 2, ccodes = ccodess}
  return s

aci :: State8080M State8080
aci = do
  s <- get
  let immediate = fromIntegral $ getNNextByte s.program s.pc 1
  let a = fromIntegral s.a
  let carry_before = fromIntegral s.ccodes.cy

  let res = a + immediate + carry_before :: Word16
  let res' = fromIntegral res :: Word8

  let a_lower = a .|. 0xf
  let byte_lower = immediate .|. 0xf

  let aux_carry = if a_lower + byte_lower + carry_before > 0xf then 1 else 0
  let carry = if res > 0xff then 1 else 0
  let sign = getSign $ fromIntegral res'
  let zero = getZero $ fromIntegral res'
  let parity = getParity res'

  let ccodes = CCState{ac = aux_carry, cy = carry, si = sign, z = zero, p = parity}

  put s{a = res', pc = s.pc + 2, ccodes = ccodes}
  return s

sui :: State8080M State8080
sui = do
  s <- get
  let immediate = fromIntegral $ getNNextByte s.program s.pc 1
  let a = fromIntegral s.a

  let res = a - immediate :: Word16
  let res' = fromIntegral res :: Word8

  let a_lower = a .|. 0xf
  let byte_lower = immediate .|. 0xf

  let aux_carry = if a_lower < byte_lower then 1 else 0
  let carry = if a < immediate then 1 else 0
  let sign = getSign $ fromIntegral res'
  let zero = getZero $ fromIntegral res'
  let parity = getParity res'

  let ccodes = CCState{ac = aux_carry, cy = carry, si = sign, z = zero, p = parity}

  put s{a = res', pc = s.pc + 2, ccodes = ccodes}
  return s

sbi :: State8080M State8080
sbi = do
  s <- get
  let immediate = fromIntegral $ getNNextByte s.program s.pc 1
  let a = fromIntegral s.a
  let carry_before = fromIntegral s.ccodes.cy

  let res = a - immediate - carry_before :: Word16
  let res' = fromIntegral res :: Word8

  let a_lower = a .|. 0xf
  let byte_lower = immediate .|. 0xf

  let aux_carry = if a_lower < byte_lower + carry_before then 1 else 0
  let carry = if a < immediate + carry_before then 1 else 0
  let sign = getSign $ fromIntegral res'
  let zero = getZero $ fromIntegral res'
  let parity = getParity res'

  let ccodes = CCState{ac = aux_carry, cy = carry, si = sign, z = zero, p = parity}

  put s{a = res', pc = s.pc + 2, ccodes = ccodes}
  return s

inrA :: State8080M State8080
inrA = do
  s <- get
  let (res8, res16) = arithOp (+) s.a 1

  let a_lower = s.a .|. 0xf

  let ccodes = getCCodes (a_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{a = res8, pc = s.pc + 1, ccodes = ccodes}
  return s

inrB :: State8080M State8080
inrB = do
  s <- get
  let (res8, res16) = arithOp (+) s.b 1
  let b_lower = s.b .|. 0xf

  let ccodes = getCCodes (b_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{b = res8, pc = s.pc + 1, ccodes = ccodes}
  return s

getCCodes :: Bool -> Bool -> Word8 -> CCState
getCCodes auxcond carrycond byte = res
 where
  aux_carry = if auxcond then 1 else 0
  carry = if carrycond then 1 else 0
  sign = getSign byte
  zero = getZero byte
  parity = getParity byte
  res = CCState{ac = aux_carry, cy = carry, si = sign, z = zero, p = parity}

arithOp :: (Integral a, Integral b, Num c, Num d) => (c -> d -> Word16) -> a -> b -> (Word8, Word16)
arithOp op operand1 operand2 = (res8, res16)
 where
  operand1' = fromIntegral operand1
  operand2' = fromIntegral operand2
  res16 = op operand1' operand2' :: Word16
  res8 = fromIntegral res16 :: Word8
