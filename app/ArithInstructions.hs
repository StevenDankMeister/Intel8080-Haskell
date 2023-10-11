{-# LANGUAGE OverloadedRecordDot #-}

module ArithInstructions where

import Control.Monad.ST (runST)
import Control.Monad.State
import Data.Binary (Word16, Word32, Word8)
import Data.Bits (Bits (complementBit, popCount, (.|.), complement), (.&.))
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

dcrC :: State8080M State8080
dcrC = do
  addPC 1
  s <- get
  let (res8, _) = arithOp (-) s.c 1
  let cc = getCCodes (maskLower4Bytes s.c - 1 == 0) (toEnum $ fromIntegral s.ccodes.cy) res8

  put s{c = res8, ccodes = cc}
  return s


dcrA :: State8080M State8080
dcrA = do
  addPC 1
  s <- get
  let (res8, cc) = dcrRegister s.a s

  put s{a = res8, ccodes = cc}
  return s

dcrD :: State8080M State8080
dcrD = do
  addPC 1
  s <- get
  let (res8, cc) = dcrRegister s.d s

  put s{d = res8, ccodes = cc}
  return s

dcrH :: State8080M State8080
dcrH = do
  addPC 1
  s <- get
  let (res8, cc) = dcrRegister s.h s

  put s{h = res8, ccodes = cc}
  return s

dcrE :: State8080M State8080
dcrE = do
  addPC 1
  s <- get
  let (res8, cc) = dcrRegister s.e s

  -- test

  put s{e = res8, ccodes = cc}
  return s

dcrL :: State8080M State8080
dcrL = do 
  addPC 1
  s <- get
  let (res8, cc) = dcrRegister s.l s

  put s{l = res8, ccodes = cc}
  return s

dcrRegister :: Word8 -> State8080 -> (Word8, CCState)
dcrRegister reg st = (res8, ccodes)
  where (res8, _) = arithOp (-) reg 1
        ccodes = getCCodes (maskLower4Bytes reg - 1 == 0) (toEnum $ fromIntegral st.ccodes.cy) res8


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

addB :: State8080M State8080
addB = do
  s <- get
  let (res, ccodes) = addRegister s.a s.b
  put s{a=res, ccodes = ccodes}
  addPC 1

addC :: State8080M State8080
addC = do
  s <- get
  addRegisterA s.c
  addPC 1
  
addD :: State8080M State8080
addD = do
  s <- get
  addRegisterA s.d
  addPC 1

addE :: State8080M State8080
addE = do
  s <- get
  addRegisterA s.e
  addPC 1

addH :: State8080M State8080
addH = do
  s <- get
  addRegisterA s.h
  addPC 1

addL :: State8080M State8080
addL = do
  s <- get
  addRegisterA s.l
  addPC 1

addA :: State8080M State8080
addA = do
  s <- get
  addRegisterA s.a
  addPC 1

addRegisterA :: Word8 -> State8080M State8080
addRegisterA reg = do
  s <- get
  let (res, ccodes) = addRegister s.a reg
  put s{a=res, ccodes = ccodes}
  return s

addRegister :: Word8 -> Word8 -> (Word8, CCState)
addRegister operand1 operand2 = (res8, ccodes)
  where (res8, res16) = arithOp (+) operand1 operand2
        op1_masked = maskLower4Bytes operand1
        op2_masked = maskLower4Bytes operand2
        ccodes = getCCodes (op1_masked + op2_masked == 0xf) (res16 > 0xff) res8

subB :: State8080M State8080
subB = do
  s <- get
  subRegisterA s.b
  addPC 1

subC :: State8080M State8080
subC = do
  s <- get
  subRegisterA s.c
  addPC 1

subD :: State8080M State8080
subD = do
  s <- get
  subRegisterA s.d
  addPC 1
  
subE :: State8080M State8080
subE = do
  s <- get
  subRegisterA s.e
  addPC 1

subH :: State8080M State8080
subH = do
  s <- get
  subRegisterA s.h
  addPC 1

subL :: State8080M State8080
subL = do
  s <- get
  subRegisterA s.l
  addPC 1

subA :: State8080M State8080
subA = do
  s <- get
  subRegisterA s.a
  addPC 1

adcB :: State8080M State8080
adcB = do
  s <- get
  adcRegisterA s.b s.ccodes.cy
  addPC 1

adcC :: State8080M State8080
adcC = do
  s <- get
  adcRegisterA s.c s.ccodes.cy
  addPC 1

adcD :: State8080M State8080
adcD = do
  s <- get
  adcRegisterA s.d s.ccodes.cy
  addPC 1

adcE :: State8080M State8080
adcE = do
  s <- get
  adcRegisterA s.e s.ccodes.cy
  addPC 1

adcH :: State8080M State8080
adcH = do
  s <- get
  adcRegisterA s.h s.ccodes.cy
  addPC 1

adcL :: State8080M State8080
adcL = do
  s <- get
  adcRegisterA s.l s.ccodes.cy
  addPC 1

adcA :: State8080M State8080
adcA = do
  s <- get
  adcRegisterA s.a s.ccodes.cy
  addPC 1

sbbB :: State8080M State8080
sbbB = do
  s <- get
  sbbRegisterA s.b s.ccodes.cy
  addPC 1

sbbC :: State8080M State8080
sbbC = do
  s <- get
  sbbRegisterA s.c s.ccodes.cy
  addPC 1

sbbD :: State8080M State8080
sbbD = do
  s <- get
  sbbRegisterA s.d s.ccodes.cy
  addPC 1
  
sbbE :: State8080M State8080
sbbE = do
  s <- get
  sbbRegisterA s.e s.ccodes.cy
  addPC 1

sbbH :: State8080M State8080
sbbH = do
  s <- get
  sbbRegisterA s.h s.ccodes.cy
  addPC 1

sbbL :: State8080M State8080
sbbL = do
  s <- get
  sbbRegisterA s.l s.ccodes.cy
  addPC 1

sbbM :: State8080M State8080
sbbM = do
  s <- get
  let mem = getMem s
  sbbRegisterA mem s.ccodes.cy
  addPC 1

sbbA :: State8080M State8080
sbbA = do
  s <- get
  sbbRegisterA s.a s.ccodes.cy
  addPC 1

sbbRegisterA :: Word8 -> Word8 -> State8080M State8080
sbbRegisterA reg carry = do
  if carry == 0 then do
    subRegisterA reg
  else do
    s <- get
    -- TODO: WORKS FOR NOW?? BUT REFACTOR
    let reg_twos = complement reg + 1
    let (res, ccodes_c) = subRegister reg_twos carry
    let (total, ccodes_t) = subRegister s.a (complement res + 1)
    let ccodes = ccodes_t{cy = ccodes_c.cy .|. ccodes_t.cy, ac = ccodes_c.ac .|. ccodes_t.ac}
    put s{a = total, ccodes = ccodes}
    return s

adcRegisterA :: Word8 -> Word8 -> State8080M State8080
adcRegisterA reg carry = do
  s <- get
  if carry == 0 then do
    addRegisterA reg
  else do
    let (res, ccodes_c) = addRegister reg carry
    let (total, ccodes_t) = addRegister s.a res
    let ccodes = ccodes_t{cy = ccodes_c.cy .|. ccodes_t.cy, ac = ccodes_c.ac .|. ccodes_t.ac}
    put s{a = total, ccodes = ccodes}
    return s
    
subRegisterA :: Word8 -> State8080M State8080
subRegisterA reg = do
  s <- get
  let (res, ccodes) = subRegister s.a reg
  put s{a=res, ccodes = ccodes}
  return s

subRegister :: Word8 -> Word8 -> (Word8, CCState)
subRegister operand1 operand2 = (res8, ccodes)
  where (res8, res16) = arithOp (-) operand1 operand2
        op1_masked = maskLower4Bytes operand1
        op2_masked = maskLower4Bytes operand2
        ccodes = getCCodes (op1_masked < op2_masked) (operand1 < operand2) res8

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

inrB :: State8080M State8080
inrB = do
  s <- get
  let (res8, res16) = arithOp (+) s.b 1
  let b_lower = s.b .|. 0xf

  let ccodes = getCCodes (b_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{b = res8, pc = s.pc + 1, ccodes = ccodes}
  return s

inrD :: State8080M State8080
inrD = do
  addPC 1
  s <- get
  let (res8, res16) = arithOp (+) s.d 1
  let d_lower = maskLower4Bytes s.d

  let ccodes = getCCodes (d_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{d = res8, ccodes = ccodes}
  return s

inrC :: State8080M State8080
inrC = do
  addPC 1
  s <- get
  let (res8, res16) = arithOp (+) s.c 1
  let c_lower = maskLower4Bytes s.c

  let ccodes = getCCodes (c_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{c = res8, ccodes = ccodes}
  return s

inrE :: State8080M State8080
inrE = do
  addPC 1
  s <- get
  let (res8, res16) = arithOp (+) s.e 1
  let e_lower = maskLower4Bytes s.e

  let ccodes = getCCodes (e_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{e = res8, ccodes = ccodes}
  return s

inrH :: State8080M State8080
inrH = do
  addPC 1
  s <- get
  let (res8, res16) = arithOp (+) s.h 1
  let h_lower = maskLower4Bytes s.h

  let ccodes = getCCodes (h_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{h = res8, ccodes = ccodes}
  return s

inrL :: State8080M State8080
inrL = do
  addPC 1
  s <- get
  let (res8, res16) = arithOp (+) s.l 1
  let l_lower = maskLower4Bytes s.l

  let ccodes = getCCodes (l_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{l = res8, ccodes = ccodes}
  return s

inrM :: State8080M State8080
inrM = do
  addPC 1
  s <- get
  let mem = getMem s
  let (res8, res16) = arithOp (+) mem 1
  let mem_lower = maskLower4Bytes mem

  let ccodes = getCCodes (mem_lower + 1 > 0xf) (res16 > 0xff) res8

  toMem mem
  s <- get

  put s{ccodes = ccodes}
  return s

inrA :: State8080M State8080
inrA = do
  addPC 1
  s <- get
  let (res8, res16) = arithOp (+) s.a 1
  let a_lower = maskLower4Bytes s.a

  let ccodes = getCCodes (a_lower + 1 > 0xf) (res16 > 0xff) res8

  put s{a = res8, ccodes = ccodes}
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
