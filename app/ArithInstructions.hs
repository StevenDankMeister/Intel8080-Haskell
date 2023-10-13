{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module ArithInstructions where

import Control.Monad.ST (runST)
import Control.Monad.State
import Data.Binary (Word16, Word32, Word8)
import Data.Bits (Bits (complement, complementBit, popCount, (.|.)), shiftR, xor, (.&.))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (ModName (ModName), addCorePlugin)
import Macros (addPC_T, getRecFieldValue_T, getState_T, put_T, recUpd_T)
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
 where
  (res8, _) = arithOp (-) reg 1
  ccodes = getCCodes (maskLower4Bytes reg == 0) (toEnum $ fromIntegral st.ccodes.cy) res8

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
  put s{a = res, ccodes = ccodes}
  addPC 1

addRegisterA :: Word8 -> State8080M State8080
addRegisterA reg = do
  s <- get
  let (res, ccodes) = addRegister s.a reg
  put s{a = res, ccodes = ccodes}
  addPC 1

adcRegisterA :: Word8 -> State8080M State8080
adcRegisterA reg = do
  s <- get
  let carry = s.ccodes.cy
  if carry == 0
    then do
      addRegisterA reg
    else do
      let (res, ccodes_c) = addRegister reg carry
      let (total, ccodes_t) = addRegister s.a res
      let ccodes = ccodes_t{cy = ccodes_c.cy .|. ccodes_t.cy, ac = ccodes_c.ac .|. ccodes_t.ac}
      put s{a = total, ccodes = ccodes}
      addPC 1

subRegisterA :: Word8 -> State8080M State8080
subRegisterA reg = do
  s <- get
  let (res, ccodes) = subRegister s.a reg
  put s{a = res, ccodes = ccodes}
  addPC 1

subRegister :: Word8 -> Word8 -> (Word8, CCState)
subRegister operand1 operand2 = (res8, ccodes)
 where
  (res8, res16) = arithOp (-) operand1 operand2
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

inrX :: String -> Q Exp
inrX x = do
  (s, get_stmt) <- getState_T
  (x', get_value_x) <- getRecFieldValue_T x s

  let (res', ccodes') = (mkName "res", mkName "ccodes")
  let inc_call = AppE (VarE 'gets) (AppE (VarE 'incRegister) get_value_x)
  let inc_result = BindS (TupP [VarP res', VarP ccodes']) inc_call
  let put_expr = put_T s [(x', VarE res')] -- , (ccodes', VarE ccodes')]
  return $ LamE [] $ DoE Nothing [get_stmt, inc_result, put_expr, addPC_T 1]

inrM :: State8080M State8080
inrM = do
  s <- get
  let (res, ccodes) = incRegister (getMem s) s
  toMem res
  put s{ccodes = ccodes}
  addPC 1

sbbAWithRegister :: Word8 -> State8080M State8080
sbbAWithRegister reg = do
  s <- get
  let (a, ccodes) = subRegisterWithCarry s.a reg s.ccodes.cy
  put s{a = a, ccodes = ccodes}
  addPC 1

subRegisterWithCarry :: Word8 -> Word8 -> Word8 -> (Word8, CCState)
subRegisterWithCarry reg operand carry = (res, ccodes')
 where
  operand_twos = complement (operand + carry) + 1
  (res, ccodes) = addRegister reg operand_twos
  carry_flipped = ccodes.cy `xor` 0x01
  -- TODO: DOES IT ACTUALLY WORK???
  aux_carry
    | operand + carry == 16 = 0
    | reg .&. 0x10 /= operand .&. 0x10 = 1
    | otherwise = 0
  ccodes' = ccodes{cy = carry_flipped, ac = aux_carry}

incRegister :: Word8 -> State8080 -> (Word8, CCState)
incRegister reg s = (res, ccodes')
 where
  (res, ccodes) = addRegister reg 1
  -- This instruction does not modify
  -- the carry flag so preserve the old one
  ccodes' = ccodes{cy = s.ccodes.cy}

addRegister :: Word8 -> Word8 -> (Word8, CCState)
addRegister operand1 operand2 = (res8, ccodes)
 where
  (res8, res16) = arithOp (+) operand1 operand2
  op1_masked = maskLower4Bytes operand1
  op2_masked = maskLower4Bytes operand2
  ccodes = getCCodes (op1_masked + op2_masked == 0xf) (res16 > 0xff) res8

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
