{-# LANGUAGE OverloadedRecordDot #-}

module BitwiseInstructions where

import Control.Monad.State
import Data.Binary (Word8)
import Data.Bits (Bits (bit, xor), (.&.), (.^.), (.|.))
import States
import Utils
import ArithInstructions (getCCodes)
import Macros (getState_T, getRecFieldValue_T)

-- TODO: rewrite everything inside here...its bad

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
