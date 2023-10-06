{-# LANGUAGE OverloadedRecordDot #-}

module LoadInstructions where

import Control.Monad.State
import Data.Binary (Word16)
import States
import Utils

lda :: Word16 -> State8080M State8080
lda adr = do
  s <- get
  let res = getByteAtAdr s.program adr
  put s{a = res, pc = s.pc + 3}

  return s

lxiB :: State8080M State8080
lxiB = do
  s <- get
  let b = getNNextByte s.program s.pc 2
  let c = getNNextByte s.program s.pc 1
  put s{b = b, c = c, pc = s.pc + 3}
  return s

lxiD :: State8080M State8080
lxiD = do
  s <- get
  let d = getNNextByte s.program s.pc 2
  let e = getNNextByte s.program s.pc 1
  put s{d = d, e = e, pc = s.pc + 3}
  return s

lxiH :: State8080M State8080
lxiH = do
  s <- get
  let h = getNNextByte s.program s.pc 2
  let l = getNNextByte s.program s.pc 1
  put s{h = h, l = l, pc = s.pc + 3}
  return s

lxiSP :: State8080M State8080
lxiSP = do
  s <- get
  let newSP = nextTwoBytesToWord16BE s.program s.pc
  put s{sp = newSP, pc = s.pc + 3}
  return s

ldaxD :: State8080M State8080
ldaxD = do
  s <- get
  let adr = concatBytesBE s.d s.e
  let a = getByteAtAdr s.program adr
  put s{a = a, pc = s.pc + 1}
  return s
