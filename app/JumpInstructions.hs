{-# LANGUAGE OverloadedRecordDot #-}

module JumpInstructions where

import Control.Monad.State
import Data.Binary (Word16)
import StackInstructions
import States
import Utils

jmp :: State8080M State8080
jmp = do
  s <- get
  let adr = nextTwoBytesToWord16BE s.program s.pc
  put s{pc = adr}
  return s

jnz :: Word16 -> State8080M State8080
jnz adr = do
  s <- get
  ( if s.ccodes.z == 0x01
      then put s{pc = adr}
      else put s{pc = s.pc + 3}
    )
  return s

ret :: State8080M State8080
ret = do
  s <- get
  lo <- stackPop
  hi <- stackPop

  let adr = concatBytesBE hi lo
  s <- get
  put s{pc = adr + 1}
  return s

call :: Word16 -> State8080M State8080
call adr = do
  s <- get
  let (hi, lo) = word16ToWord8s s.pc

  stackPush hi
  stackPush lo

  s <- get

  put s{pc = adr}
  return s
