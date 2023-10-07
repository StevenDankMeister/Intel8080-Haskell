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
  if s.ccodes.z == 0x0
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

jz :: Word16 -> State8080M State8080
jz adr = do
  s <- get
  if s.ccodes.z == 0x01
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

jnc :: Word16 -> State8080M State8080
jnc adr = do
  s <- get
  if s.ccodes.cy == 0
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

jpe :: Word16 -> State8080M State8080
jpe adr = do
  s <- get
  if s.ccodes.p == 0x1
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

jp :: Word16 -> State8080M State8080
jp adr = do
  s <- get
  if s.ccodes.si == 0
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

jc :: Word16 -> State8080M State8080
jc adr = do
  s <- get
  if s.ccodes.cy == 1
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

jpo :: Word16 -> State8080M State8080
jpo adr = do
  s <- get
  if s.ccodes.p == 0
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

jm :: Word16 -> State8080M State8080
jm adr = do
  s <- get
  if s.ccodes.si == 1
    then put s{pc = adr}
    else put s{pc = s.pc + 3}
  return s

cc :: State8080M State8080
cc = do
  s <- get
  ifCall (s.ccodes.cy == 1)

-- let adr = nextTwoBytesToWord16BE s.program s.pc
--
-- if s.ccodes.cy == 1
--   then do
--     call adr
--     get
--   else do
--     put s{pc = s.pc + 3}
--     return s

cpo :: State8080M State8080
cpo = do
  s <- get
  ifCall (s.ccodes.p == 0)

-- s <- get
-- let adr = nextTwoBytesToWord16BE s.program s.pc
--
-- if s.ccodes.p == 0
--   then do
--     call adr
--     get
--   else do
--     put s{pc = s.pc + 3}
--     return s

cm :: State8080M State8080
cm = do
  s <- get
  ifCall (s.ccodes.si == 1)

-- cm = do
--   s <- get
--   let adr = nextTwoBytesToWord16BE s.program s.pc
--
--   if s.ccodes.s == 1
--     then do
--       call adr
--       get
--     else do
--       put s{pc = s.pc + 3}
--       return s

cnz :: State8080M State8080
cnz = do
  s <- get
  ifCall (s.ccodes.z == 0)

cnc :: State8080M State8080
cnc = do
  s <- get
  ifCall (s.ccodes.cy == 0)

ifCall :: Bool -> State8080M State8080
ifCall bool = do
  s <- get
  if bool
    then do
      let adr = nextTwoBytesToWord16BE s.program s.pc
      call adr
      get
    else do
      put s{pc = s.pc + 3}
      return s

ret :: State8080M State8080
ret = do
  lo <- stackPop
  hi <- stackPop

  let adr = concatBytesBE hi lo
  s <- get
  put s{pc = adr + 3}
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
