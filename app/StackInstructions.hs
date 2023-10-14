{-# LANGUAGE OverloadedRecordDot #-}

module StackInstructions where

import Control.Monad.State (MonadIO (liftIO), MonadState (state), StateT (runStateT), evalStateT, get, modify, put, return, runState)
import Data.Binary (Word8)
import States (
  State8080 (a, b, c, ccodes, d, e, h, l, pc, program, sp, stack),
  State8080M,
 )
import Utils (addPC, byteToFlags, flagsToByte, getByteAtAdr, getNNextByte, insertIntoByteString, isEmpty, putAt)

xthl :: State8080M State8080
xthl = do
  s <- get
  let l = getByteAtAdr s.program s.sp
  let h = getByteAtAdr s.program $ s.sp + 1
  let l_old = s.l
  let h_old = s.h

  putAt l_old s.sp
  putAt h_old $ s.sp + 1

  s <- get

  put s{l = l, h = h}
  addPC 1

stackPush :: Word8 -> State8080M State8080
stackPush byte = do
  s <- get
  let mem = insertIntoByteString byte s.program (fromIntegral (s.sp - 1))
  put s{stack = s.stack ++ [byte], program = mem, sp = s.sp - 1}
  return s

stackPopRegisterB :: State8080M State8080
stackPopRegisterB = do
  c <- stackPop
  b <- stackPop

  s <- get
  put s{b = b, c = c, pc = s.pc + 1}

  return s

stackPushRegisterB :: State8080M State8080
stackPushRegisterB = do
  s <- get
  stackPush s.b
  stackPush s.c

  s <- get
  put s{pc = s.pc + 1}
  return s

stackPushRegisterD :: State8080M State8080
stackPushRegisterD = do
  s <- get
  stackPush s.d
  stackPush s.e

  s <- get
  put s{pc = s.pc + 1}
  return s

stackPopRegisterD :: State8080M State8080
stackPopRegisterD = do
  e <- stackPop
  d <- stackPop

  s <- get
  put s{d = d, e = e, pc = s.pc + 1}

  return s

stackPushRegisterH :: State8080M State8080
stackPushRegisterH = do
  s <- get
  stackPush s.h
  stackPush s.l

  s <- get
  put s{pc = s.pc + 1}
  return s

stackPopRegisterH :: State8080M State8080
stackPopRegisterH = do
  l <- stackPop
  h <- stackPop

  s <- get
  put s{h = h, l = l, pc = s.pc + 1}

  return s

stackPushPSW :: State8080M State8080
stackPushPSW = do
  s <- get
  let psw = flagsToByte s.ccodes

  stackPush s.a
  stackPush psw

  s <- get
  put s{pc = s.pc + 1}
  return s

stackPop :: State8080M Word8
stackPop = do
  s <- get
  let popped = getNNextByte s.program s.sp 0
  -- Stack is just for debugging so we need this
  -- when pop instructions are used before push
  let stack = if isEmpty s.stack then s.stack else Prelude.init s.stack
  put s{stack = stack, sp = s.sp + 1}
  return popped

stackPopPSW :: State8080M State8080
stackPopPSW = do
  flags <- stackPop
  a <- stackPop

  let ccodes = byteToFlags flags

  s <- get
  put s{a = a, ccodes = ccodes, pc = s.pc + 1}

  return s
