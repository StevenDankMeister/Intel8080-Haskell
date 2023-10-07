{-# LANGUAGE OverloadedRecordDot #-}

module StackInstructions where

import Control.Monad.State (MonadIO (liftIO), MonadState (state), StateT (runStateT), evalStateT, get, modify, put, return, runState)
import Data.Binary (Word8)
import States (
  State8080 (a, b, c, ccodes, d, e, h, l, pc, program, sp, stack),
  State8080M,
 )
import Utils (byteToFlags, flagsToByte, getNNextByte, insertIntoByteString)

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
  put s{stack = Prelude.init s.stack, sp = s.sp + 1}
  return popped

stackPopPSW :: State8080M State8080
stackPopPSW = do
  flags <- stackPop
  a <- stackPop

  let ccodes = byteToFlags flags

  s <- get
  put s{a = a, ccodes = ccodes, pc = s.pc + 1}

  return s
