{-# LANGUAGE OverloadedRecordDot #-}

module MoveInstructions where

import Control.Monad.State
import Data.Binary (Word16)
import States
import Utils

movIB :: State8080M State8080
movIB = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s{b = im, pc = s.pc + 2}
  return s

movIM :: State8080M State8080
movIM = do
  s <- get
  let adr = concatBytesBE s.h s.l
  let im = getNNextByte s.program s.pc 1
  let mem = insertIntoByteString im s.program (fromIntegral adr)
  put s{program = mem, pc = s.pc + 2}
  return s

movIC :: State8080M State8080
movIC = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s{c = im, pc = s.pc + 2}
  return s

movIH :: State8080M State8080
movIH = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s{h = im, pc = s.pc + 2}
  return s

movMA :: State8080M State8080
movMA = do
  s <- get
  let adr = concatBytesBE s.h s.l
  let mem = insertIntoByteString s.a s.program (fromIntegral adr)

  put s{program = mem, pc = s.pc + 1}
  return s

movLA :: State8080M State8080
movLA = do
  s <- get
  put s{l = s.a, pc = s.pc + 1}
  return s

movAH :: State8080M State8080
movAH = do
  s <- get
  put s{a = s.h, pc = s.pc + 1}
  return s

movEH :: State8080M State8080
movEH = do
  s <- get
  put s{e = s.h, pc = s.pc + 1}
  return s

lhld :: Word16 -> State8080M State8080
lhld adr = do
  s <- get
  let l = getByteAtAdr s.program adr
  let h = getByteAtAdr s.program (adr + 1)

  put s{l = l, h = h, pc = s.pc + 3}
  return s
