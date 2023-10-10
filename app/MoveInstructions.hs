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

movBA :: State8080M State8080
movBA = do
  s <- get
  put s{b = s.a, pc = s.pc + 1}
  return s

movBC :: State8080M State8080
movBC = do
  s <- get
  put s{b = s.c, pc = s.pc + 1}
  return s

movBD :: State8080M State8080
movBD = do
  s <- get
  put s{b = s.d, pc = s.pc + 1}
  return s

movBE :: State8080M State8080
movBE = do
  s <- get
  put s{b = s.e, pc = s.pc + 1}
  return s
movBH :: State8080M State8080
movBH = do
  s <- get
  put s{b = s.h, pc = s.pc + 1}
  return s
movBL :: State8080M State8080
movBL = do
  s <- get
  put s{b = s.l, pc = s.pc + 1}
  return s

movBM :: State8080M State8080
movBM = do
  s <- get
  put s{b = getMem s, pc = s.pc + 1}
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

movCB :: State8080M State8080
movCB = do
  s <- get
  put s{c = s.b, pc = s.pc + 1}
  return s

movCD :: State8080M State8080
movCD = do
  s <- get
  put s{c = s.d, pc = s.pc + 1}
  return s

movCE :: State8080M State8080
movCE = do
  s <- get
  put s{c = s.e, pc = s.pc + 1}
  return s

movCH :: State8080M State8080
movCH = do
  s <- get
  put s{c = s.h, pc = s.pc + 1}
  return s

movCL :: State8080M State8080
movCL = do
  s <- get
  put s{c = s.l, pc = s.pc + 1}
  return s

movCM :: State8080M State8080
movCM = do
  s <- get
  put s{c = getMem s, pc = s.pc + 1}
  return s

movCA :: State8080M State8080
movCA = do
  s <- get
  put s{c = s.a, pc = s.pc + 1}
  return s

movDB :: State8080M State8080
movDB = do
  addPC 1
  s <- get
  put s{d = s.b}
  return s

movDC :: State8080M State8080
movDC = do
  addPC 1
  s <- get
  put s{d = s.c}
  return s

movDE :: State8080M State8080
movDE = do
  addPC 1
  s <- get
  put s{d = s.e}
  return s

movDH :: State8080M State8080
movDH = do
  addPC 1
  s <- get
  put s{d = s.h}
  return s

movDL :: State8080M State8080
movDL = do
  addPC 1
  s <- get
  put s{d = s.l}
  return s

movDM :: State8080M State8080
movDM = do
  addPC 1
  s <- get
  put s{d = getMem s}
  return s

movDA :: State8080M State8080
movDA = do
  addPC 1
  s <- get
  put s{d = s.a}
  return s

movEB :: State8080M State8080
movEB = do
  addPC 1
  s <- get
  put s{e = s.b}
  return s

movEC :: State8080M State8080
movEC = do
  addPC 1
  s <- get
  put s{e = s.c}
  return s

movED :: State8080M State8080
movED = do
  addPC 1
  s <- get
  put s{e = s.d}
  return s

movEH :: State8080M State8080
movEH = do
  addPC 1
  s <- get
  put s{e = s.h}
  return s

movEL :: State8080M State8080
movEL = do
  addPC 1
  s <- get
  put s{e = s.l}
  return s

movEM :: State8080M State8080
movEM = do
  addPC 1
  s <- get
  put s{e = getMem s}
  return s

movEA :: State8080M State8080
movEA = do
  addPC 1
  s <- get
  put s{e = s.a}
  return s

movIH :: State8080M State8080
movIH = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s{h = im, pc = s.pc + 2}
  return s

movIA :: State8080M State8080
movIA = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s{a = im, pc = s.pc + 2}
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

movAM :: State8080M State8080
movAM = do
  s <- get
  let adr = concatBytesBE s.h s.l
  let byte = getByteAtAdr s.program adr
  put s{a = byte, pc = s.pc + 1}
  return s

movHB :: State8080M State8080
movHB = do
  addPC 1
  s <- get
  put s{h = s.b}
  return s

movHL :: State8080M State8080
movHL = do
  addPC 1
  s <- get
  put s{h = s.l}
  return s

movHM :: State8080M State8080
movHM = do
  s <- get
  let mem = getNNextByte s.program s.pc 1

  addPC 1
  s <- get
  put s{h = mem}
  return s

movHE :: State8080M State8080
movHE = do
  addPC 1
  s <- get
  put s{h = s.e}
  return s

movHD :: State8080M State8080
movHD = do
  addPC 1
  s <- get
  put s{h = s.d}
  return s

movHC :: State8080M State8080
movHC = do
  addPC 1
  s <- get
  put s{h = s.c}
  return s

movHA :: State8080M State8080
movHA = do
  addPC 1
  s <- get
  put s{h = s.a}
  return s

movLB :: State8080M State8080
movLB = do
  addPC 1
  s <- get
  put s{l = s.b}
  return s

movLC :: State8080M State8080
movLC = do
  addPC 1
  s <- get
  put s{l = s.c}
  return s

movLD :: State8080M State8080
movLD = do
  addPC 1
  s <- get
  put s{l = s.d}
  return s

movLE :: State8080M State8080
movLE = do
  addPC 1
  s <- get
  put s{l = s.e}
  return s

movLH :: State8080M State8080
movLH = do
  addPC 1
  s <- get
  put s{l = s.h}
  return s

movLM :: State8080M State8080
movLM = do
  s <- get
  let byte = getNNextByte s.program s.pc 1

  addPC 1
  put s{l = byte}
  return s

lhld :: Word16 -> State8080M State8080
lhld adr = do
  s <- get
  let l = getByteAtAdr s.program adr
  let h = getByteAtAdr s.program (adr + 1)

  put s{l = l, h = h, pc = s.pc + 3}
  return s
