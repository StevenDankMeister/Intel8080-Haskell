{-# LANGUAGE OverloadedRecordDot #-}

module MoveInstructions where

import Control.Monad.State
import Data.Binary (Word16)
import States
import Utils

movIB :: State8080M State8080
movIB = do
  s <- get
  put s{b = getNextByte s}
  addPC 2

movBA :: State8080M State8080
movBA = do
  s <- get
  put s{b = s.a}
  addPC 1

movBC :: State8080M State8080
movBC = do
  s <- get
  put s{b = s.c}
  addPC 1

movBD :: State8080M State8080
movBD = do
  s <- get
  put s{b = s.d}
  addPC 1

movBE :: State8080M State8080
movBE = do
  s <- get
  put s{b = s.e}
  addPC 1

movBH :: State8080M State8080
movBH = do
  s <- get
  put s{b = s.h}
  addPC 1

movBL :: State8080M State8080
movBL = do
  s <- get
  put s{b = s.l}
  addPC 1

movBM :: State8080M State8080
movBM = do
  s <- get
  put s{b = getMem s}
  addPC 1

movIM :: State8080M State8080
movIM = do
  s <- get
  toMem $ getIM s
  addPC 2

movIC :: State8080M State8080
movIC = do
  s <- get
  put s{c = getIM s}
  addPC 2

movCB :: State8080M State8080
movCB = do
  s <- get
  put s{c = s.b}
  addPC 1

movCD :: State8080M State8080
movCD = do
  s <- get
  put s{c = s.d}
  addPC 1

movCE :: State8080M State8080
movCE = do
  s <- get
  put s{c = s.e}
  addPC 1

movCH :: State8080M State8080
movCH = do
  s <- get
  put s{c = s.h}
  addPC 1

movCL :: State8080M State8080
movCL = do
  s <- get
  put s{c = s.l}
  addPC 1

movCM :: State8080M State8080
movCM = do
  s <- get
  put s{c = getMem s}
  addPC 1

movCA :: State8080M State8080
movCA = do
  s <- get
  put s{c = s.a}
  addPC 1

movDB :: State8080M State8080
movDB = do
  s <- get
  put s{d = s.b}
  addPC 1

movDC :: State8080M State8080
movDC = do
  s <- get
  put s{d = s.c}
  addPC 1

movDE :: State8080M State8080
movDE = do
  s <- get
  put s{d = s.e}
  addPC 1

movDH :: State8080M State8080
movDH = do
  s <- get
  put s{d = s.h}
  addPC 1

movDL :: State8080M State8080
movDL = do
  s <- get
  put s{d = s.l}
  addPC 1

movDM :: State8080M State8080
movDM = do
  s <- get
  put s{d = getMem s}
  addPC 1

movDA :: State8080M State8080
movDA = do
  s <- get
  put s{d = s.a}
  addPC 1

movEB :: State8080M State8080
movEB = do
  s <- get
  put s{e = s.b}
  addPC 1

movEC :: State8080M State8080
movEC = do
  s <- get
  put s{e = s.c}
  addPC 1

movED :: State8080M State8080
movED = do
  s <- get
  put s{e = s.d}
  addPC 1

movEH :: State8080M State8080
movEH = do
  s <- get
  put s{e = s.h}
  addPC 1

movEL :: State8080M State8080
movEL = do
  s <- get
  put s{e = s.l}
  addPC 1

movEM :: State8080M State8080
movEM = do
  s <- get
  put s{e = getMem s}
  addPC 1

movEA :: State8080M State8080
movEA = do
  s <- get
  put s{e = s.a}
  addPC 1

movIH :: State8080M State8080
movIH = do
  s <- get
  let im = getNextByte s
  put s{h = im}
  addPC 2

movIA :: State8080M State8080
movIA = do
  s <- get
  let im = getNextByte s
  put s{a = im}
  addPC 2

movMA :: State8080M State8080
movMA = do
  s <- get
  toMem s.a
  addPC 1

movLA :: State8080M State8080
movLA = do
  s <- get
  put s{l = s.a}
  addPC 1

movAH :: State8080M State8080
movAH = do
  s <- get
  put s{a = s.h}
  addPC 1

movAM :: State8080M State8080
movAM = do
  s <- get
  put s{a = getMem s}
  addPC 1

movHB :: State8080M State8080
movHB = do
  s <- get
  put s{h = s.b}
  addPC 1

movHL :: State8080M State8080
movHL = do
  s <- get
  put s{h = s.l}
  addPC 1

movHM :: State8080M State8080
movHM = do
  s <- get
  let mem = getMem s
  put s{h = mem}
  addPC 1

movHE :: State8080M State8080
movHE = do
  s <- get
  put s{h = s.e}
  addPC 1

movHD :: State8080M State8080
movHD = do
  s <- get
  put s{h = s.d}
  addPC 1

movHC :: State8080M State8080
movHC = do
  s <- get
  put s{h = s.c}
  addPC 1

movHA :: State8080M State8080
movHA = do
  s <- get
  put s{h = s.a}
  addPC 1

movLB :: State8080M State8080
movLB = do
  s <- get
  put s{l = s.b}
  addPC 1

movLC :: State8080M State8080
movLC = do
  s <- get
  put s{l = s.c}
  addPC 1

movLD :: State8080M State8080
movLD = do
  s <- get
  put s{l = s.d}
  addPC 1

movLE :: State8080M State8080
movLE = do
  s <- get
  put s{l = s.e}
  addPC 1

movLH :: State8080M State8080
movLH = do
  s <- get
  put s{l = s.h}
  addPC 1

movLM :: State8080M State8080
movLM = do
  s <- get
  put s{l = getNextByte s}
  addPC 1

movMB :: State8080M State8080
movMB = do
  s <- get
  toMem s.b
  addPC 1

movMC :: State8080M State8080
movMC = do
  s <- get
  toMem s.c
  addPC 1

movMD :: State8080M State8080
movMD = do
  s <- get
  toMem s.d
  addPC 1

movME :: State8080M State8080
movME = do
  s <- get
  toMem s.e
  addPC 1

movMH :: State8080M State8080
movMH = do
  s <- get
  toMem s.h
  addPC 1

movML :: State8080M State8080
movML = do
  s <- get
  toMem s.l
  addPC 1

movAB :: State8080M State8080
movAB = do
  s <- get
  put s{a = s.b}
  addPC 1

movAC :: State8080M State8080
movAC = do
  s <- get
  put s{a = s.c}
  addPC 1

movAD :: State8080M State8080
movAD = do
  s <- get
  put s{a = s.d}
  addPC 1

movAE :: State8080M State8080
movAE = do
  s <- get
  put s{a = s.e}
  addPC 1

movAL :: State8080M State8080
movAL = do
  s <- get
  put s{a = s.l}
  addPC 1

mviD :: State8080M State8080
mviD = do
  s <- get
  put s{d = getNextByte s}
  addPC 2

mviE :: State8080M State8080
mviE = do
  s <- get
  put s{e = getNextByte s}
  addPC 2

mviL :: State8080M State8080
mviL = do
  s <- get
  put s{l = getNextByte s}
  addPC 2

lhld :: Word16 -> State8080M State8080
lhld adr = do
  s <- get
  let l = getByteAtAdr s.program adr
  let h = getByteAtAdr s.program (adr + 1)

  put s{l = l, h = h, pc = s.pc + 3}
  return s
