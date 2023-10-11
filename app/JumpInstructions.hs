{-# LANGUAGE OverloadedRecordDot #-}

module JumpInstructions where

import Control.Monad.State
import Data.Binary (Word16)
import StackInstructions
import States
import Utils

jumpIf :: Bool -> Word16 -> State8080M State8080
jumpIf cond adr = do
  s <- get
  if cond
    then setPC adr
    else addPC 3

jmp :: State8080M State8080
jmp = do
  s <- get
  let adr = nextTwoBytesToWord16BE s.program s.pc
  jumpIf True adr

jnz :: Word16 -> State8080M State8080
jnz adr = do
  s <- get
  jumpIf (s.ccodes.z == 0) adr

jz :: Word16 -> State8080M State8080
jz adr = do
  s <- get
  jumpIf (s.ccodes.z == 0x01) adr

jnc :: Word16 -> State8080M State8080
jnc adr = do
  s <- get
  jumpIf (s.ccodes.cy == 0) adr

jpe :: Word16 -> State8080M State8080
jpe adr = do
  s <- get
  jumpIf (s.ccodes.p == 0x1) adr

jp :: Word16 -> State8080M State8080
jp adr = do
  s <- get
  jumpIf (s.ccodes.si == 0) adr

jc :: Word16 -> State8080M State8080
jc adr = do
  s <- get
  jumpIf (s.ccodes.cy == 1) adr

jpo :: Word16 -> State8080M State8080
jpo adr = do
  s <- get
  jumpIf (s.ccodes.p == 0) adr

jm :: Word16 -> State8080M State8080
jm adr = do
  s <- get
  jumpIf (s.ccodes.si == 1) adr

cc :: State8080M State8080
cc = do
  s <- get
  callIf (s.ccodes.cy == 1)

cpo :: State8080M State8080
cpo = do
  s <- get
  callIf (s.ccodes.p == 0)

cm :: State8080M State8080
cm = do
  s <- get
  callIf (s.ccodes.si == 1)

cnz :: State8080M State8080
cnz = do
  s <- get
  callIf (s.ccodes.z == 0)

cz :: State8080M State8080
cz = do
  s <- get
  callIf (s.ccodes.z == 1)

cnc :: State8080M State8080
cnc = do
  s <- get
  callIf (s.ccodes.cy == 0)

cpe :: State8080M State8080
cpe = do
  s <- get
  callIf (s.ccodes.p == 1)

cp :: State8080M State8080
cp = do
  s <- get
  callIf (s.ccodes.si == 0)

callIf :: Bool -> State8080M State8080
callIf bool = do
  s <- get
  if bool
    then do
      let adr = nextTwoBytesToWord16BE s.program s.pc
      call adr
    else do
      addPC 3

call :: Word16 -> State8080M State8080
call adr = do
  s <- get
  let (hi, lo) = word16ToWord8s s.pc

  stackPush hi
  stackPush lo
  setPC adr

ret :: State8080M State8080
ret = retIf True

rnz :: State8080M State8080
rnz = do
  s <- get
  retIf (s.ccodes.z == 0)

rz :: State8080M State8080
rz = do
  s <- get
  retIf (s.ccodes.z == 1)

rnc :: State8080M State8080
rnc = do
  s <- get
  retIf (s.ccodes.cy == 0)

rc :: State8080M State8080
rc = do
  s <- get
  retIf (s.ccodes.cy == 1)

rpo :: State8080M State8080
rpo = do
  s <- get
  retIf (s.ccodes.p == 0)

rpe :: State8080M State8080
rpe = do
  s <- get
  retIf (s.ccodes.p == 1)

rp :: State8080M State8080
rp = do
  s <- get
  retIf (s.ccodes.si == 0)

rm :: State8080M State8080
rm = do
  s <- get
  retIf (s.ccodes.si == 1)

retIf :: Bool -> State8080M State8080
retIf bool = do
  if bool
    then do
      lo <- stackPop
      hi <- stackPop
      let adr = concatBytesBE hi lo
      retSetPC adr
    else addPC 1

retSetPC :: Word16 -> State8080M State8080
retSetPC adr = setPC $ adr + 3
