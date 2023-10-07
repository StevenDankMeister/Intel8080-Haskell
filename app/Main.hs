{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.State (MonadIO (liftIO), MonadState (state), State, StateT (runStateT), evalStateT, get, modify, put, return, runState)
import Data.Binary (Binary (), Word16, Word32, Word8)
import Data.Bits (Bits (complementBit, popCount, shiftL, testBit, (.&.), (.|.)), shiftR, xor)
import Data.ByteString as BS (ByteString, append, empty, hGet, head, index, init, length, pack, singleton, snoc, split, splitAt, tail, take, uncons)
import Data.Char (intToDigit)
import Data.Map (Map, fromList, size, (!?))
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (ReadMode))
import Numeric (showHex)
import System.IO (openFile)
import Text.Printf (printf)

import ArithInstructions
import Disassembler (Byte, dissasembleOp)
import JumpInstructions
import LoadInstructions
import MoveInstructions
import StackInstructions
import States
import Utils

instructionNotImplemented :: State8080 -> a
instructionNotImplemented s = error ("\nInstruction not implemented: \n        " ++ show s)

emulateProgram :: State8080M State8080
emulateProgram = do
  s <- get
  let pc = fromIntegral s.pc
  _ <- liftIO (dissasembleOp s.program pc)
  if pc < BS.length s.program
    then do
      emulateNextOp
      s <- get
      liftIO (print s)
      emulateProgram
    else return s

emulateNextOp :: State8080M State8080
emulateNextOp = do
  s <- get
  let op = program s `getByteAtAdr` s.pc
  if
    | op == 0x00 -> put s{pc = s.pc + 1} >> return s
    | op == 0x01 -> lxiB
    | op == 0x05 -> dcrB
    | op == 0x06 -> movIB
    | op == 0x09 -> dadB
    -- 0x0d
    | op == 0x0e -> movIC
    | op == 0x0f -> rrc
    | op == 0x11 -> lxiD
    | op == 0x13 -> inxD
    | op == 0x19 -> dadD
    | op == 0x1a -> ldaxD
    | op == 0x21 -> lxiH
    | op == 0x23 -> inxH
    | op == 0x26 -> movIH
    | op == 0x29 -> dadH
    | op == 0x31 -> lxiSP
    | op == 0x32 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        sta adr
    | op == 0x36 -> movIM
    | op == 0x3a -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        lda adr
    -- 0x3e
    -- 0x56
    -- 0x5e
    -- 0x66
    -- \| op == 0x5c -> movEH
    | op == 0x6f -> movLA
    | op == 0x77 -> movMA
    -- 0x7a
    -- 0x7b
    | op == 0x7c -> movAH
    -- 0x7e
    | op == 0xc1 -> stackPopRegisterB
    | op == 0xc2 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jnz adr
    | op == 0xc3 -> jmp
    | op == 0xc5 -> stackPushRegisterB
    | op == 0xc6 -> addI (getNNextByte s.program s.pc 1)
    | op == 0xc9 -> ret
    | op == 0xcd -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        call adr
    | op == 0xd1 -> stackPopRegisterD
    | op == 0xd3 -> out
    | op == 0xd5 -> stackPushRegisterD
    | op == 0xe1 -> stackPopRegisterH
    | op == 0xe5 -> stackPushRegisterH
    | op == 0xe6 -> ani (getNNextByte s.program s.pc 1)
    | op == 0xeb -> xchg
    | op == 0xf1 -> stackPopPSW
    | op == 0xf5 -> stackPushPSW
    -- 0xfb
    | op == 0xfe -> cpi (getNNextByte s.program s.pc 1)
    -- \| op == 0xff -> rst 7
    | otherwise -> do instructionNotImplemented s

rrc :: State8080M State8080
rrc = do
  s <- get

  let cy = s.a .&. 0x1
  let mask = cy `shiftL` 7
  let a = (s.a `shiftR` 1) .|. mask

  put s{a = a, pc = s.pc + 1, ccodes = s.ccodes{cy = cy}}
  return s

ani :: Word8 -> State8080M State8080
ani byte = do
  s <- get
  let a = s.a .&. byte
  let sign = getSign a
  let carry = 0
  let zero = if a == 0 then 1 else 0
  let p = getParity a

  put s{a = a, pc = s.pc + 2, ccodes = s.ccodes{si = sign, cy = carry, z = zero, p = p}}
  return s

-- TODO: IMPLEMENT THIS
out :: State8080M State8080
out = do
  s <- get
  put s{pc = s.pc + 2}
  return s

xchg :: State8080M State8080
xchg = do
  s <- get
  let h = s.h
  let d = s.d

  let l = s.l
  let e = s.e

  put s{pc = s.pc + 1, h = d, d = h, l = e, e = l}
  return s

cpi :: Word8 -> State8080M State8080
cpi byte = do
  s <- get
  let res = s.a - byte

  let z = if res == 0 then 1 else 0
  let si = getSign res
  let p = getParity res
  let cy = if s.a < byte then 1 else 0

  let b_lower = byte .&. 0x0f
  let a_lower = s.a .&. 0x0f
  let ac = (if a_lower < b_lower then 1 else 0)

  let cc = s.ccodes{z = z, si = si, p = p, ac = ac, cy = cy}
  put s{pc = s.pc + 2, ccodes = cc}
  return s

sta :: Word16 -> State8080M State8080
sta adr = do
  s <- get
  let mem = insertIntoByteString s.a s.program (fromIntegral adr)
  put s{program = mem, pc = s.pc + 3}
  return s

rst :: Int -> State8080M State8080
rst 7 = do
  s <- get
  call 0x38

main :: IO (State8080, State8080)
main = do
  f <- openFile "../space-invaders.rom" ReadMode
  size <- hFileSize f
  buffer <- hGet f (fromIntegral size)
  let memory = buffer `BS.append` pack (replicate (0x10000 - fromIntegral size) 0)
  let ccodes = CCState{cy = 0, ac = 0, si = 0, z = 0, p = 0}

  let startState =
        State8080
          { a = 0
          , b = 0
          , c = 0
          , d = 0
          , e = 0
          , h = 0
          , l = 0
          , sp = 0
          , pc = 0
          , program = memory
          , stack = []
          , ccodes = ccodes
          , inte = 0
          }
  runStateT emulateProgram startState

-- Just for some manual testing
test :: IO (State8080, State8080)
test = do
  let memory = pack (replicate 0x5 0)
  let testbytes = pack [0xe6, 0xff]
  let ccodes = CCState{cy = 0, ac = 0, si = 0, z = 0, p = 0}

  let test_memory = append testbytes memory
  let startState =
        State8080
          { a = 1
          , b = 0
          , c = 0
          , d = 0
          , e = 0
          , h = 0
          , l = 0
          , sp = 0
          , pc = 0
          , program = test_memory
          , stack = []
          , ccodes = ccodes
          , inte = 0
          }
  runStateT emulateProgram startState
