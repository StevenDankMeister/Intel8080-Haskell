{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.State (MonadIO (liftIO), MonadState (state), State, StateT (runStateT), evalStateT, get, modify, put, return, runState)
import Data.Binary (Binary (), Word16, Word32, Word8)
import Data.Bits (Bits (complementBit, popCount, shiftL, testBit, (.&.), (.|.)), shiftR, xor)
import Data.ByteString as BS (ByteString, append, drop, empty, hGet, head, index, init, length, pack, singleton, snoc, split, splitAt, tail, take, takeWhile, uncons)
import Data.Char (intToDigit)
import Data.Map (Map, fromList, size, (!?))
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (ReadMode))
import Numeric (showHex)
import System.IO (openFile)
import Text.Printf (IsChar (fromChar), printf)

import ArithInstructions
import BitwiseInstructions
import Control.Arrow qualified as BS
import Disassembler (Byte, dissasembleOp, instructionPrintMap)
import JumpInstructions
import LoadInstructions
import MoveInstructions
import StackInstructions
import States
import Utils

-- TODO: Fix this print
instructionNotImplemented :: Word8 -> State8080 -> a
instructionNotImplemented op s =
  error
    ( "\nInstruction not implemented: 0x"
        ++ show (showHexList [op])
        ++ ": "
        ++ string
        ++ "\n        "
        ++ show s
    )
 where
  Just (string, _) = instructionPrintMap Data.Map.!? op

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
    | op == 0x2a -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        lhld adr
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
    | op == 0x7e -> movAM
    | op == 0xb6 -> oram
    | op == 0xc0 -> rnz
    | op == 0xc1 -> stackPopRegisterB
    | op == 0xc2 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jnz adr
    | op == 0xc3 -> jmp
    | op == 0xc4 -> cnz
    | op == 0xc5 -> stackPushRegisterB
    | op == 0xc6 -> addI (getNNextByte s.program s.pc 1)
    | op == 0xc8 -> rz
    | op == 0xc9 -> ret
    | op == 0xca -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jz adr
    | op == 0xcc -> cz
    | op == 0xcd -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        call adr
    | op == 0xce -> aci
    | op == 0xd0 -> rnc
    | op == 0xd1 -> stackPopRegisterD
    | op == 0xd2 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jnc adr
    | op == 0xd3 -> out
    | op == 0xd4 -> cnc
    | op == 0xd5 -> stackPushRegisterD
    | op == 0xd6 -> sui
    | op == 0xd8 -> rc
    | op == 0xda -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jc adr
    | op == 0xdc -> cc
    | op == 0xde -> sbi
    | op == 0xe0 -> rpo
    | op == 0xe1 -> stackPopRegisterH
    | op == 0xe2 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jpo adr
    | op == 0xe4 -> cpo
    | op == 0xe5 -> stackPushRegisterH
    | op == 0xe6 -> ani (getNNextByte s.program s.pc 1)
    | op == 0xe8 -> rpe
    | op == 0xea -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jpe adr
    | op == 0xeb -> xchg
    | op == 0xec -> cpe
    | op == 0xee -> xri
    | op == 0xf0 -> rp
    | op == 0xf1 -> stackPopPSW
    | op == 0xf2 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jp adr
    | op == 0xf4 -> cp
    | op == 0xf5 -> stackPushPSW
    | op == 0xf6 -> ori
    | op == 0xf8 -> rm
    | op == 0xfa -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jm adr
    | op == 0xf9 -> sphl
    -- 0xfb
    | op == 0xfc -> cm
    | op == 0xfe -> cpi (getNNextByte s.program s.pc 1)
    -- \| op == 0xff -> rst 7
    | otherwise -> do instructionNotImplemented op s

rrc :: State8080M State8080
rrc = do
  s <- get

  let cy = s.a .&. 0x1
  let mask = cy `shiftL` 7
  let a = (s.a `shiftR` 1) .|. mask

  put s{a = a, pc = s.pc + 1, ccodes = s.ccodes{cy = cy}}
  return s

-- TODO: IMPLEMENT THIS
out :: State8080M State8080
out = do
  s <- get
  put s{pc = s.pc + 2}
  return s

sphl :: State8080M State8080
sphl = do
  s <- get
  let sp = concatBytesBE s.h s.l
  put s{sp = sp, pc = s.pc + 1}
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

testCPU :: IO (State8080, State8080)
testCPU = do
  f <- openFile "../TST8080.COM" ReadMode
  size <- hFileSize f
  buffer <- hGet f $ fromIntegral size
  -- Program should start at 0x100
  let memory = pack (replicate 0x100 0) `BS.append` buffer `BS.append` pack (replicate (0x10000 - fromIntegral size) 0)
  let ccodes = CCState{cy = 0, ac = 0, si = 0, z = 0, p = 0}

  -- 0xc9 is RET
  let modified_memory = insertIntoByteString 0xc9 memory 0x0005

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
          , pc = 0x100 -- Start at 1x100
          , program = modified_memory
          , stack = []
          , ccodes = ccodes
          , inte = 0
          }
  runStateT runTest startState

runTest :: State8080M State8080
runTest = do
  s <- get
  let pc = fromIntegral s.pc
  -- Print instruction to be executed
  _ <- liftIO (dissasembleOp s.program pc)
  -- Special test behaviour
  if pc == 0x0005
    then
      if s.c == 9
        then do
          let adr = fromIntegral $ concatBytesBE s.d s.e
          let stop_char = fromIntegral $ fromEnum '$'
          let output = BS.takeWhile (/= stop_char) (BS.drop adr s.program)
          liftIO $ print output
          emulateNextOp
          runTest
        else do
          liftIO (print s.e)
          runTest
    else
      if pc < BS.length s.program
        then do
          emulateNextOp
          s <- get
          liftIO (print s)
          runTest
        else return s

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
