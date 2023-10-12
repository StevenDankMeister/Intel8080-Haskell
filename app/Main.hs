{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.State (MonadIO (liftIO), MonadState (state), State, StateT (runStateT), evalStateT, execStateT, get, modify, put, return, runState)
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
import Interrupts
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
  if pc < BS.length s.program
    then do
      liftIO (print s)
      _ <- liftIO (dissasembleOp s.program pc)
      s <- emulateNextOp
      emulateProgram
    else return s

emulateNextOp :: State8080M State8080
emulateNextOp = do
  s <- get
  let op = program s `getByteAtAdr` s.pc
  if
    | op == 0x00 -> nop
    | op == 0x01 -> lxiB
    | op == 0x04 -> inrB
    | op == 0x05 -> dcrB
    | op == 0x06 -> $(mvIX "b") -- movIB
    | op == 0x09 -> dadB
    | op == 0x0c -> inrC
    | op == 0x0d -> dcrC
    | op == 0x0e -> movIC
    | op == 0x0f -> rrc
    | op == 0x11 -> lxiD
    | op == 0x13 -> inxD
    | op == 0x14 -> inrD
    | op == 0x15 -> dcrD
    | op == 0x16 -> mviD
    | op == 0x19 -> dadD
    | op == 0x1a -> ldaxD
    | op == 0x1c -> inrE
    | op == 0x1d -> dcrE
    | op == 0x1e -> mviE
    | op == 0x21 -> lxiH
    | op == 0x23 -> inxH
    | op == 0x24 -> inrH
    | op == 0x25 -> dcrH
    | op == 0x26 -> movIH
    | op == 0x29 -> dadH
    | op == 0x2a -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        lhld adr
    | op == 0x2c -> inrL
    | op == 0x2d -> dcrL
    | op == 0x2e -> mviL
    | op == 0x31 -> lxiSP
    | op == 0x32 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        sta adr
    | op == 0x36 -> movIM
    | op == 0x3a -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        lda adr
    | op == 0x3c -> inrA
    | op == 0x3d -> dcrA
    -- \| op == 0x3e -> movIA
    | op == 0x3e -> $(mvIX "a")
    | op == 0x40 -> nop
    | op == 0x41 -> $(movXY "b" "c") -- movBC
    | op == 0x42 -> $(movXY "b" "d") -- movBD
    | op == 0x43 -> $(movXY "b" "e") -- movBE
    | op == 0x44 -> $(movXY "b" "h") -- movBH
    | op == 0x45 -> $(movXY "b" "l") -- movBL
    | op == 0x46 -> $(movXY "b" "getMem") -- movBM
    | op == 0x47 -> $(movXY "b" "a") -- movBA
    | op == 0x48 -> $(movXY "c" "b") -- movCB
    | op == 0x49 -> nop
    | op == 0x4a -> $(movXY "c" "d") -- movCD
    | op == 0x4b -> $(movXY "c" "e") -- movCE
    | op == 0x4c -> $(movXY "c" "h") -- movCH
    | op == 0x4d -> $(movXY "c" "l") -- movCL
    | op == 0x4e -> $(movXY "c" "getMem") -- movCM
    | op == 0x4f -> $(movXY "c" "a") -- movCA
    | op == 0x50 -> $(movXY "d" "b") -- movDB
    | op == 0x51 -> $(movXY "d" "c") -- movDC
    | op == 0x52 -> nop
    | op == 0x53 -> $(movXY "d" "e") -- movDE
    | op == 0x54 -> $(movXY "d" "h") -- movDH
    | op == 0x55 -> $(movXY "d" "l") -- movDL
    | op == 0x56 -> $(movXY "d" "getMem") -- movDM
    | op == 0x57 -> $(movXY "d" "a") -- movDA
    | op == 0x58 -> $(movXY "e" "b") -- movEB
    | op == 0x59 -> $(movXY "e" "c") -- movEC
    | op == 0x5a -> $(movXY "e" "d") -- movED
    | op == 0x5b -> nop
    | op == 0x5c -> $(movXY "e" "h") -- movEH
    | op == 0x5d -> $(movXY "e" "l") -- movEL
    | op == 0x5e -> $(movXY "e" "getMem") -- movEM
    | op == 0x5f -> $(movXY "e" "a") -- movEA
    | op == 0x60 -> $(movXY "h" "b") -- movHB
    | op == 0x61 -> $(movXY "h" "c") -- movHC
    | op == 0x62 -> $(movXY "h" "d") -- movHD
    | op == 0x63 -> $(movXY "h" "e") -- movHE
    | op == 0x64 -> nop
    | op == 0x65 -> $(movXY "h" "l") -- movHL
    | op == 0x66 -> $(movXY "h" "getMem") -- movHM
    | op == 0x67 -> $(movXY "h" "a") -- movHA
    | op == 0x68 -> $(movXY "l" "b") -- movLB
    | op == 0x69 -> $(movXY "l" "c") -- movLC
    | op == 0x6a -> $(movXY "l" "d") -- movLD
    | op == 0x6b -> $(movXY "l" "e") -- movLE
    | op == 0x6c -> $(movXY "l" "h") -- movLH
    | op == 0x6d -> nop
    | op == 0x6e -> $(movXY "l" "getMem") -- movLM
    | op == 0x6f -> $(movXY "l" "a") -- movLA
    | op == 0x70 -> movMB
    | op == 0x71 -> movMC
    | op == 0x72 -> movMD
    | op == 0x73 -> movME
    | op == 0x74 -> movMH
    | op == 0x75 -> movML
    | op == 0x76 -> hlt
    | op == 0x77 -> movMA
    | op == 0x78 -> $(movXY "a" "b") -- movAB
    | op == 0x79 -> $(movXY "a" "c") -- movAC
    | op == 0x7a -> $(movXY "a" "d") -- movAD
    | op == 0x7b -> $(movXY "a" "e") -- movAE
    | op == 0x7c -> $(movXY "a" "h") -- movAH
    | op == 0x7d -> $(movXY "a" "l") -- movAL
    | op == 0x7e -> $(movXY "a" "getMem") -- movAM
    | op == 0x80 -> addRegisterA s.b
    | op == 0x81 -> addRegisterA s.c
    | op == 0x82 -> addRegisterA s.d
    | op == 0x83 -> addRegisterA s.e
    | op == 0x84 -> addRegisterA s.h
    | op == 0x85 -> addRegisterA s.l
    | op == 0x87 -> addRegisterA s.a
    | op == 0x88 -> adcB
    | op == 0x89 -> adcC
    | op == 0x8a -> adcD
    | op == 0x8b -> adcE
    | op == 0x8c -> adcH
    | op == 0x8d -> adcL
    | op == 0x8f -> adcA
    | op == 0x90 -> subRegisterA s.b
    | op == 0x91 -> subRegisterA s.c
    | op == 0x92 -> subRegisterA s.d
    | op == 0x93 -> subRegisterA s.e
    | op == 0x94 -> subRegisterA s.h
    | op == 0x95 -> subRegisterA s.l
    | op == 0x97 -> subRegisterA s.a
    | op == 0x98 -> sbbAWithRegister s.b
    | op == 0x99 -> sbbAWithRegister s.c
    | op == 0x9a -> sbbAWithRegister s.d
    | op == 0x9b -> sbbAWithRegister s.e
    | op == 0x9c -> sbbAWithRegister s.h
    | op == 0x9d -> sbbAWithRegister s.l
    | op == 0x9e -> sbbAWithRegister $ getMem s
    | op == 0x9f -> sbbAWithRegister s.a
    | op == 0xaf -> xraa
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

nop :: State8080M State8080
nop = addPC 1

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
  -- Special test behaviour
  if pc == 0x0005
    then
      if s.c == 9
        then do
          -- Start scanning from DE until '$' and print
          let adr = fromIntegral $ concatBytesBE s.d s.e
          let stop_char = fromIntegral $ fromEnum '$'
          let output = BS.takeWhile (/= stop_char) (BS.drop adr s.program)
          liftIO $ print output
          emulateNextOp
          runTest
        else do
          liftIO (print s.e)
          -- runTest
          return s
    else
      if pc < BS.length s.program
        then do
          emulateNextOp
          s <- get
          _ <- liftIO (dissasembleOp s.program pc)
          liftIO $ putStrLn ("          " ++ show s)
          runTest
        else return s

-- Just for some manual testing
test :: IO (State8080, State8080)
test = do
  let memory = pack (replicate 0x5 0)
  let testbytes = pack [0x98]
  let ccodes = CCState{cy = 1, ac = 0, si = 0, z = 0, p = 0}

  let test_memory = append testbytes memory
  let startState =
        State8080
          { a = 0
          , b = 1
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
