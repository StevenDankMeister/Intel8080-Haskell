{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.State (MonadIO (liftIO), MonadState (state), State, StateT (runStateT), evalStateT, get, modify, put, return, runState)
import Data.Binary (Binary (), Word16, Word32, Word8)
import Data.Bits (Bits (complementBit, popCount, shiftL, (.&.), (.|.)), shiftR, xor)
import Data.ByteString as BS (ByteString, append, empty, hGet, head, index, init, length, pack, singleton, snoc, split, splitAt, tail, take, uncons)
import Data.Char (intToDigit)
import Data.Map (Map, fromList, size, (!?))
import Disassembler (Byte, dissasembleOp)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (ReadMode))
import Numeric (showHex)
import System.IO (openFile)
import Text.Printf (printf)

showHexList :: [Word8] -> [String]
showHexList = map (`showHex` "")

data CCState = CCState
  { cy :: Word8,
    ac :: Word8,
    si :: Word8,
    z :: Word8,
    p :: Word8
  }

data State8080 = State8080
  { a :: Word8,
    b :: Word8,
    c :: Word8,
    d :: Word8,
    e :: Word8,
    h :: Word8,
    l :: Word8,
    sp :: Word16,
    pc :: Word16,
    stack :: [Word8], -- This is just for printing
    program :: ByteString,
    ccodes :: CCState,
    inte :: Word8
  }

instance Show State8080 where
  show :: State8080 -> String
  show state =
    "State8080 { "
      ++ "a = "
      ++ showHex (a state) ""
      ++ ", "
      ++ "b = "
      ++ showHex (b state) ""
      ++ ", "
      ++ "c = "
      ++ showHex (c state) ""
      ++ ", "
      ++ "d = "
      ++ showHex (d state) ""
      ++ ", "
      ++ "e = "
      ++ showHex (e state) ""
      ++ ", "
      ++ "h = "
      ++ showHex (h state) ""
      ++ ", "
      ++ "l = "
      ++ showHex (l state) ""
      ++ ", "
      ++ "sp = "
      ++ showHex (sp state) ""
      ++ ", "
      ++ "pc = "
      ++ showHex (pc state) ""
      ++ ", "
      ++ "stack = "
      ++ show (showHexList (stack state))
      ++ ", "
      ++ "program = "
      ++ show (BS.head (program state))
      ++ ", "
      ++ "ccodes = "
      ++ show (ccodes state)
      ++ ", "
      ++ "inte = "
      ++ show (inte state)
      ++ " }"

instance Show CCState where
  show :: CCState -> String
  show ccstate =
    "CCState { "
      ++ "cy = "
      ++ showHex (cy ccstate) ""
      ++ ", "
      ++ "ac = "
      ++ showHex (ac ccstate) ""
      ++ ", "
      ++ "si = "
      ++ showHex (si ccstate) ""
      ++ ", "
      ++ "z = "
      ++ showHex (z ccstate) ""
      ++ ", "
      ++ "p = "
      ++ showHex (p ccstate) ""
      ++ " }"

type State8080M = StateT State8080 IO

instructionNotImplemented :: State8080 -> a
instructionNotImplemented s = error ("\nInstruction not implemented: \n        " ++ show s)

emulateProgram :: State8080M State8080
emulateProgram = do
  s <- get
  let pc = fromIntegral s.pc
  _ <- liftIO (dissasembleOp (s.program) pc)
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
    | op == 0x00 -> put s {pc = s.pc + 1} >> return s
    | op == 0x01 -> lxi "B"
    | op == 0x05 -> dcr "B"
    | op == 0x06 -> movI "B"
    | op == 0x11 -> lxi "D"
    | op == 0x19 -> dad "D"
    | op == 0x0e -> movI "C"
    | op == 0x1a -> ldax "D"
    | op == 0x21 -> lxi "H"
    | op == 0x23 -> inx "H"
    | op == 0x26 -> movI "H"
    | op == 0x29 -> dad "H"
    | op == 0x31 -> lxi "SP"
    | op == 0x32 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        sta adr
    | op == 0x36 -> movI "M"
    | op == 0x6f -> mov "L" "A"
    | op == 0x77 -> mov "M" "A"
    | op == 0xc2 -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        jnz adr
    | op == 0xc3 -> jmp
    | op == 0xc5 -> stackPushRegister "B"
    | op == 0xc9 -> ret
    | op == 0xcd -> do
        let adr = nextTwoBytesToWord16BE s.program s.pc
        call adr
    | op == 0xd3 -> out
    | op == 0xd5 -> stackPushRegister "D"
    | op == 0xe1 -> stackPopRegister "H"
    | op == 0xeb -> xchg
    | op == 0xe5 -> stackPushRegister "H"
    | op == 0xfe -> cpi (getNNextByte s.program s.pc 1)
    | op == 0xff -> rst 7
    | otherwise -> do instructionNotImplemented s

-- TODO: IMPLEMENT THIS
out :: State8080M State8080
out = do
  s <- get
  put s {pc = s.pc + 2}
  return s

xchg :: State8080M State8080
xchg = do
  s <- get
  let h = s.h
  let d = s.d

  let l = s.l
  let e = s.e

  put s {pc = s.pc + 1, h = d, d = h, l = e, e = l}
  return s

dad :: String -> State8080M State8080
dad "H" = do
  s <- get
  let hl = fromIntegral (concatBytesBE s.h s.l) :: Word32
  let res = hl + hl
  let carry = if res > 0xffff then 1 else 0

  let res' = fromIntegral res :: Word16

  let (h, l) = word16ToWord8s res'

  put s {h = h, l = l, pc = s.pc + 1, ccodes = s.ccodes {cy = carry}}
  return s
dad "D" = do
  s <- get
  let hl = fromIntegral (concatBytesBE s.h s.l) :: Word32
  let de = fromIntegral (concatBytesBE s.d s.e) :: Word32
  let res = hl + de
  let carry = if res > 0xffff then 1 else 0

  let res' = fromIntegral res :: Word16

  let (h, l) = word16ToWord8s res'
  put s {h = h, l = l, pc = s.pc + 1, ccodes = s.ccodes {cy = carry}}
  return s

cpi :: Word8 -> State8080M State8080
cpi byte = do
  s <- get
  let byte = fromIntegral byte
  let res = s.a - byte

  let z = if res == 0 then 1 else 0 :: Word8
  let si = res .&. 0x01
  let p = fromIntegral (complementBit (popCount res `mod` 2) 0)
  let cy = if s.a < byte then 1 else 0 :: Word8

  let b_lower = byte .&. 0x0f
  let a_lower = s.a .&. 0x0f
  let ac = (if a_lower < b_lower then 1 else 0) :: Word8

  let cc = s.ccodes {z = z, si = si, p = p, ac = ac, cy = cy}
  put s {pc = s.pc + 2}
  return s

sta :: Word16 -> State8080M State8080
sta adr = do
  s <- get
  let mem = insertIntoByteString s.a s.program (fromIntegral adr)
  put s {program = mem, pc = s.pc + 3}
  return s

rst :: Int -> State8080M State8080
rst 7 = do
  s <- get
  call 0x38

dcr :: String -> State8080M State8080
dcr "B" = do
  s <- get
  let b = s.b - 0x01

  let z = (if b == 0 then 0x01 else 0x0) :: Word8
  let si = b .&. 0x01

  let p = fromIntegral (complementBit (popCount b `mod` 2) 0)

  let b_lower = s.b .&. 0x0f
  let ac = (if b_lower < 0x01 then 0x01 else 0x0) :: Word8 -- Maybe just hardcode b_lower == 0?
  let cc = s.ccodes {z = z, si = si, p = p, ac = ac}
  put s {b = s.b - 1, ccodes = cc, pc = s.pc + 1}
  return s

call :: Word16 -> State8080M State8080
call adr = do
  s <- get
  let (hi, lo) = word16ToWord8s s.pc

  stackPush hi
  stackPush lo

  s <- get

  put s {pc = adr}
  return s

word16ToWord8s :: Word16 -> (Word8, Word8)
word16ToWord8s w = (fromIntegral (w `shiftR` 8), fromIntegral w)

stackPush :: Word8 -> State8080M State8080
stackPush byte = do
  s <- get
  let mem = insertIntoByteString byte s.program (fromIntegral (s.sp - 1))
  put s {stack = s.stack ++ [byte], program = mem, sp = s.sp - 1}
  return s

stackPushRegister :: String -> State8080M State8080
stackPushRegister "D" = do
  s <- get
  stackPush s.d
  stackPush s.e

  s <- get
  put s {pc = s.pc + 1}
  return s
stackPushRegister "H" = do
  s <- get
  stackPush s.h
  stackPush s.l

  s <- get
  put s {pc = s.pc + 1}
  return s
stackPushRegister "B" = do
  s <- get
  stackPush s.b
  stackPush s.c

  s <- get
  put s {pc = s.pc + 1}
  return s

stackPop :: State8080M Word8
stackPop = do
  s <- get
  let popped = getNNextByte s.program s.sp 0
  put s {stack = Prelude.init s.stack, sp = s.sp + 1}
  return popped

stackPopRegister :: String -> State8080M State8080
stackPopRegister "H" = do
  l <- stackPop
  h <- stackPop

  s <- get
  put s {h = h, l = l, pc = s.pc + 1}

  return s

insertIntoByteString :: Word8 -> ByteString -> Int -> ByteString
insertIntoByteString byte bs n = (BS.init left `snoc` byte) `append` right
  where
    (left, right) = BS.splitAt (n + 1) bs

lxi :: String -> State8080M State8080
-- lxi "B" state = state {b = getNNextByte state 2,
--                        c = getNNextByte state 1,
--                        pc = pc state + 3}

lxi "D" = do
  s <- get
  let d = getNNextByte s.program s.pc 2
  let e = getNNextByte s.program s.pc 1
  put s {d = d, e = e, pc = s.pc + 3}
  return s
lxi "H" = do
  s <- get
  let h = getNNextByte s.program s.pc 2
  let l = getNNextByte s.program s.pc 1
  put s {h = h, l = l, pc = s.pc + 3}
  return s
lxi "SP" = do
  s <- get
  let newSP = nextTwoBytesToWord16BE s.program s.pc
  put s {sp = newSP, pc = s.pc + 3}
  return s

ldax :: String -> State8080M State8080
ldax "D" = do
  s <- get
  let adr = concatBytesBE s.d s.e
  let a = getByteAtAdr s.program adr
  put s {a = a, pc = s.pc + 1}
  return s

inx :: String -> State8080M State8080
inx "H" = do
  s <- get
  let hl = (concatBytesBE s.h s.l) + 1
  let (h, l) = word16ToWord8s hl
  put s {h = h, l = l, pc = s.pc + 2}
  return s

concatBytesBE :: Word8 -> Word8 -> Word16
concatBytesBE hi lo = res
  where
    low = fromIntegral lo
    high = fromIntegral hi
    res = high `shiftL` 8 .|. low

jmp :: State8080M State8080
jmp = do
  s <- get
  let adr = nextTwoBytesToWord16BE s.program s.pc
  put s {pc = adr}
  return s

jnz :: Word16 -> State8080M State8080
jnz adr = do
  s <- get
  ( if s.ccodes.z == 0x1
      then put s {pc = adr}
      else put s {pc = s.pc + 3}
    )
  return s

ret :: State8080M State8080
ret = do
  s <- get
  lo <- stackPop
  hi <- stackPop

  let adr = concatBytesBE hi lo
  s <- get
  put s {pc = adr + 3}
  return s

movI :: String -> State8080M State8080
movI "B" = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s {b = im, pc = s.pc + 2}
  return s
movI "M" = do
  s <- get
  let adr = concatBytesBE s.h s.l
  let im = getNNextByte s.program s.pc 1
  let mem = insertIntoByteString im s.program (fromIntegral adr)
  put s {program = mem, pc = s.pc + 2}
  return s
movI "C" = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s {c = im, pc = s.pc + 2}
  return s
movI "H" = do
  s <- get
  let im = getNNextByte s.program s.pc 1
  put s {h = im, pc = s.pc + 2}
  return s

mov :: String -> String -> State8080M State8080
mov "M" "A" = do
  s <- get
  let adr = concatBytesBE s.h s.l
  let mem = insertIntoByteString s.a s.program (fromIntegral adr)

  put s {program = mem, pc = s.pc + 1}
  return s
mov "L" "A" = do
  s <- get
  put s {l = s.a, pc = s.pc + 1}
  return s

nextTwoBytesToWord16BE :: ByteString -> Word16 -> Word16
nextTwoBytesToWord16BE mem pc = res
  where
    low = fromIntegral (getNNextByte mem pc 1)
    high = fromIntegral (getNNextByte mem pc 2)
    res = high `shiftL` 8 .|. low

getNNextByte :: ByteString -> Word16 -> Int -> Word8
getNNextByte mem pc n = mem `getByteAtAdr` (pc + fromIntegral n)

getByteAtAdr :: ByteString -> Word16 -> Word8
getByteAtAdr mem pc = mem `BS.index` fromIntegral pc

main :: IO (State8080, State8080)
main = do
  f <- openFile "../space-invaders.rom" ReadMode
  size <- hFileSize f
  buffer <- hGet f (fromIntegral size)
  let memory = buffer `BS.append` pack (replicate (0x10000 - fromIntegral size) 0)
  let ccodes = CCState {cy = 0, ac = 0, si = 0, z = 0, p = 0}

  let startState =
        State8080
          { a = 0,
            b = 0,
            c = 0,
            d = 0,
            e = 0,
            h = 0,
            l = 0,
            sp = 0,
            pc = 0,
            program = memory,
            stack = [],
            ccodes = ccodes,
            inte = 0
          }
  runStateT emulateProgram startState

-- Just for some manual testing
test :: IO (State8080, State8080)
test = do
  let memory = pack (replicate 0x10000 0)
  let ccodes = CCState {cy = 0, ac = 0, si = 0, z = 0, p = 0}

  let startState =
        State8080
          { a = 0,
            b = 0,
            c = 0,
            d = 0,
            e = 0,
            h = 0,
            l = 0,
            sp = 0,
            pc = 0,
            program = memory,
            stack = [],
            ccodes = ccodes,
            inte = 0
          }
  runStateT emulateProgram startState
