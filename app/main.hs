{-# LANGUAGE MultiWayIf #-}
import Text.Printf (printf)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hFileSize)
import Data.ByteString as BS (ByteString, hGet, tail, head, take, index, empty, uncons, length, singleton, append, snoc, pack)
import Data.Binary (Word8, Word16)
import Data.Map (Map, fromList, (!?), size)
import Data.Char (intToDigit)
import Data.Bits ( Bits((.|.), shiftL) )
import Disassembler ( dissasembleOp )

data CCState = CCState {
  cy :: Word8,
  ac :: Word8,
  si :: Word8,
  z  :: Word8,
  p  :: Word8
} deriving Show

data State8080 = State8080 {
  a       :: Word16,
  b       :: Word8,
  c       :: Word8,
  d       :: Word8,
  e       :: Word8,
  h       :: Word8,
  l       :: Word8,
  sp      :: Word16,
  pc      :: Int,
  stack   :: ByteString,
  program :: ByteString,
  ccodes  :: CCState,
  inte    :: Word8
} deriving Show



instructionNotImplemented :: State8080 -> a
instructionNotImplemented state = error ("Instruction not implemented: " ++ show state {program = BS.singleton (BS.index (program state) (pc state))})

emulateProgram :: State8080 -> IO State8080
emulateProgram state = do _ <- dissasembleOp (program state) (pc state)
                          (if pc state < BS.length (program state) then emulateProgram (emulateOp state) else return state)

emulateOp :: State8080 -> State8080
emulateOp state | op == 0x00 = state {pc = pc state + 1}
                | op == 0x01 = loadPairImmediate "B" state
                | op == 0x06 = movI "B" state
                | op == 0x31 = loadPairImmediate "SP" state
                | op == 0xc3 = jmp state
                | op == 0xcd = call state
                | otherwise = instructionNotImplemented state
          where op = BS.index (program state) (pc state)

call :: State8080 -> State8080
call state = state {stack = stack state `append` pushed, pc = fromIntegral adr, sp = sp state - 2}
    where adr     = nextTwoBytesToWord16BE state
          hi      = getNNextByte state 3
          lo      = getNNextByte state 2
          pushed  = BS.pack [hi, lo]

loadPairImmediate :: String -> State8080 -> State8080
loadPairImmediate "B" state = state {b = BS.index (program state) (pc state + 2),
                                     c = BS.index (program state) (pc state + 1),
                                     pc = pc state + 3}
loadPairImmediate "SP" state = state {sp = newSP, pc = pc state + 3}
      where newSP = nextTwoBytesToWord16BE state

jmp :: State8080 -> State8080
jmp state = state {pc = fromIntegral adr}
      where adr = nextTwoBytesToWord16BE state

movI :: String -> State8080 -> State8080
movI "B" state = state {b = im, pc = nextpc}
  where im = getNNextByte state 1
        nextpc = pc state + 1

nextTwoBytesToWord16BE :: State8080 -> Word16
nextTwoBytesToWord16BE state = res
  where res = high `shiftL` 8 .|. low
        low = fromIntegral (getNNextByte state 1)
        high = fromIntegral (getNNextByte state 2)

getNNextByte :: State8080 -> Int -> Word8
getNNextByte state n = BS.index (program state) (pc state + n)

main :: IO State8080
main = do f <- openFile "../space-invaders.rom" ReadMode
          size <- hFileSize f
          buffer <- hGet f (fromIntegral size)
          let ccodes = CCState {cy = 0, ac = 0, si = 0, z = 0, p = 0}
          let state = State8080 {
            a = 0, b = 0, c = 0, d = 0,
            e = 0, h = 0, l = 0, sp = 0, stack = empty,
            pc = 0, program = buffer, 
            ccodes = ccodes, inte = 0
          }

          emulateProgram state