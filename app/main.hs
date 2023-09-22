import Text.Printf (printf)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hFileSize)
import Data.ByteString as BS (ByteString, hGet, tail, head, take, index, empty, uncons, length, singleton)
import Data.Binary (Word8, Word16)
import Data.Map (Map, fromList, (!?), size)
import Data.Char (intToDigit)
import Data.Bits ( Bits((.|.), shiftL) )


data CCState = CCState {
  cy :: Word8,
  ac :: Word8,
  si :: Word8,
  z  :: Word8,
  p  :: Word8
} deriving Show

data State8080 = State8080 {
  a      :: Word16,
  b      :: Word8,
  c      :: Word8,
  d      :: Word8,
  e      :: Word8,
  h      :: Word8,
  l      :: Word8,
  sp     :: Word16,
  pc     :: Int,
  mem    :: ByteString,
  ccodes :: CCState,
  inte   :: Word8
} deriving Show



instructionNotImplemented :: State8080 -> a
instructionNotImplemented state = error ("Instruction not implemented: " ++ show state {mem = BS.singleton (BS.index (mem state) (pc state))})

emulateProgram :: State8080 -> State8080
emulateProgram state | pc state < BS.length (mem state) = emulateProgram (emulateOp state)
                     | otherwise = state

emulateOp :: State8080 -> State8080
emulateOp state | op == 0x00 = state {pc = pc state + 1}
                | op == 0x01 = loadPairImmediate 'B' state
                | op == 0xc3 = jmp state
                | otherwise = instructionNotImplemented state
          where op = BS.index (mem state) (pc state)

loadPairImmediate :: Char -> State8080 -> State8080
loadPairImmediate 'B' state = state {b = BS.index (mem state) (pc state + 2),
                                     c = BS.index (mem state) (pc state + 1),
                                     pc = pc state + 3}

jmp :: State8080 -> State8080
jmp state = state {pc = fromIntegral adr}
      where adr = b `shiftL` 8 .|. a
            a = fromIntegral (getNNextByte state 1) :: Word16
            b = fromIntegral (getNNextByte state 2) :: Word16

getNNextByte :: State8080 -> Int -> Word8
getNNextByte state n = BS.index (mem state) (pc state + n)

main :: IO State8080
main = do f <- openFile "space-invaders.rom" ReadMode
          size <- hFileSize f
          buffer <- hGet f (fromIntegral size)
          let ccodes = CCState {cy = 0, ac = 0, si = 0, z = 0, p = 0}
          let state = State8080 {
            a = 0, b = 0, c = 0, d = 0,
            e = 0, h = 0, l = 0, sp = 0,
            pc = 0, mem = buffer,
            ccodes = ccodes, inte = 0
          }

          let final_state = emulateProgram state
          return final_state