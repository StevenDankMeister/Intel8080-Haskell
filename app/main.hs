{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE InstanceSigs #-}
import Text.Printf (printf)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hFileSize)
import Data.ByteString as BS (ByteString, hGet, tail, head, take, index, empty, uncons, length, singleton, append, snoc, pack, split, splitAt)
import Data.Binary (Word8, Word16)
import Data.Map (Map, fromList, (!?), size)
import Data.Char (intToDigit)
import Data.Bits ( Bits((.|.), shiftL, (.&.), popCount, complementBit), shiftR )
import Disassembler ( dissasembleOp, Byte )
import Control.Monad.State (State, get, put, runState, MonadState (state), return, StateT (runStateT), MonadIO (liftIO))
import Numeric (showHex)
import System.Posix.Internals (puts)

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
  pc      :: Word16,
  stack   :: [Word8], -- This is just for printing
  program :: ByteString,
  ccodes  :: CCState,
  inte    :: Word8
}

instance Show State8080 where
  show :: State8080 -> String
  show state = "State8080 { " ++
               "a = " ++ showHex (a state) "" ++ ", " ++
               "b = " ++ showHex (b state) "" ++ ", " ++
               "c = " ++ showHex (c state) "" ++ ", " ++
               "d = " ++ showHex (d state) "" ++ ", " ++
               "e = " ++ showHex (e state) "" ++ ", " ++
               "h = " ++ showHex (h state) "" ++ ", " ++
               "l = " ++ showHex (l state) "" ++ ", " ++
               "sp = " ++ showHex (sp state) "" ++ ", " ++
               "pc = " ++ showHex (pc state) "" ++ ", " ++
               "stack = " ++ show (stack state) ++ ", " ++
               "program = " ++ show (BS.head (program state)) ++ ", " ++
               "ccodes = " ++ show (ccodes state) ++ ", " ++
               "inte = " ++ show (inte state) ++
               " }"

type State8080M = StateT State8080 IO


instructionNotImplemented :: State8080 -> a
instructionNotImplemented s = error ("\nInstruction not implemented: \n        " ++ show s)


emulateProgram :: State8080M State8080
emulateProgram = do s <- get
                    let pc = fromIntegral s.pc
                    _ <- liftIO (dissasembleOp (s.program) pc)
                    if pc < BS.length s.program then
                      do emulateNextOp
                         emulateProgram
                    else return s


emulateNextOp :: State8080M State8080
emulateNextOp = do s <- get
                   let op = program s `pcIndex` s.pc
                   if | op == 0x00 -> put s {pc = s.pc + 1} >> return s
                      | op == 0x01 -> lxi "B"
                      | op == 0xc3 -> jmp
                      | op == 0x05 -> dcr "B"
                      | op == 0x06 -> movI "B"
                -- | op == 0x06 = movI "B" state
                      | op == 0x11 -> lxi "D"
                      | op == 0x1a -> ldax "D"
                      | op == 0x21 -> lxi "H"
                      | op == 0x31 -> lxi "SP"
                      | op == 0xcd -> do let adr = nextTwoBytesToWord16BE s.program s.pc
                                         call adr
                      | op == 0xff -> rst 7
                      | otherwise  -> do instructionNotImplemented s

rst :: Int -> State8080M State8080
rst 7 = do s <- get
           call 0x38

dcr :: String -> State8080M State8080
dcr "B" = do s <- get
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
call adr = do s <- get
              let (hi, lo) = word16ToWord8s s.pc

              stackPush hi
              stackPush lo

              s <- get

              put s {pc = adr}
              return s

word16ToWord8s :: Word16 -> (Word8, Word8)
word16ToWord8s w = (fromIntegral (w `shiftR` 8), fromIntegral w)

stackPush :: Word8 -> State8080M State8080
stackPush byte = do s <- get
                    let mem = insertIntoByteString byte s.program (fromIntegral s.pc)
                    put s {stack = s.stack ++ [byte], program = mem}
                    return s

insertIntoByteString :: Word8 -> ByteString -> Int -> ByteString
insertIntoByteString byte bs n = (left `snoc` byte) `append` right
  where (left, right) = BS.splitAt n bs

lxi :: String -> State8080M State8080
-- lxi "B" state = state {b = getNNextByte state 2,
--                        c = getNNextByte state 1,
--                        pc = pc state + 3}

lxi "D" = do s <- get
             let d = getNNextByte s.program s.pc 2
             let e = getNNextByte s.program s.pc 1
             put s {d = d, e = e, pc = s.pc + 3}
             return s
lxi "H" = do s <- get
             let h = getNNextByte s.program s.pc 2
             let l = getNNextByte s.program s.pc 1
             put s {h = h, l = l, pc = s.pc + 3}
             return s


lxi "SP" = do s <- get
              let newSP = nextTwoBytesToWord16BE s.program s.pc
              put s {sp = newSP, pc = s.pc + 3}
              return s


ldax :: String -> State8080M State8080
ldax "D" = do s <- get
              let a = concatBytes s.d s.e
              put s {a = a, pc = s.pc + 1}
              return s


concatBytes :: Word8 -> Word8 -> Word16
concatBytes x y = res
     where low = fromIntegral y
           high = fromIntegral x
           res = high `shiftL` 8 .|. low


jmp :: State8080M State8080
jmp = do s <- get
         let adr = nextTwoBytesToWord16BE s.program s.pc
         put s {pc = adr}
         return s


movI :: String -> State8080M State8080
movI "B" = do s <- get
              let im = getNNextByte s.program s.pc 1
              put s {b = im, pc = s.pc + 2}
              return s


mov :: String -> String -> State8080M State8080
mov "M" "A" = do s <- get
                 return s


nextTwoBytesToWord16BE :: ByteString -> Word16 -> Word16
nextTwoBytesToWord16BE mem pc = res
     where low = fromIntegral (getNNextByte mem pc 1)
           high = fromIntegral (getNNextByte mem pc 2)
           res = high `shiftL` 8 .|. low


getNNextByte :: ByteString -> Word16 -> Int -> Word8
getNNextByte mem pc n = mem `pcIndex` (pc + fromIntegral n)


pcIndex :: ByteString -> Word16 -> Word8
pcIndex mem pc = mem `BS.index` fromIntegral pc


main :: IO (State8080, State8080)
main = do f <- openFile "../space-invaders.rom" ReadMode
          size <- hFileSize f
          buffer <- hGet f (fromIntegral size)
          let ccodes = CCState {cy = 0, ac = 0, si = 0, z = 0, p = 0}
          let startState = State8080 {
            a = 0, b = 0, c = 0, d = 0,
            e = 0, h = 0, l = 0, sp = 0,
            pc = 0, program = buffer, stack = [],
            ccodes = ccodes, inte = 0
          }
          runStateT emulateProgram startState
