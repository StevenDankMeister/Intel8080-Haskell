{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE InstanceSigs #-}
import Text.Printf (printf)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hFileSize)
import Data.ByteString as BS (ByteString, hGet, tail, head, take, index, empty, uncons, length, singleton, append, snoc, pack)
import Data.Binary (Word8, Word16)
import Data.Map (Map, fromList, (!?), size)
import Data.Char (intToDigit)
import Data.Bits ( Bits((.|.), shiftL) )
import Disassembler ( dissasembleOp, Byte )
import Control.Monad.State (State, get, put, runState, MonadState (state), return, StateT (runStateT), MonadIO (liftIO))

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
  stack   :: ByteString,
  program :: ByteString,
  ccodes  :: CCState,
  inte    :: Word8
}

instance Show State8080 where
  show :: State8080 -> String
  show state = "State8080 { " ++
               "a = " ++ show (a state) ++ ", " ++
               "b = " ++ show (b state) ++ ", " ++
               "c = " ++ show (c state) ++ ", " ++
               "d = " ++ show (d state) ++ ", " ++
               "e = " ++ show (e state) ++ ", " ++
               "h = " ++ show (h state) ++ ", " ++
               "l = " ++ show (l state) ++ ", " ++
               "sp = " ++ show (sp state) ++ ", " ++
               "pc = " ++ show (pc state) ++ ", " ++
               "stack = " ++ show (stack state) ++ ", " ++               -- Probably dont need this?
               "program = " ++ show (BS.head (program state)) ++ ", " ++ -- TODO: fix this
               "ccodes = " ++ show (ccodes state) ++ ", " ++
               "inte = " ++ show (inte state) ++
               " }"

type State8080M = StateT State8080 IO

-- instructionNotImplemented :: State8080M State8080 -> a
-- instructionNotImplemented = error ("Instruction not implemented: " ++ show state {program = BS.singleton ((program state) `pcIndex` (pc state))})
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
-- emulateProgram (State8080 s) = do _ <- dissasembleOp (program s) (pc s)

-- emulateProgram state = do _ <- dissasembleOp (program state) (pc state)
--                           (if pc state < BS.length (program state) then emulateProgram (emulateOp state) else return state)

emulateNextOp :: State8080M State8080
emulateNextOp = do s <- get
                   let op = program s `pcIndex` s.pc
                   if | op == 0x00 -> put s {pc = s.pc + 1} >> return s
                      | op == 0xc3 -> jmp
                      -- | op == 0x06 -> movI "B"
                -- | op == 0x01 = lxi "B" state
                -- | op == 0x06 = movI "B" state
                      -- | op == 0x11 -> lxi "D"
                -- | op == 0x21 = lxi "H" state
                      -- | op == 0x31 -> lxi "SP"
                      -- | op == 0xcd -> call
                      | otherwise  -> do instructionNotImplemented s
                -- | otherwise = instructionNotImplemented state
          -- where op = program state `BS.index` pc state

pcIndex :: ByteString -> Word16 -> Word8
pcIndex mem pc = mem `BS.index` fromIntegral pc

-- getNNextByte :: (Integral a) => State8080 -> a -> Word8
-- getNNextByte state n = program state `pcIndex` (pc state + n)

-- call :: State8080M State8080
-- call = do s <- get
--           let adr = fromIntegral (nextTwoBytesToWord16BE s)
--           let hi  = getNNextByte s 2
--           let lo  = getNNextByte s 1
--           let pushed = BS.pack [hi, lo]
--           put s {stack = s.stack `append` pushed, pc = adr, sp = s.sp - 2}
--           return s
  -- state {stack = stack state `append` pushed, pc = fromIntegral adr, sp = sp state - 2}

    -- where adr     = nextTwoBytesToWord16BE state
    --       hi      = getNNextByte state 3
    --       lo      = getNNextByte state 2
    --       pushed  = BS.pack [hi, lo]

-- lxi :: String -> State8080M State8080
-- lxi "B" state = state {b = getNNextByte state 2,
--                        c = getNNextByte state 1,
--                        pc = pc state + 3}

-- lxi "D" = do s <- get 
--              let d = getNNextByte s 1
-- state {d = getNNextByte state 2,
--                        e = getNNextByte state 1,
--                        pc = pc state + 3}

-- lxi "H" state = state {h = getNNextByte state 2,
--                        l = getNNextByte state 1,
--                        pc = pc state + 3}
-- lxi "SP" = do s <- get
--               let newSP = nextTwoBytesToWord16BE s
--               put s {sp = newSP, pc = s.pc + 3}
--               return s

-- lxi "SP" state = state {sp = newSP, pc = pc state + 3}
--       where newSP = nextTwoBytesToWord16BE state

jmp :: State8080M State8080
jmp = do s <- get
         adr <- nextTwoBytesToWord16BE
         put s {pc = adr}
         return s
-- jmp state = state {pc = fromIntegral adr}
--       where adr = nextTwoBytesToWord16BE state

-- movI :: String -> State8080 -> State8080
-- movI "B" state = state {b = im, pc = nextpc}
--   where im = getNNextByte state 1
--         nextpc = pc state + 1
-- movI :: String -> State8080M State8080
-- movI "B" = do s <- get
--               let im = getNNextByte s 1
--               put s {b = im, pc = pc s + 1}
--               return s
              -- s { b = im, pc = pc s + 1 }

-- nextTwoBytesToWord16BE :: State8080 -> Word16
-- nextTwoBytesToWord16BE state = res
--   where res = high `shiftL` 8 .|. low
--         low = fromIntegral (getNNextByte state 1)
--         high = fromIntegral (getNNextByte state 2)
nextTwoBytesToWord16BE :: State8080M Word16
nextTwoBytesToWord16BE = do low  <- fmap fromIntegral (getNNextByte 1)
                            high <- fmap fromIntegral (getNNextByte 2)
                            return (high `shiftL` 8 .|. low)


getNNextByte :: Word16 -> State8080M Word8
getNNextByte n = do s <- get
                    return (s.program `pcIndex` (s.pc + n))
-- getNNextByte state n = state.program `pcIndex` (state.pc + n)
-- getNNextByte :: ByteString -> Int -> Word8
-- getNNextByte b n = do s <- get
--                     let next = 5 :: Word8
--                     return s

main :: IO (State8080, State8080)
main = do f <- openFile "../space-invaders.rom" ReadMode
          size <- hFileSize f
          buffer <- hGet f (fromIntegral size)
          let ccodes = CCState {cy = 0, ac = 0, si = 0, z = 0, p = 0}
          let startState = State8080 {
            a = 0, b = 0, c = 0, d = 0,
            e = 0, h = 0, l = 0, sp = 0, stack = empty,
            pc = 0, program = buffer,
            ccodes = ccodes, inte = 0
          }
          runStateT emulateProgram startState