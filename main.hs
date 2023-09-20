{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
import Text.Printf (printf)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hFileSize)
import Data.ByteString as BS (ByteString, hGet, tail, head, take, index, empty, uncons, length)
import Data.Binary (Word8)
import Data.Map (Map, fromList, (!?))

type Opcode = String
type Byte = Word8
type OPSize = Int
type Instruction = (Opcode, OPSize, ByteString -> Int -> String)

i2ToString :: ByteString -> Int -> String
i2ToString ops pc = printf "%02x%02x" (BS.index ops pc+2) (BS.index ops pc+1)

i1ToString :: ByteString -> Int -> String
i1ToString ops pc = printf "%02x" (BS.index ops pc+1)

noArg :: p1 -> p2 -> String
noArg _ _ = ""

instructionPrintMap :: Map Byte Instruction
instructionPrintMap = fromList [
        (0x00, ("NOP",     1, noArg)),
        (0x01, ("LXI B, ", 3, i2ToString)),
        (0x02, ("STAX B",  1, noArg)),
        (0x03, ("INX B",   1, noArg)),
        (0x04, ("INR B",   1, noArg)),
        (0x05, ("DCR B",   1, noArg)),
        (0x06, ("MVI B, ", 2, i1ToString)),
        (0x07, ("RLC",     1, noArg)),

        (0x09, ("DAD B",   1, noArg)),
        (0x0a, ("LDAX B",  1, noArg)),
        (0x0b, ("DCX B",   1, noArg)),
        (0x0c, ("INR C",   1, noArg)),
        (0x0d, ("DCR C",   1, noArg)),
        (0x0e, ("MVI C, ", 2, i1ToString)),
        (0x0f, ("RRC",     1, noArg)),

        (0x11, ("LXI D, ", 3, i2ToString)),
        (0x12, ("STAX D",  1, noArg)),
        (0x13, ("INX D",   1, noArg)),
        (0x14, ("INR D",   1, noArg)),
        (0x15, ("DCR D",   1, noArg)),
        (0x16, ("MVI D, ", 2, i1ToString)),
        (0x17, ("RAL",     1, noArg)),

        (0x19, ("DAD D",   1, noArg)),
        (0x1a, ("LDAX D",  1, noArg)),
        (0x1b, ("DCX D",   1, noArg)),
        (0x1c, ("INR E",   1, noArg)),
        (0x1d, ("DCR E",   1, noArg)),
        (0x1e, ("MVI E, ", 2, i1ToString)),
        (0x1f, ("RAR",     1, noArg)),

        (0x21, ("LXI H, ", 3, i2ToString)),
        (0x22, ("SHLD ",   3, i2ToString)),
        (0x23, ("INX H",   1, noArg)),
        (0x24, ("INR H",   1, noArg)),
        (0x25, ("DCR H",   1, noArg)),
        (0x26, ("MVI H, ", 2, i1ToString)),
        (0x27, ("DAA",     1, noArg)),

        (0x29, ("DAD H",   1, noArg)),
        (0x2a, ("LHLD ",   3, i2ToString)),
        (0x2b, ("DCX H",   1, noArg)),
        (0x2c, ("INR L",   1, noArg)),
        (0x2d, ("DCR L",   1, noArg)),
        (0x2e, ("MVI L, ", 2, i1ToString)),
        (0x2f, ("CMA",     1, noArg)),

        (0x31, ("LXI SP, ", 3, i2ToString)),
        (0x32, ("STA ",     2, i1ToString)),
        (0x33, ("INX SP",   1, noArg)),
        (0x34, ("INR M",    1, noArg)),
        (0x35, ("DCR M",    1, noArg)),
        (0x36, ("MVI M, ",  2, i1ToString)),
        (0x37, ("STC",      1, noArg)),

        (0x39, ("DAD SP",   1, noArg)),
        (0x3a, ("LDA ",     3, i2ToString)),
        (0x3b, ("DCX SP",   1, noArg)),
        (0x3c, ("INR A",    1, noArg)),
        (0x3d, ("DCR A",    1, noArg)),
        (0x3e, ("MVI A, ",  2, i1ToString)),
        (0x3f, ("CMC",      1, noArg)),
        (0x41, ("MOV B, C", 1, noArg)),
        (0x42, ("MOV B, D", 1, noArg)),
        (0x43, ("MOV B, E", 1, noArg)),
        (0x44, ("MOV B, H", 1, noArg)),
        (0x45, ("MOV B, L", 1, noArg)),
        (0x46, ("MOV B, M", 1, noArg)),
        (0x47, ("MOV B, A", 1, noArg)),
        (0x48, ("MOV C, B", 1, noArg)),
        (0x49, ("MOV C, C", 1, noArg)),
        (0x4a, ("MOV C, D", 1, noArg)),
        (0x4b, ("MOV C, E", 1, noArg)),
        (0x4c, ("MOV C, H", 1, noArg)),
        (0x4d, ("MOV C, L", 1, noArg)),
        (0x4e, ("MOV C, M", 1, noArg)),
        (0x4f, ("MOV C, A", 1, noArg)),
        (0x50, ("MOV D, B", 1, noArg)),
        (0x51, ("MOV D, C", 1, noArg)),
        (0x52, ("MOV D, D", 1, noArg)),
        (0x53, ("MOV D, E", 1, noArg)),
        (0x54, ("MOV D, H", 1, noArg)),
        (0x55, ("MOV D, L", 1, noArg)),
        (0x56, ("MOV D, M", 1, noArg)),
        (0x57, ("MOV D, A", 1, noArg)),
        (0x58, ("MOV E, B", 1, noArg)),
        (0x59, ("MOV E, C", 1, noArg)),
        (0x5a, ("MOV E, D", 1, noArg)),
        (0x5b, ("MOV E, E", 1, noArg)),
        (0x5c, ("MOV E, H", 1, noArg)),
        (0x5d, ("MOV E, L", 1, noArg)),
        (0x5e, ("MOV E, M", 1, noArg)),
        (0x5f, ("MOV E, A", 1, noArg)),
        (0x60, ("MOV H, B", 1, noArg)),
        (0x61, ("MOV H, C", 1, noArg)),
        (0x62, ("MOV H, D", 1, noArg)),
        (0x63, ("MOV H, E", 1, noArg)),
        (0x64, ("MOV H, H", 1, noArg)),
        (0x65, ("MOV H, L", 1, noArg)),
        (0x66, ("MOV H, M", 1, noArg)),
        (0x67, ("MOV H, A", 1, noArg)),
        (0x68, ("MOV L, B", 1, noArg)),
        (0x69, ("MOV L, C", 1, noArg)),
        (0x6a, ("MOV L, D", 1, noArg)),
        (0x6b, ("MOV L, E", 1, noArg)),
        (0x6c, ("MOV L, H", 1, noArg)),
        (0x6d, ("MOV L, L", 1, noArg)),
        (0x6e, ("MOV L, M", 1, noArg)),
        (0x6f, ("MOV L, A", 1, noArg))
    ]

dissasembleOp :: ByteString -> Int -> IO Int
dissasembleOp ops pc = do putStr (printf "0x%04x" pc ++ " ")
                          let code = BS.index ops pc
                          let op = instructionPrintMap !? code
                          case op of
                            Just (ins, size, args) -> do putStrLn (ins ++ args ops pc)
                                                         return size
                            Nothing -> do putStrLn "instruction not implemented"
                                          return 1

loopThroughOps :: ByteString -> Int -> IO ()
loopThroughOps (uncons -> Nothing) _ = return ()
loopThroughOps ops pc = do opbytes <- dissasembleOp ops pc
                           if | pc + opbytes < BS.length ops -> loopThroughOps ops (pc + opbytes)
                              | otherwise -> return ()


main :: IO Integer
main = do f <- openFile "space-invaders.rom" ReadMode
          size <- hFileSize f
          buffer <- hGet f (fromIntegral size)
          print size
          let pc = 0
          loopThroughOps buffer pc
          return 0

