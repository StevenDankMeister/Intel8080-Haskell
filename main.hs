{-# LANGUAGE ViewPatterns #-}
import Text.Printf (printf)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hFileSize)
import Data.ByteString as BS (ByteString, hGet, tail, head, take, index, empty, uncons)
import Data.Binary (Word8)
import Data.Map (Map, fromList, (!?))

type Opcode = String
type Byte = Word8
type OPSize = Int
type Instruction = (Opcode, OPSize, ByteString -> String)

immediate2ToString :: ByteString -> String
immediate2ToString ops = printf "%02x%02x" (BS.index ops 2) (BS.index ops 1)

immediate1ToString :: ByteString -> String
immediate1ToString ops = printf "%02x" (BS.index ops 1)

noArg :: b -> String
noArg = const ""

instructionMap :: Map Byte Instruction 
instructionMap = fromList [
        (0x00, ("NOP",     1, noArg)),
        (0x01, ("LXI B, ", 3, immediate2ToString)),
        (0x02, ("STAX B",  1, noArg)),
        (0x03, ("INX B",   1, noArg)),
        (0x04, ("INR B",   1, noArg)),
        (0x05, ("DCR B",   1, noArg)),
        (0x06, ("MVI B, ", 2, immediate1ToString)),
        (0x07, ("RLC",     1, noArg)),

        (0x09, ("DAD B",   1, noArg)),
        (0x0a, ("LDAX B",  1, noArg)),
        (0x0b, ("DCX B",   1, noArg)),
        (0x0c, ("INR C",   1, noArg)),
        (0x0d, ("DCR C",   1, noArg)),
        (0x0e, ("MVI C, ", 2, immediate1ToString)),
        (0x0f, ("RRC",     1, noArg)),

        (0x11, ("LXI D, ", 3, immediate2ToString)),
        (0x12, ("STAX D",  1, noArg)),
        (0x13, ("INX D",   1, noArg)),
        (0x14, ("INR D",   1, noArg)),
        (0x15, ("DCR D",   1, noArg)),
        (0x16, ("MVI D, ", 2, immediate1ToString)),
        (0x17, ("RAL",     1, noArg)),

        (0x19, ("DAD D",   1, noArg)),
        (0x1a, ("LDAX D",  1, noArg)),
        (0x1b, ("DCX D",   1, noArg)),
        (0x1c, ("INR E",   1, noArg)),
        (0x1d, ("DCR E",   1, noArg)),
        (0x1e, ("MVI E, ", 2, immediate1ToString)),
        (0x1f, ("RAR",     1, noArg)),

        (0x21, ("LXI H, ", 3, immediate2ToString)),
        (0x22, ("SHLD ",   3, immediate2ToString)),
        (0x23, ("INX H",   1, noArg)),
        (0x24, ("INR H",   1, noArg)),
        (0x25, ("DCR H",   1, noArg)),
        (0x26, ("MVI H, ", 2, immediate1ToString)),
        (0x27, ("DAA",     1, noArg))
    ]

dissasembleOp :: ByteString -> Int -> IO Int
dissasembleOp ops pc = do putStr (printf "0x%04x" pc ++ " ")
                          let code = BS.head ops
                          let op = instructionMap !? code
                          case op of
                            Just (ins, size, args) -> do putStrLn (ins ++ args ops)
                                                         return size
                            Nothing -> do putStrLn "instruction not implemented"
                                          return 1

-- loopThroughOps :: ByteString -> Int -> IO ()
loopThroughOps :: ByteString -> Int -> IO ()
loopThroughOps (uncons -> Nothing) _ = return ()
loopThroughOps ops pc = do opbytes <- dissasembleOp ops pc
                           loopThroughOps (BS.tail ops) (pc + opbytes)
                           return ()
                           

main :: IO Integer
main = do f <- openFile "space-invaders.rom" ReadMode
          size <- hFileSize f
          buffer <- hGet f (fromIntegral size)
          print size
          let pc = 1
          loopThroughOps buffer pc
          return 0





