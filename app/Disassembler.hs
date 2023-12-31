{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Disassembler where

import Data.Binary (Word8)
import Data.ByteString as BS (
  ByteString,
  hGet,
  index,
  length,
  uncons,
 )
import Data.Map (Map, fromList, (!?))
import System.IO (IOMode (ReadMode), hFileSize, openFile)
import Text.Printf (printf)

i2ToString :: ByteString -> Int -> String
i2ToString ops pc = printf "%02x%02x" (BS.index ops (pc + 2)) (BS.index ops (pc + 1))

i1ToString :: ByteString -> Int -> String
i1ToString ops pc = printf "%02x" (BS.index ops (pc + 1))

noArg :: p1 -> p2 -> String
noArg _ _ = ""

type Opcode = String
type Byte = Word8
type OPSize = Int
type Instruction = (Opcode, OPSize)

instructionPrintMap :: Map Byte Instruction
instructionPrintMap =
  fromList
    [ (0x00, ("NOP", 1))
    , (0x01, ("LXI B, ", 3))
    , (0x02, ("STAX B", 1))
    , (0x03, ("INX B", 1))
    , (0x04, ("INR B", 1))
    , (0x05, ("DCR B", 1))
    , (0x06, ("MVI B, ", 2))
    , (0x07, ("RLC", 1))
    , (0x09, ("DAD B", 1))
    , (0x0a, ("LDAX B", 1))
    , (0x0b, ("DCX B", 1))
    , (0x0c, ("INR C", 1))
    , (0x0d, ("DCR C", 1))
    , (0x0e, ("MVI C, ", 2))
    , (0x0f, ("RRC", 1))
    , (0x11, ("LXI D, ", 3))
    , (0x12, ("STAX D", 1))
    , (0x13, ("INX D", 1))
    , (0x14, ("INR D", 1))
    , (0x15, ("DCR D", 1))
    , (0x16, ("MVI D, ", 2))
    , (0x17, ("RAL", 1))
    , (0x19, ("DAD D", 1))
    , (0x1a, ("LDAX D", 1))
    , (0x1b, ("DCX D", 1))
    , (0x1c, ("INR E", 1))
    , (0x1d, ("DCR E", 1))
    , (0x1e, ("MVI E, ", 2))
    , (0x1f, ("RAR", 1))
    , (0x21, ("LXI H, ", 3))
    , (0x22, ("SHLD ", 3))
    , (0x23, ("INX H", 1))
    , (0x24, ("INR H", 1))
    , (0x25, ("DCR H", 1))
    , (0x26, ("MVI H, ", 2))
    , (0x27, ("DAA", 1))
    , (0x29, ("DAD H", 1))
    , (0x2a, ("LHLD ", 3))
    , (0x2b, ("DCX H", 1))
    , (0x2c, ("INR L", 1))
    , (0x2d, ("DCR L", 1))
    , (0x2e, ("MVI L, ", 2))
    , (0x2f, ("CMA", 1))
    , (0x31, ("LXI SP, ", 3))
    , (0x32, ("STA ", 2))
    , (0x33, ("INX SP", 1))
    , (0x34, ("INR M", 1))
    , (0x35, ("DCR M", 1))
    , (0x36, ("MVI M, ", 2))
    , (0x37, ("STC", 1))
    , (0x39, ("DAD SP", 1))
    , (0x3a, ("LDA ", 3))
    , (0x3b, ("DCX SP", 1))
    , (0x3c, ("INR A", 1))
    , (0x3d, ("DCR A", 1))
    , (0x3e, ("MVI A, ", 2))
    , (0x3f, ("CMC", 1))
    , (0x41, ("MOV B, C", 1))
    , (0x42, ("MOV B, D", 1))
    , (0x43, ("MOV B, E", 1))
    , (0x44, ("MOV B, H", 1))
    , (0x45, ("MOV B, L", 1))
    , (0x46, ("MOV B, M", 1))
    , (0x47, ("MOV B, A", 1))
    , (0x48, ("MOV C, B", 1))
    , (0x49, ("MOV C, C", 1))
    , (0x4a, ("MOV C, D", 1))
    , (0x4b, ("MOV C, E", 1))
    , (0x4c, ("MOV C, H", 1))
    , (0x4d, ("MOV C, L", 1))
    , (0x4e, ("MOV C, M", 1))
    , (0x4f, ("MOV C, A", 1))
    , (0x50, ("MOV D, B", 1))
    , (0x51, ("MOV D, C", 1))
    , (0x52, ("MOV D, D", 1))
    , (0x53, ("MOV D, E", 1))
    , (0x54, ("MOV D, H", 1))
    , (0x55, ("MOV D, L", 1))
    , (0x56, ("MOV D, M", 1))
    , (0x57, ("MOV D, A", 1))
    , (0x58, ("MOV E, B", 1))
    , (0x59, ("MOV E, C", 1))
    , (0x5a, ("MOV E, D", 1))
    , (0x5b, ("MOV E, E", 1))
    , (0x5c, ("MOV E, H", 1))
    , (0x5d, ("MOV E, L", 1))
    , (0x5e, ("MOV E, M", 1))
    , (0x5f, ("MOV E, A", 1))
    , (0x60, ("MOV H, B", 1))
    , (0x61, ("MOV H, C", 1))
    , (0x62, ("MOV H, D", 1))
    , (0x63, ("MOV H, E", 1))
    , (0x64, ("MOV H, H", 1))
    , (0x65, ("MOV H, L", 1))
    , (0x66, ("MOV H, M", 1))
    , (0x67, ("MOV H, A", 1))
    , (0x68, ("MOV L, B", 1))
    , (0x69, ("MOV L, C", 1))
    , (0x6a, ("MOV L, D", 1))
    , (0x6b, ("MOV L, E", 1))
    , (0x6c, ("MOV L, H", 1))
    , (0x6d, ("MOV L, L", 1))
    , (0x6e, ("MOV L, M", 1))
    , (0x6f, ("MOV L, A", 1))
    , (0x70, ("MOV M, B", 1))
    , (0x71, ("MOV M, C", 1))
    , (0x72, ("MOV M, D", 1))
    , (0x73, ("MOV M, E", 1))
    , (0x74, ("MOV M, H", 1))
    , (0x75, ("MOV M, L", 1))
    , (0x76, ("HLT", 1))
    , (0x77, ("MOV M, A", 1))
    , (0x78, ("MOV A, B", 1))
    , (0x79, ("MOV A, C", 1))
    , (0x7a, ("MOV A, D", 1))
    , (0x7b, ("MOV A, E", 1))
    , (0x7c, ("MOV A, H", 1))
    , (0x7d, ("MOV A, L", 1))
    , (0x7e, ("MOV A, M", 1))
    , (0x7f, ("MOV A, A", 1))
    , (0x80, ("ADD B", 1))
    , (0x81, ("ADD C", 1))
    , (0x82, ("ADD D", 1))
    , (0x83, ("ADD E", 1))
    , (0x84, ("ADD H", 1))
    , (0x85, ("ADD L", 1))
    , (0x86, ("ADD M", 1))
    , (0x87, ("ADD A", 1))
    , (0x88, ("ADC B", 1))
    , (0x89, ("ADC C", 1))
    , (0x8a, ("ADC D", 1))
    , (0x8b, ("ADC E", 1))
    , (0x8c, ("ADC H", 1))
    , (0x8d, ("ADC L", 1))
    , (0x8e, ("ADC M", 1))
    , (0x8f, ("ADC A", 1))
    , (0x90, ("SUB B", 1))
    , (0x91, ("SUB C", 1))
    , (0x92, ("SUB D", 1))
    , (0x93, ("SUB E", 1))
    , (0x94, ("SUB H", 1))
    , (0x95, ("SUB L", 1))
    , (0x96, ("SUB M", 1))
    , (0x97, ("SUB A", 1))
    , (0x98, ("SBB B", 1))
    , (0x99, ("SBB C", 1))
    , (0x9a, ("SBB D", 1))
    , (0x9b, ("SBB E", 1))
    , (0x9c, ("SBB H", 1))
    , (0x9d, ("SBB L", 1))
    , (0x9e, ("SBB M", 1))
    , (0x9f, ("SBB A", 1))
    , (0xa0, ("ANA B", 1))
    , (0xa1, ("ANA C", 1))
    , (0xa2, ("ANA D", 1))
    , (0xa3, ("ANA E", 1))
    , (0xa4, ("ANA H", 1))
    , (0xa5, ("ANA L", 1))
    , (0xa6, ("ANA M", 1))
    , (0xa7, ("ANA A", 1))
    , (0xa8, ("XRA B", 1))
    , (0xa9, ("XRA C", 1))
    , (0xaa, ("XRA D", 1))
    , (0xab, ("XRA E", 1))
    , (0xac, ("XRA H", 1))
    , (0xad, ("XRA L", 1))
    , (0xae, ("XRA M", 1))
    , (0xaf, ("XRA A", 1))
    , (0xb0, ("ORA B", 1))
    , (0xb1, ("ORA C", 1))
    , (0xb2, ("ORA D", 1))
    , (0xb3, ("ORA E", 1))
    , (0xb4, ("ORA H", 1))
    , (0xb5, ("ORA L", 1))
    , (0xb6, ("ORA M", 1))
    , (0xb7, ("ORA A", 1))
    , (0xb8, ("CMP B", 1))
    , (0xb9, ("CMP C", 1))
    , (0xba, ("CMP D", 1))
    , (0xbb, ("CMP E", 1))
    , (0xbc, ("CMP H", 1))
    , (0xbd, ("CMP L", 1))
    , (0xbe, ("CMP M", 1))
    , (0xbf, ("CMP A", 1))
    , (0xc0, ("RNZ", 1))
    , (0xc1, ("POP B", 1))
    , (0xc2, ("JNZ ", 3))
    , (0xc3, ("JMP ", 3))
    , (0xc4, ("CNZ ", 3))
    , (0xc5, ("PUSH B", 1))
    , (0xc6, ("ADI ", 2))
    , (0xc7, ("RST 0", 1))
    , (0xc8, ("RZ", 1))
    , (0xc9, ("RET", 1))
    , (0xca, ("JZ ", 3))
    , (0xcc, ("CZ ", 3))
    , (0xcd, ("CALL ", 3))
    , (0xce, ("ACI ", 2))
    , (0xcf, ("RST 1", 1))
    , (0xd0, ("RNC", 1))
    , (0xd1, ("POP D", 1))
    , (0xd2, ("JNC ", 3))
    , (0xd3, ("OUT ", 2))
    , (0xd4, ("CNC ", 3))
    , (0xd5, ("PUSH D", 1))
    , (0xd6, ("SUI ", 2))
    , (0xd7, ("RST 2", 1))
    , (0xd8, ("RC", 1))
    , (0xda, ("JC ", 3))
    , (0xdb, ("IN ", 2))
    , (0xdc, ("CC ", 3))
    , (0xde, ("SBI ", 2))
    , (0xdf, ("RST 3", 1))
    , (0xe0, ("RPO", 1))
    , (0xe1, ("POP H", 1))
    , (0xe2, ("JPO ", 3))
    , (0xe3, ("XTHL", 1))
    , (0xe4, ("CPO ", 3))
    , (0xe5, ("PUSH H", 1))
    , (0xe6, ("ANI ", 2))
    , (0xe7, ("RST 4", 1))
    , (0xe8, ("RPE", 1))
    , (0xe9, ("PCHL", 1))
    , (0xea, ("JPE ", 3))
    , (0xeb, ("XCHG", 1))
    , (0xec, ("CPE ", 3))
    , (0xee, ("XRI ", 2))
    , (0xef, ("RST 5", 1))
    , (0xf0, ("RP", 1))
    , (0xf1, ("POP PSW", 1))
    , (0xf2, ("JP ", 3))
    , (0xf3, ("DI", 1))
    , (0xf4, ("CP ", 3))
    , (0xf5, ("PUSH PSW", 1))
    , (0xf6, ("ORI ", 2))
    , (0xf7, ("RST 6", 1))
    , (0xf8, ("RM", 1))
    , (0xf9, ("SPHL", 1))
    , (0xfa, ("JM ", 3))
    , (0xfb, ("EI", 1))
    , (0xfc, ("CM ", 3))
    , (0xfe, ("CPI ", 2))
    , (0xff, ("RST 7", 1))
    ]

dissasembleOp :: ByteString -> Int -> IO Int
dissasembleOp ops pc = do
  Prelude.putStr (printf "0x%04x" pc ++ "    ")
  let code = BS.index ops pc
  let op = instructionPrintMap Data.Map.!? code
  case op of
    Just (ins, size) ->
      do
        if
          | size == 3 -> putStrLn (ins ++ i2ToString ops pc)
          | size == 2 -> putStrLn (ins ++ i1ToString ops pc)
          | size == 1 -> putStrLn ins
        return size
    Nothing -> do
      putStrLn ""
      return 1

loopThroughOps :: ByteString -> Int -> IO ()
loopThroughOps (uncons -> Nothing) _ = return ()
loopThroughOps ops pc = do
  opbytes <- dissasembleOp ops pc
  (if pc + opbytes < BS.length ops then loopThroughOps ops (pc + opbytes) else return ())

dissasemble :: IO Integer
dissasemble = do
  f <- openFile "../space-invaders.rom" ReadMode
  size <- hFileSize f
  buffer <- hGet f (fromIntegral size)
  print size
  let pc = 0
  loopThroughOps buffer pc
  return 0
