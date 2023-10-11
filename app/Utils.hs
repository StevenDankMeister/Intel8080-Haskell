{-# LANGUAGE OverloadedRecordDot #-}

module Utils where

import Control.Monad.State
import Data.Binary (Word16, Word8)
import Data.Bits (Bits (complementBit, popCount, shiftL, shiftR, (.|.)), (.&.))
import Data.ByteString as BS (
  ByteString,
  append,
  index,
  init,
  snoc,
  splitAt,
 )
import States

maskLower4Bytes :: (Bits a, Num a) => a -> a
maskLower4Bytes x = res
 where
  res = x .&. 0xf

setPC :: Word16 -> State8080M State8080
setPC adr = do
  s <- get
  put s{pc = adr}
  return s

addPC :: Word16 -> State8080M State8080
addPC x = do
  s <- get
  put s{pc = s.pc + x}
  return s

getIM :: State8080 -> Word8
getIM s = im
 where
  im = getNNextByte s.program s.pc 1

getMem :: State8080 -> Word8
getMem s = byte
 where
  adr = concatBytesBE s.h s.l
  byte = getByteAtAdr s.program adr

toMem :: Word8 -> State8080M State8080
toMem byte = do
  s <- get
  let adr = concatBytesBE s.h s.l
  let program = insertIntoByteString byte s.program (fromIntegral adr)
  put s{program = program}
  return s

getParity :: (Bits a) => a -> Word8
getParity bits = fromIntegral (complementBit (popCount bits `mod` 2) 0)

getSign :: Word8 -> Word8
getSign byte = (byte .&. 0x80) `shiftR` 7

getZero :: Word8 -> Word8
getZero bits = if bits == 0 then 1 else 0

nextTwoBytesToWord16BE :: ByteString -> Word16 -> Word16
nextTwoBytesToWord16BE mem pc = res
 where
  low = fromIntegral (getNNextByte mem pc 1)
  high = fromIntegral (getNNextByte mem pc 2)
  res = high `shiftL` 8 .|. low

getNNextByte :: ByteString -> Word16 -> Int -> Word8
getNNextByte mem pc n = mem `getByteAtAdr` (pc + fromIntegral n)

getNNextByte' :: Int -> State8080M Word8
getNNextByte' n = do
  s <- get
  let byte = getNNextByte s.program s.pc n
  return byte

getNextByte :: State8080 -> Word8
getNextByte s = getNNextByte s.program s.pc 1

getByteAtAdr :: ByteString -> Word16 -> Word8
getByteAtAdr mem pc = mem `BS.index` fromIntegral pc

insertIntoByteString :: Word8 -> ByteString -> Int -> ByteString
insertIntoByteString byte bs n = (BS.init left `snoc` byte) `append` right
 where
  (left, right) = BS.splitAt (n + 1) bs

flagsToByte :: CCState -> Word8
flagsToByte ccstate = res
 where
  sign = ccstate.si `shiftL` 7
  zero = ccstate.z `shiftL` 6
  aux = ccstate.ac `shiftL` 4
  parity = ccstate.p `shiftL` 2
  res = sign .|. zero .|. aux .|. parity .|. ccstate.cy .|. 0x2 -- Second bit always 1

byteToFlags :: Word8 -> CCState
byteToFlags byte = res
 where
  sign = fromIntegral $ popCount $ byte .&. 0x80
  zero = fromIntegral $ popCount $ byte .&. 0x40
  aux = fromIntegral $ popCount $ byte .&. 0x10
  parity = fromIntegral $ popCount $ byte .&. 0x4
  carry = byte .&. 0x1
  res = CCState{si = sign, z = zero, ac = aux, p = parity, cy = carry}

word16ToWord8s :: Word16 -> (Word8, Word8)
word16ToWord8s w = (fromIntegral (w `shiftR` 8), fromIntegral w)

concatBytesBE :: Word8 -> Word8 -> Word16
concatBytesBE hi lo = res
 where
  low = fromIntegral lo
  high = fromIntegral hi
  res = high `shiftL` 8 .|. low
