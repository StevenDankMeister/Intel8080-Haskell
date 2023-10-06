{-# LANGUAGE OverloadedRecordDot #-}

module Utils where

import Data.Binary (Word16, Word8)
import Data.Bits (Bits (shiftL, shiftR, (.|.)))
import Data.ByteString as BS (
  ByteString,
  append,
  index,
  init,
  snoc,
  splitAt,
 )
import States (CCState (ac, cy, p, si, z))

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
  res = sign .|. zero .|. aux .|. parity .|. ccstate.cy .|. 0x2

word16ToWord8s :: Word16 -> (Word8, Word8)
word16ToWord8s w = (fromIntegral (w `shiftR` 8), fromIntegral w)
concatBytesBE :: Word8 -> Word8 -> Word16
concatBytesBE hi lo = res
 where
  low = fromIntegral lo
  high = fromIntegral hi
  res = high `shiftL` 8 .|. low
