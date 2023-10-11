module States where

import Control.Monad.State (StateT)
import Data.Binary (Word16, Word8)
import Data.ByteString as BS (ByteString, head)
import Numeric (showHex)

showHexList :: [Word8] -> [String]
showHexList = Prelude.map (`showHex` "")

data CCState = CCState
  { cy :: Word8 -- Carry
  , ac :: Word8 -- Aux carry
  , si :: Word8 -- Sign
  , z :: Word8 -- Zero
  , p :: Word8 -- Parity
  }

data State8080 = State8080
  { a :: Word8
  , b :: Word8
  , c :: Word8
  , d :: Word8
  , e :: Word8
  , h :: Word8
  , l :: Word8
  , sp :: Word16
  , pc :: Word16
  , stack :: [Word8] -- This is just for printing
  , program :: ByteString
  , ccodes :: CCState
  , inte :: Word8
  }

instance Show State8080 where
  show :: State8080 -> String
  show state =
    "a="
      ++ showHex (a state) ""
      ++ ","
      ++ "b="
      ++ showHex (b state) ""
      ++ ","
      ++ "c="
      ++ showHex (c state) ""
      ++ ","
      ++ "d="
      ++ showHex (d state) ""
      ++ ","
      ++ "e="
      ++ showHex (e state) ""
      ++ ","
      ++ "h="
      ++ showHex (h state) ""
      ++ ","
      ++ "l="
      ++ showHex (l state) ""
      ++ ","
      ++ "sp="
      ++ showHex (sp state) ""
      ++ ","
      ++ "pc="
      ++ showHex (pc state) ""
      ++ ","
      ++ "stack="
      ++ show (showHexList (stack state))
      -- ++ ","
      -- ++ "program = "
      -- ++ show (BS.head (program state))
      ++ ","
      ++ "; FLAGS: "
      ++ show (ccodes state)
      ++ ","
      ++ "inte="
      ++ show (inte state)

instance Show CCState where
  show :: CCState -> String
  show ccstate =
    "CY="
      ++ showHex (cy ccstate) ""
      ++ ","
      ++ "ac="
      ++ showHex (ac ccstate) ""
      ++ ","
      ++ "si="
      ++ showHex (si ccstate) ""
      ++ ","
      ++ "z="
      ++ showHex (z ccstate) ""
      ++ ","
      ++ "p="
      ++ showHex (p ccstate) ""

type State8080M = StateT State8080 IO
