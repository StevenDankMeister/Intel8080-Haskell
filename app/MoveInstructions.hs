{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module MoveInstructions where

import Control.Monad.State
import Data.Binary (Word16, Word8)
import Language.Haskell.TH
import Macros (addPC_T, getRecFieldValue_T, getState_T, put_T, recUpd_T)
import States
import Utils

-- :: State8080M State8080
-- Register x ->
-- \() -> do
--   s <- get
--   put s {x = getNextByte s}
--   addPC 2
mvIX :: String -> Q Exp
mvIX x = do
  s <- newName "s"
  let x' = mkName x
  let immediate = AppE (VarE 'getNextByte) (VarE s)
  return
    $ LamE []
    $ DoE
      Nothing
      [ BindS (VarP s) (VarE 'get)
      , NoBindS (AppE (VarE 'put) (RecUpdE (VarE s) [(x', immediate)]))
      , NoBindS (AppE (VarE 'addPC) (LitE (IntegerL 2)))
      ]

-- :: State8080M State8080
-- Register x -> Register y ->
-- \() -> do
--   s <- get
--   put s {x = s.y}
--   addPC 1
movXY :: String -> String -> Q Exp
movXY x y = do
  (s, get_stmt) <- getState_T
  (y', get_y_expr) <- getRecFieldValue_T y s
  let x' = mkName x
  let put_stmt = put_T s [(x', get_y_expr)]

  return
    $ LamE []
    $ DoE
      Nothing
      [get_stmt, put_stmt, addPC_T 1]

movIM :: State8080M State8080
movIM = do
  s <- get
  toMem $ getIM s
  addPC 2

movIC :: State8080M State8080
movIC = do
  s <- get
  put s{c = getIM s}
  addPC 2

movIH :: State8080M State8080
movIH = do
  s <- get
  let im = getNextByte s
  put s{h = im}
  addPC 2

movIA :: State8080M State8080
movIA = do
  s <- get
  let im = getNextByte s
  put s{a = im}
  addPC 2

movMA :: State8080M State8080
movMA = do
  s <- get
  toMem s.a
  addPC 1

movMB :: State8080M State8080
movMB = do
  s <- get
  toMem s.b
  addPC 1

movMC :: State8080M State8080
movMC = do
  s <- get
  toMem s.c
  addPC 1

movMD :: State8080M State8080
movMD = do
  s <- get
  toMem s.d
  addPC 1

movME :: State8080M State8080
movME = do
  s <- get
  toMem s.e
  addPC 1

movMH :: State8080M State8080
movMH = do
  s <- get
  toMem s.h
  addPC 1

movML :: State8080M State8080
movML = do
  s <- get
  toMem s.l
  addPC 1

mviD :: State8080M State8080
mviD = do
  s <- get
  put s{d = getNextByte s}
  addPC 2

mviE :: State8080M State8080
mviE = do
  s <- get
  put s{e = getNextByte s}
  addPC 2

mviL :: State8080M State8080
mviL = do
  s <- get
  put s{l = getNextByte s}
  addPC 2

lhld :: Word16 -> State8080M State8080
lhld adr = do
  s <- get
  let l = getByteAtAdr s.program adr
  let h = getByteAtAdr s.program (adr + 1)

  put s{l = l, h = h, pc = s.pc + 3}
  return s

shld :: Word16 -> State8080M State8080
shld adr = do
  s <- get
  putAt s.l adr
  putAt s.h (adr + 1)
  addPC 3
