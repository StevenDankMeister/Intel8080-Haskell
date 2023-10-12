{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module MoveInstructions where

import Control.Monad.State
import Data.Binary (Word16, Word8)
import Language.Haskell.TH
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
  s <- newName "s"
  let x' = mkName x
  let y' = mkName y
  let y_access = AppE (VarE y') (VarE s)
  let get_state = BindS (VarP s) (VarE 'get)
  let put_state = NoBindS (AppE (VarE 'put) (RecUpdE (VarE s) [(x', y_access)]))
  let update_pc = NoBindS (AppE (VarE 'addPC) (LitE (IntegerL 1)))
  return
    $ LamE []
    $ DoE
      Nothing
      [get_state, put_state, update_pc]

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
