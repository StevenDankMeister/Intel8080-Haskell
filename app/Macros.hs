{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Macros where

import Control.Monad.State
import Language.Haskell.TH
import Utils

getRecFieldValue_T :: String -> Name -> Q (Name, Exp)
getRecFieldValue_T field rec = do
  let field' = mkName field
  let get_field_value_exp = AppE (VarE field') (VarE rec)
  return (field', get_field_value_exp)

getState_T :: Q (Name, Stmt)
getState_T = do
  s <- newName "s"
  let get_stmt = BindS (VarP s) (VarE 'get)
  return (s, get_stmt)

put_T :: Name -> [FieldExp] -> Stmt
put_T rec_name rec_updates = NoBindS $ AppE (VarE 'put) (recUpd_T rec_name rec_updates)

recUpd_T :: Name -> [FieldExp] -> Exp
recUpd_T rec = RecUpdE (VarE rec)

addPC_T :: Integer -> Stmt
addPC_T n = NoBindS $ AppE (VarE 'addPC) (LitE (IntegerL n))
