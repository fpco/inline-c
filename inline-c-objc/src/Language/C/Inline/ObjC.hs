{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.C.Inline.ObjC
  ( module Language.C.Inline
  , objcCtx
  , CId, Id(..)
  ) where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.C.Types as CT
import Foreign

import           Language.C.Inline
import           Language.C.Inline.Context

import qualified Data.Map as Map

objcCtx :: Context
objcCtx = baseCtx <> mempty
  { ctxForeignSrcLang = Just TH.LangObjc
  , ctxTypesTable = Map.singleton (CT.TypeName "id") [t|Id|]
  }

data CId
newtype Id = Id (Ptr CId)
