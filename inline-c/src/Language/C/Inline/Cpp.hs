{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module exposing a 'Context' to inline C++ code.  We only have used
-- this for experiments, so use with caution.  See the C++ tests to see
-- how to build inline C++ code.
module Language.C.Inline.Cpp
  ( module Language.C.Inline
  , cppCtx
  , using
  , CppPtr(..)
  , new
  ) where

import           Foreign.Ptr (Ptr, FunPtr)
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import           Data.Proxy (Proxy(..))
import           Data.Functor ((<$>))

import           Language.C.Inline
import           Language.C.Inline.Internal
import           Language.C.Inline.Context
import           Language.C.Inline.Cpp.Internal
import qualified Language.C.Types as C

context cppCtx

using :: String -> TH.DecsQ
using s = literal $ "using " ++ s ++ ";"

newtype CppPtr (ty :: Symbol) = CppPtr {unCppPtr :: ForeignPtr ()}

new :: String -> TH.ExpQ
new ty = do
  newExp <- inlineExp
    TH.Safe
    [t| IO (Ptr ()) |]
    (C.quickCParser_ "void *" C.parseType)
    []
    ("(void *) new " ++ ty)
  deleteFunName <- uniqueCName $ "delete_" ++ ty
  emitLiteral $ unlines
    [ "extern \"C\" {"
    , "    void " ++ deleteFunName ++ "(void *ptr) {"
    , "        delete ((" ++ ty ++ " *) ptr);"
    , "    }"
    , "}"
    ]
  deleteFunImport <- uniqueFfiImportName
  deleteFunDec <- TH.forImpD
    TH.CCall
    TH.Safe
    ("&" ++ deleteFunName)
    deleteFunImport
    [t| FunPtr (Ptr () -> IO ()) |]
  TH.addTopDecls [deleteFunDec]
  let ptrty = TH.appT (TH.conT ''CppPtr) (TH.litT (TH.strTyLit ty))
  [| do ptr <- $(return newExp)
        fptr <- newForeignPtr $(TH.varE deleteFunImport) ptr
        return (CppPtr fptr :: $(ptrty)) |]
