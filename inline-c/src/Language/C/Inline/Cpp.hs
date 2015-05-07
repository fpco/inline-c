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
  , cppPtrCtx
  ) where

import qualified Data.Map as Map
import           Data.Monoid (mempty)
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import           Foreign.Ptr (Ptr, FunPtr, castPtr)
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import           Data.Char (isSpace)

import           Language.C.Inline
import           Language.C.Inline.Internal
import           Language.C.Inline.Context
import           Language.C.Inline.Cpp.Internal
import           Language.C.Inline.Cpp.Types
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
    voidPtrCType
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

voidPtrCType :: C.Type
voidPtrCType = C.quickCParser_ "void *" C.parseType

getPtr :: (KnownSymbol l) => String -> CppPtr l -> (Ptr a -> IO b) -> IO b
getPtr cppPtrTy ptr cont =
  let cppTy' = symbolVal ptr
  in if cppTy == cppTy'
       then withForeignPtr (unCppPtr ptr) $ \ptr' -> cont (castPtr ptr')
       else error $ "cppPtrCtx: expecting CppPtr with label " ++ cppTy ++ ", got " ++ cppTy'
  where
    -- Drop the trailing * and whitespace after that.
    cppTy = reverse $ dropWhile isSpace $ tail $ reverse $ cppPtrTy

cppPtrAntiQuoter :: AntiQuoter (String, String)
cppPtrAntiQuoter = AntiQuoter
  { aqParser = do
      (cId, cppPtrTy) <- parseCppDeclaration
      let s = C.unId cId
      return (s, voidPtrCType, (s, cppPtrTy))
  , aqCMarshaller = \_cTy (_, cppPtrTy) cId ->
      "((" ++ cppPtrTy ++ ") " ++ cId ++ ")"
  , aqHaskellMarshaller = \_cTypes _cTy (cId, cppPtrTy) -> do
      hsTy <- [t| Ptr () |]
      hsExp <- [| getPtr $(TH.litE (TH.stringL cppPtrTy)) $(getHsVariable "cppPtrCtx" cId) |]
      return (hsTy, hsExp)
  }

getHsVariable :: String -> String -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName s
  case mbHsName of
    Nothing -> error $ "Cannot capture Haskell variable " ++ s ++
                       ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

cppPtrCtx :: Context
cppPtrCtx = mempty
  { ctxAntiQuoters = Map.fromList [("cpp-ptr", SomeAntiQuoter cppPtrAntiQuoter)]
  }
