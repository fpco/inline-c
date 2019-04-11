{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.C.Inline.ContextSpec (spec) where

import           Control.Monad.Trans.Class (lift)
import           Data.Word
import qualified Test.Hspec as Hspec
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Language.Haskell.TH as TH
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<*), (*>))
#endif

import qualified Language.C.Types as C
import           Language.C.Inline.Context

spec :: Hspec.SpecWith ()
spec = do
  Hspec.it "converts simple type correctly (1)" $
    shouldBeType (cty "int") [t| CInt |]
  Hspec.it "converts simple type correctly (2)" $
    shouldBeType (cty "char") [t| CChar |]
  Hspec.it "converts void" $
    shouldBeType (cty "void") [t| () |]
  Hspec.it "converts standard library types (1)" $
    shouldBeType (cty "FILE") [t| CFile |]
  Hspec.it "converts standard library types (2)" $
    shouldBeType (cty "uint16_t") [t| Word16 |]
  Hspec.it "converts standard library types (3)" $
    shouldBeType (cty "jmp_buf") [t| CJmpBuf |]
  Hspec.it "converts single ptr type" $
    shouldBeType (cty "long*") [t| Ptr CLong |]
  Hspec.it "converts double ptr type" $
    shouldBeType (cty "unsigned long**") [t| Ptr (Ptr CULong) |]
  Hspec.it "converts arrays" $
    shouldBeType (cty "double[]") [t| CArray CDouble |]
  Hspec.it "converts named things" $
    shouldBeType (cty "unsigned int foo[]") [t| CArray CUInt |]
  Hspec.it "converts arrays of pointers" $
    shouldBeType
      (cty "unsigned short *foo[]") [t| CArray (Ptr CUShort) |]
  Hspec.it "ignores qualifiers" $
    shouldBeType (cty "const short*") [t| Ptr CShort |]
  Hspec.it "ignores storage information" $
    shouldBeType (cty "extern unsigned long") [t| CULong |]
  Hspec.it "converts sized arrays" $
    shouldBeType (cty "float[4]") [t| CArray CFloat |]
  Hspec.it "converts variably sized arrays" $
    shouldBeType (cty "float[*]") [t| CArray CFloat |]
  Hspec.it "converts function pointers" $
    shouldBeType
      (cty "int (*f)(unsigned char, float)")
      [t| FunPtr (CUChar -> CFloat -> IO CInt) |]
  Hspec.it "converts complicated function pointers (1)" $
    -- pointer to function returning pointer to function returning int
    shouldBeType
      (cty "int (*(*)())()") [t| FunPtr (IO (FunPtr (IO CInt))) |]
  Hspec.it "converts complicated function pointerst (2)" $
    -- foo is an array of pointer to pointer to function returning
    -- pointer to array of pointer to char
    shouldBeType
      (cty "char *(*(**foo [])())[]")
      [t| CArray (Ptr (FunPtr (IO (Ptr (CArray (Ptr CChar)))))) |]
  Hspec.it "converts complicated function pointers (3)" $
    -- foo is an array of pointer to pointer to function taking int
    -- returning pointer to array of pointer to char
    shouldBeType
      (cty "char *(*(**foo [])(int x))[]")
      [t| CArray (Ptr (FunPtr (CInt -> IO (Ptr (CArray (Ptr CChar)))))) |]
  where
    goodConvert cTy = do
      mbHsTy <- TH.runQ $ convertType IO baseTypes cTy
      case mbHsTy of
        Nothing   -> error "Could not convert type (goodConvert)"
        Just hsTy -> return hsTy

    shouldBeType cTy hsTy = do
      x <- goodConvert cTy
      y <- TH.runQ hsTy
      x `Hspec.shouldBe` y

    assertParse p s =
      case C.runCParser (C.cCParserContext (typeNamesFromTypesTable baseTypes)) "spec" s (lift spaces *> p <* lift eof) of
        Left err -> error $ "Parse error (assertParse): " ++ show err
        Right x -> x

    cty s = C.parameterDeclarationType $ assertParse C.parseParameterDeclaration s

    baseTypes = ctxTypesTable baseCtx
