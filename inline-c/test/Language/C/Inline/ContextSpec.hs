{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.C.Inline.ContextSpec (spec) where

import           Control.Applicative ((<*), (*>))
import           Control.Monad.Trans.Class (lift)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import           Foreign.Ptr (Ptr, FunPtr)
import qualified Language.Haskell.TH as TH
import qualified Test.Hspec as Hspec
import           Text.Parser.Char
import           Text.Parser.Combinators

import qualified Language.C.Types as C
import           Language.C.Inline.Context

spec :: Hspec.SpecWith ()
spec = do
  Hspec.it "converts simple type correctly (1)" $ do
    shouldBeType (cty "int") [t| Int32 |]
  Hspec.it "converts simple type correctly (2)" $ do
    shouldBeType (cty "char") [t| Int8 |]
  Hspec.it "converts void" $ do
    shouldBeType (cty "void") [t| () |]
  Hspec.it "converts single ptr type" $ do
    shouldBeType (cty "long*") [t| Ptr Int64 |]
  Hspec.it "converts double ptr type" $ do
    shouldBeType (cty "unsigned long**") [t| Ptr (Ptr Word64) |]
  Hspec.it "converts arrays" $ do
    shouldBeType (cty "double[]") [t| CArray Double |]
  Hspec.it "converts named things" $ do
    shouldBeType (cty "unsigned int foo[]") [t| CArray Word32 |]
  Hspec.it "converts arrays of pointers" $ do
    shouldBeType
      (cty "unsigned short *foo[]") [t| CArray (Ptr Word16) |]
  Hspec.it "ignores qualifiers" $ do
    shouldBeType (cty "const short*") [t| Ptr Int16 |]
  Hspec.it "ignores storage information" $ do
    shouldBeType (cty "extern unsigned long") [t| Word64 |]
  Hspec.it "converts sized arrays" $ do
    shouldBeType (cty "float[4]") [t| CArray Float |]
  Hspec.it "converts variably sized arrays" $ do
    shouldBeType (cty "float[*]") [t| CArray Float |]
  Hspec.it "converts function pointers" $ do
    shouldBeType
      (cty "int (*f)(unsigned char, float)")
      [t| FunPtr (Word8 -> Float -> IO Int32) |]
  Hspec.it "converts complicated function pointers (1)" $ do
    -- pointer to function returning pointer to function returning int
    shouldBeType
      (cty "int (*(*)())()") [t| FunPtr (IO (FunPtr (IO Int32))) |]
  Hspec.it "converts complicated function pointerst (2)" $ do
    -- foo is an array of pointer to pointer to function returning
    -- pointer to array of pointer to char
    shouldBeType
      (cty "char *(*(**foo [])())[]")
      [t| CArray (Ptr (FunPtr (IO (Ptr (CArray (Ptr Int8)))))) |]
  Hspec.it "converts complicated function pointers (3)" $ do
    -- foo is an array of pointer to pointer to function taking int
    -- returning pointer to array of pointer to char
    shouldBeType
      (cty "char *(*(**foo [])(int x))[]")
      [t| CArray (Ptr (FunPtr (Int32 -> IO (Ptr (CArray (Ptr Int8)))))) |]
  where
    goodConvert cTy = do
      mbHsTy <- TH.runQ $ convertCType (ctxCTypesTable baseCtx) IO cTy
      case mbHsTy of
        Nothing   -> error $ "Could not convert type (goodConvert)"
        Just hsTy -> return hsTy

    shouldBeType cTy hsTy = do
      x <- goodConvert cTy
      y <- TH.runQ hsTy
      x `Hspec.shouldBe` y

    assertParse p s =
      case C.runCParser (const False) "spec" s (lift spaces *> p <* lift eof) of
        Left err -> error $ "Parse error (assertParse): " ++ show err
        Right x -> x

    cty s = C.parameterDeclarationType $ assertParse C.parseParameterDeclaration s
