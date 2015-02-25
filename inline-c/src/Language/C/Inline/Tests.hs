{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- This is used for IsString C.Id
module Language.C.Inline.Tests (tests) where

import           Control.Applicative ((<*))
import           Control.Exception (evaluate)
import           Control.Monad (void)
import           Data.String (IsString(..))
import qualified Language.C as C
import qualified Language.C.Quote.C as C
import qualified Language.Haskell.TH as TH
import qualified Test.Hspec as Hspec
import qualified Text.Parsec as Parsec
import           Text.RawString.QQ (r)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)
import           Data.Loc (noLoc)

import           Language.C.Inline.Context
import           Language.C.Inline.Parse

------------------------------------------------------------------------
-- Test

tests :: Hspec.SpecWith ()
tests = do
  Hspec.describe "parsing" $ do
    Hspec.it "parses simple C expression" $ do
      (retType, params, cExp) <- goodParse C.parseExp [r|
          int(double x, float y) { (int) ceil(x + ((double) y)) }
        |]
      retType `Hspec.shouldBe` [C.cty| int |]
      params `Hspec.shouldMatchList` [("x", [C.cty| double |]), ("y", [C.cty| float |])]
      cExp `Hspec.shouldBe` [C.cexp| (int) ceil(x + ((double) y)) |]
    Hspec.it "empty argument list (1)" $ do
      void $ goodParse C.parseExp [r| int{ 4 } |]
    Hspec.it "empty argument list (2)" $ do
      void $ goodParse C.parseExp [r| int(){ 4 } |]
    Hspec.it "does not accept conflicting declarations (1)" $ do
      badParse C.parseExp [r| int(int x, double x) { x } |]
    Hspec.it "does not accept conflicting declarations (2)" $ do
      badParse C.parseExp [r| int(int x) { x_double } |]
    Hspec.it "does not accept conflicting declarations (3)" $ do
      badParse C.parseExp [r| int { x_double + x_int } |]
    Hspec.it "accepts agreeing declarations, if with suffix (1)" $ do
      void $ goodParse C.parseExp [r| int(int x) { x_int } |]
    Hspec.it "accepts agreeing declarations, if with suffix (2)" $ do
      void $ goodParse C.parseExp [r| int { x_int + y_int } |]
    Hspec.it "rejects duplicate agreeing declarations, in params list" $ do
      badParse C.parseExp [r| int(int x, int x) { x } |]
    Hspec.it "accepts suffix types" $ do
      void $ goodParse C.parseExp [r| int { x_int } |]
    Hspec.it "rejects if bad braces (1)" $ do
      badParse C.parseExp [r| int(int x) x |]
    Hspec.it "rejects if bad braces (2)" $ do
      badParse C.parseExp [r| int(int x) { x |]
    Hspec.it "rejects void params list" $ do
      badParse C.parseExp [r| int(void) { 4 } |]
    Hspec.it "rejects unnamed parameters" $ do
      badParse C.parseExp [r| int(int, double) { 4 } |]
    Hspec.it "parses function pointers" $ do
      void $ goodParse C.parseExp [r| int(int (*add)(int, int)) { add(3, 4) } |]
    Hspec.it "parses returning function pointers without parameters" $ do
      (retType, params, cExp) <-
        goodParse C.parseExp [r| double (*)(double) { &cos } |]
      retType `Hspec.shouldBe` [C.cty| double (*)(double) |]
      params `Hspec.shouldMatchList` []
      cExp `Hspec.shouldBe` [C.cexp| &cos |]
    Hspec.it "parses returning function pointers with parameters" $ do
      (retType, params, cExp) <-
        goodParse C.parseExp [r| double (*f(int dummy))(double) { &cos } |]
      retType `Hspec.shouldBe` [C.cty| double (*)(double) |]
      params `Hspec.shouldMatchList` [("dummy", [C.cty| int |])]
      cExp `Hspec.shouldBe` [C.cexp| &cos |]
    Hspec.it "parses named function type" $ do
      (retType, params, cExp) <-
        goodParse C.parseExp [r| double c_cos(double x) { cos(x) } |]
      retType `Hspec.shouldBe` [C.cty| double |]
      params `Hspec.shouldMatchList` [("x", [C.cty| double |])]
      cExp `Hspec.shouldBe` [C.cexp| cos(x) |]
  Hspec.describe "type conversion" $ do
    Hspec.it "converts simple type correctly (1)" $ do
      shouldBeType baseCtx [C.cty| int |] [t| CInt |]
    Hspec.it "converts simple type correctly (2)" $ do
      shouldBeType baseCtx [C.cty| char |] [t| CChar |]
    Hspec.it "converts void" $ do
      shouldBeType baseCtx [C.cty| void |] [t| () |]
    Hspec.it "converts single ptr type" $ do
      shouldBeType baseCtx [C.cty| long* |] [t| Ptr CLong |]
    Hspec.it "converts double ptr type" $ do
      shouldBeType baseCtx [C.cty| unsigned long** |] [t| Ptr (Ptr CULong) |]
    Hspec.it "converts arrays" $ do
      shouldBeType baseCtx [C.cty| double[] |] [t| CArray CDouble |]
    Hspec.it "converts named things" $ do
      shouldBeType baseCtx [C.cty| unsigned int foo[] |] [t| CArray CUInt |]
    Hspec.it "converts arrays of pointers" $ do
      shouldBeType baseCtx
        [C.cty| unsigned short *foo[] |] [t| CArray (Ptr CUShort) |]
    Hspec.it "ignores qualifiers" $ do
      shouldBeType baseCtx [C.cty| const short* |] [t| Ptr CShort |]
    Hspec.it "ignores storage information" $ do
      shouldBeType baseCtx [C.cty| extern unsigned long |] [t| CULong |]
    Hspec.it "converts sized arrays" $ do
      shouldBeType baseCtx [C.cty| float[4] |] [t| CArray CFloat |]
    Hspec.it "converts variably sized arrays" $ do
      shouldBeType baseCtx [C.cty| float[*] |] [t| CArray CFloat |]
    Hspec.it "converts function pointers" $ do
      shouldBeType baseCtx
        [C.cty| int (*f)(unsigned char, float) |]
        [t| FunPtr (CUChar -> CFloat -> IO CInt) |]
    Hspec.it "converts complicated function pointers (1)" $ do
      -- pointer to function returning pointer to function returning int
      shouldBeType baseCtx
        [C.cty| int (*(*)())() |] [t| FunPtr (IO (FunPtr (IO CInt))) |]
    Hspec.it "converts complicated function pointerst (2)" $ do
      -- foo is an array of pointer to pointer to function returning
      -- pointer to array of pointer to char
      shouldBeType baseCtx
        [C.cty| char *(*(**foo [])())[] |]
        [t| CArray (Ptr (FunPtr (IO (Ptr (CArray (Ptr CChar)))))) |]
    Hspec.it "converts complicated function pointers (3)" $ do
      -- foo is an array of pointer to pointer to function taking int
      -- returning pointer to array of pointer to char
      shouldBeType baseCtx
        [C.cty| char *(*(**foo [])(int x))[] |]
        [t| CArray (Ptr (FunPtr (CInt -> IO (Ptr (CArray (Ptr CChar)))))) |]
  where
    -- We use show + length to fully evaluate the result -- there
    -- might be exceptions hiding.  TODO get rid of exceptions.
    strictParse p s =
      case Parsec.runParser (parseTypedC baseCtx p <* Parsec.eof) () "test" s of
        Left err -> error $ "Parse error (strictParse): " ++ show err
        Right x -> do
          void $ evaluate $ length $ show x
          return x

    goodParse = strictParse
    badParse p s = strictParse p s `Hspec.shouldThrow` Hspec.anyException

    goodConvert ctx cTy = do
      mbHsTy <- TH.runQ $ convertCType ctx False cTy
      case mbHsTy of
        Nothing   -> error $ "Could not convert type (goodConvert)"
        Just hsTy -> return hsTy

    shouldBeType ctx cTy hsTy = do
      x <- goodConvert ctx cTy
      y <- TH.runQ hsTy
      x `Hspec.shouldBe` y

instance IsString C.Id where
  fromString s = C.Id s noLoc
