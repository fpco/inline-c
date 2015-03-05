{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.C.Inline.Spec (spec) where

import           Control.Applicative ((<*), (*>))
import           Control.Monad.Trans.Class (lift)
import qualified Test.Hspec as Hspec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.RawString.QQ (r)
import           Control.Monad (void)
import           Control.Exception (evaluate)

import qualified Language.C.Types as C
import           Language.C.Inline.Parse

spec :: Hspec.SpecWith ()
spec = do
  Hspec.describe "parsing" $ do
    Hspec.it "parses simple C expression" $ do
      (retType, params, cExp) <- goodParse [r|
          int(double x, float y) { (int) ceil(x + ((double) y)) }
        |]
      retType `Hspec.shouldBe` (cty "int")
      params `Hspec.shouldMatchList` [("x", (cty "double")), ("y", (cty "float"))]
      cExp `Hspec.shouldBe` " (int) ceil(x + ((double) y)) "
    Hspec.it "empty argument list (1)" $ do
      void $ goodParse [r| int{ 4 } |]
    Hspec.it "empty argument list (2)" $ do
      void $ goodParse [r| int(){ 4 } |]
    Hspec.it "does not accept conflicting declarations (1)" $ do
      badParse [r| int(int x, double x) { x } |]
    Hspec.it "does not accept conflicting declarations (2)" $ do
      badParse [r| int(int x) { $(double x) } |]
    Hspec.it "does not accept conflicting declarations (3)" $ do
      badParse [r| int { $(double x) + $(int x) } |]
    Hspec.it "accepts agreeing declarations, if with suffix (1)" $ do
      void $ goodParse [r| int(int x) { $(int x) } |]
    Hspec.it "accepts agreeing declarations, if with suffix (2)" $ do
      void $ goodParse [r| int { $(int x) + $(int y) } |]
    Hspec.it "rejects duplicate agreeing declarations, in params list" $ do
      badParse [r| int(int x, int x) { x } |]
    Hspec.it "accepts suffix types" $ do
      void $ goodParse [r| int { $(int x) } |]
    Hspec.it "rejects if bad braces (1)" $ do
      badParse [r| int(int x) x |]
    Hspec.it "rejects if bad braces (2)" $ do
      badParse [r| int(int x) { x |]
    Hspec.it "rejects void params list" $ do
      badParse [r| int(void) { 4 } |]
    Hspec.it "rejects unnamed parameters" $ do
      badParse [r| int(int, double) { 4 } |]
    Hspec.it "parses function pointers" $ do
      void $ goodParse [r| int(int (*add)(int, int)) { add(3, 4) } |]
    Hspec.it "parses returning function pointers without parameters" $ do
      (retType, params, cExp) <-
        goodParse [r| double (*)(double) { &cos } |]
      retType `Hspec.shouldBe` (cty "double (*)(double)")
      params `Hspec.shouldMatchList` []
      cExp `Hspec.shouldBe` " &cos "
    Hspec.it "parses returning function pointers with parameters" $ do
      (retType, params, cExp) <-
        goodParse [r| double (*f(int dummy))(double) { &cos } |]
      retType `Hspec.shouldBe` (cty "double (*)(double)")
      params `Hspec.shouldMatchList` [("dummy", (cty "int"))]
      cExp `Hspec.shouldBe` " &cos "
    Hspec.it "parses named function type" $ do
      (retType, params, cExp) <-
        goodParse [r| double c_cos(double x) { cos(x) } |]
      retType `Hspec.shouldBe` (cty "double")
      params `Hspec.shouldMatchList` [("x", (cty "double"))]
      cExp `Hspec.shouldBe` " cos(x) "
  where
    assertParse p s =
      case C.runCParser (const False) "spec" s (lift spaces *> p <* lift eof) of
        Left err -> error $ "Parse error (assertParse): " ++ show err
        Right x -> x

    -- We use show + length to fully evaluate the result -- there
    -- might be exceptions hiding.  TODO get rid of exceptions.
    strictParse :: String -> IO (C.Type, [(C.Id, C.Type)], String)
    strictParse s = do
      let x = assertParse parseTypedC s
      void $ evaluate $ length $ show x
      return x

    goodParse = strictParse
    badParse s = strictParse s `Hspec.shouldThrow` Hspec.anyException

    cty :: String -> C.Type
    cty s = C.parameterDeclarationType $ assertParse C.parseParameterDeclaration s
