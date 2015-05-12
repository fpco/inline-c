{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.C.Inline.ParseSpec (spec) where

import           Control.Applicative ((<*), (*>))
import           Control.Monad.Trans.Class (lift)
import qualified Test.Hspec as Hspec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.RawString.QQ (r)
import           Control.Monad (void)
import           Control.Exception (evaluate)
import           Data.Monoid ((<>))
import           Text.Regex.Posix ((=~))

import qualified Language.C.Types as C
import           Language.C.Inline.Internal
import           Language.C.Inline.Context

spec :: Hspec.SpecWith ()
spec = do
  Hspec.describe "parsing" $ do
    Hspec.it "parses simple C expression" $ do
      (retType, params, cExp) <- goodParse [r|
          int { (int) ceil($(double x) + ((double) $(float y))) }
        |]
      retType `Hspec.shouldBe` (cty "int")
      params `shouldMatchParameters` [(cty "double", Plain "x"), (cty "float", Plain "y")]
      cExp `shouldMatchBody` " (int) ceil(x[a-z0-9_]+ \\+ ((double) y[a-z0-9_]+)) "
    Hspec.it "accepts anti quotes" $ do
      void $ goodParse [r| int { $(int x) } |]
    Hspec.it "rejects if bad braces (1)" $ do
      badParse [r| int x |]
    Hspec.it "rejects if bad braces (2)" $ do
      badParse [r| int { x |]
    Hspec.it "parses function pointers" $ do
      void $ goodParse [r| int(int (*add)(int, int)) { add(3, 4) } |]
    Hspec.it "parses returning function pointers" $ do
      (retType, params, cExp) <-
        goodParse [r| double (*)(double) { &cos } |]
      retType `Hspec.shouldBe` (cty "double (*)(double)")
      params `shouldMatchParameters` []
      cExp `shouldMatchBody` " &cos "
  where
    ctx = baseCtx <> funCtx

    assertParse p s =
      case C.runCParser (const False) "spec" s (lift spaces *> p <* lift eof) of
        Left err -> error $ "Parse error (assertParse): " ++ show err
        Right x -> x

    -- We use show + length to fully evaluate the result -- there
    -- might be exceptions hiding.  TODO get rid of exceptions.
    strictParse :: String -> IO (C.Type, [(C.Id, C.Type, ParameterType)], String)
    strictParse s = do
      let ParseTypedC retType pars body =
            assertParse (parseTypedC (ctxAntiQuoters ctx)) s
      void $ evaluate $ length $ show (retType, pars, body)
      return (retType, pars, body)

    goodParse = strictParse
    badParse s = strictParse s `Hspec.shouldThrow` Hspec.anyException

    cty :: String -> C.Type
    cty s = C.parameterDeclarationType $ assertParse C.parseParameterDeclaration s

    shouldMatchParameters
      :: [(C.Id, C.Type, ParameterType)] -> [(C.Type, ParameterType)] -> Hspec.Expectation
    shouldMatchParameters pars pars' =
      [(x, y) | (_, x, y) <- pars] `Hspec.shouldMatchList` pars'

    shouldMatchBody :: String -> String -> Hspec.Expectation
    shouldMatchBody x y = do
      let f ch' = case ch' of
            '(' -> "\\("
            ')' -> "\\)"
            ch -> [ch]
      (x =~ concatMap f y) `Hspec.shouldBe` True
