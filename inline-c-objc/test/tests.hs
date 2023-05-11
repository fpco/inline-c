{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline.ObjC as C
import qualified Test.Hspec as Hspec

C.context C.objcCtx

C.include "<Foundation/Foundation.h>"

main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Basic Objective-C" $ do
    Hspec.it "Hello World" $ do
      let x = 3
      [C.block| void {
          NSLog(@"%@ %d", @"Hello, world!", $(int x));
        } |]
    Hspec.it "Expressions" $ do
      z <- [C.exp| int { [[@"A few words" componentsSeparatedByString: @" "] count] } |]
      z `Hspec.shouldBe` 3
    Hspec.it "Objects" $ do
      a <- [C.exp| id { [@"A few more words" componentsSeparatedByString: @" "] } |]
      s <- [C.exp| int { [$(id a) count] } |]
      s `Hspec.shouldBe` 4
