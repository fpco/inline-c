{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception.Safe
import           Control.Monad
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Test.Hspec as Hspec

C.context C.cppCtx

C.include "<iostream>"
C.include "<stdexcept>"

main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Basic C++" $ do
    Hspec.it "Hello World" $ do
      let x = 3
      [C.block| void {
          std::cout << "Hello, world!" << $(int x) << std::endl;
        } |]

  Hspec.describe "Exception handling" $ do
    Hspec.it "std::exceptions are caught" $ do
      result <- try [C.catchBlock|
        throw std::runtime_error("C++ error message");
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "C++ error message")

    Hspec.it "non-exceptions are caught" $ do
      result <- try [C.catchBlock|
        throw 0xDEADBEEF;
        |]

      result `Hspec.shouldBe` Left C.CppOtherException

    Hspec.it "catch without return" $ do
      result <- [C.tryBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "C++ error message")

    Hspec.it "try and return without throwing" $ do
      result <- [C.tryBlock| int {
          return 123;
        }
        |]

      result `Hspec.shouldBe` Right 123

    Hspec.it "return maybe throwing" $ do
      result <- [C.tryBlock| int {
          if(1) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Right 123

    Hspec.it "return definitely throwing" $ do
      result <- [C.tryBlock| int {
          if(0) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "C++ error message")

    Hspec.it "code without exceptions works normally" $ do
      result :: Either C.CppException C.CInt <- try $ C.withPtr_ $ \resPtr -> [C.catchBlock|
          *$(int* resPtr) = 0xDEADBEEF;
        |]

      result `Hspec.shouldBe` Right 0xDEADBEEF
