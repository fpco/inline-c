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

      result `Hspec.shouldBe` Left (C.CppStdException "Exception: C++ error message; type: std::runtime_error")

    Hspec.it "non-exceptions are caught (unsigned int)" $ do
      result <- try [C.catchBlock|
        throw 0xDEADBEEF;
        |]

      result `Hspec.shouldBe` Left (C.CppOtherException (Just "unsigned int"))

    Hspec.it "non-exceptions are caught (std::string)" $ do
      result <- try [C.catchBlock|
        throw std::string("FOOBAR");
        |]

      result `Hspec.shouldBe` Left (C.CppOtherException (Just "std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >"))

    Hspec.it "catch without return (pure)" $ do
      result <- [C.tryBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "Exception: C++ error message; type: std::runtime_error")

    Hspec.it "try and return without throwing (pure)" $ do
      result <- [C.tryBlock| int {
          return 123;
        }
        |]

      result `Hspec.shouldBe` Right 123

    Hspec.it "return maybe throwing (pure)" $ do
      result <- [C.tryBlock| int {
          if(1) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Right 123

    Hspec.it "return definitely throwing (pure)" $ do
      result <- [C.tryBlock| int {
          if(0) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "Exception: C++ error message; type: std::runtime_error")

    Hspec.it "catch without return (pure)" $ do
      result <- [C.tryBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "Exception: C++ error message; type: std::runtime_error")

    Hspec.it "try and return without throwing (throw)" $ do
      result :: Either C.CppException C.CInt <- try [C.throwBlock| int {
          return 123;
        }
        |]

      result `Hspec.shouldBe` Right 123

    Hspec.it "return maybe throwing (throw)" $ do
      result :: Either C.CppException C.CInt <- try [C.throwBlock| int {
          if(1) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Right 123

    Hspec.it "return definitely throwing (throw)" $ do
      result <- try [C.throwBlock| int {
          if(0) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "Exception: C++ error message; type: std::runtime_error")

    Hspec.it "catch without return (throw)" $ do
      result <- try [C.throwBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `Hspec.shouldBe` Left (C.CppStdException "Exception: C++ error message; type: std::runtime_error")

    Hspec.it "code without exceptions works normally" $ do
      result :: Either C.CppException C.CInt <- try $ C.withPtr_ $ \resPtr -> [C.catchBlock|
          *$(int* resPtr) = 0xDEADBEEF;
        |]

      result `Hspec.shouldBe` Right 0xDEADBEEF
