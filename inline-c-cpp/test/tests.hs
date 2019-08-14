{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

import           Control.Exception.Safe
import           Control.Monad
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Context as CC
import qualified Language.C.Types as CT
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Test.Hspec as Hspec
import           Foreign.Ptr (Ptr)
import           Data.List (isInfixOf)

data Test
data Vector a
data Array a

C.context $ C.cppCtx `mappend` C.cppTypePairs [
  ("Test::Test", [t|Test|]),
  ("std::vector", [t|Vector|]),
  ("std::array", [t|Array|])
  ]

C.include "<iostream>"
C.include "<vector>"
C.include "<array>"
C.include "<tuple>"
C.include "<stdexcept>"
C.include "test.h"

main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Basic C++" $ do
    Hspec.it "Hello World" $ do
      let x = 3
      [C.block| void {
          std::cout << "Hello, world!" << $(int x) << std::endl;
        } |]

  Hspec.describe "C++ Types" $ do
    Hspec.it "Hello Namespace" $ do
      pt <- [C.block| Test::Test* {
          return new Test::Test();
        } |] :: IO (Ptr Test)
      [C.block| void {
          std::cout << $(Test::Test* pt)->get() << std::endl;
        } |]

    Hspec.it "Hello Template" $ do
      pt <- [C.block| std::vector<int>* {
          return new std::vector<int>();
        } |] :: IO (Ptr (Vector C.CInt))
      [C.block| void {
          $(std::vector<int>* pt)->push_back(100);
          std::cout << (*$(std::vector<int>* pt))[0] << std::endl;
        } |]

    Hspec.it "Template + Namespace" $ do
      pt <- [C.block| std::vector<Test::Test>* {
          return new std::vector<Test::Test>();
        } |] :: IO (Ptr (Vector Test))
      [C.block| void {
          $(std::vector<Test::Test>* pt)->push_back(Test::Test());
        } |]

    Hspec.it "Template with 2 arguments" $ do
      pt <- [C.block| std::array<int,10>* {
          return new std::array<int,10>();
        } |] :: IO (Ptr (Array '(C.CInt,10)))
      [C.block| void {
          (*$(std::array<int,10>* pt))[0]=true;
          std::cout << (*$(std::array<int,10>* pt))[0] << std::endl;
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

      case result of
        Left (C.CppOtherException (Just ty)) | "string" `isInfixOf` ty -> return ()
        _ -> error ("Expected Left CppOtherException with string type, but got " ++ show result)

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
