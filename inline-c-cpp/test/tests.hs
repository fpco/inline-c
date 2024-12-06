{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import           Control.Exception.Safe
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Context as CC
import qualified Language.C.Types as CT
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Cpp.Exceptions as Legacy
import           Foreign.C.String (withCString)
import           Foreign.StablePtr (StablePtr, newStablePtr, castStablePtrToPtr)
import qualified Test.Hspec as Hspec
import           Test.Hspec (shouldBe)
import           Foreign.Ptr (Ptr)
import           Data.List (isInfixOf)
import           Data.Monoid
import qualified StdVector
import qualified Data.Vector.Storable as VS


data Test
data Array a
data Tuple a

C.context $ C.cppCtx <> C.fptrCtx <> C.cppTypePairs [
  ("Test::Test", [t|Test|]),
  ("std::array", [t|Array|]),
  ("std::tuple", [t|Tuple|])
  ] `mappend` StdVector.stdVectorCtx

C.include "<iostream>"
C.include "<vector>"
C.include "<array>"
C.include "<tuple>"
C.include "<stdexcept>"
C.include "test.h"

data MyCustomException = MyCustomException Int
  deriving (Eq, Show, Typeable)
instance Exception MyCustomException

StdVector.instanceStdVector "int"
StdVector.instanceStdVector "double"

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
        } |] :: IO (Ptr (StdVector.CStdVector C.CInt))
      [C.block| void {
          $(std::vector<int>* pt)->push_back(100);
          std::cout << (*$(std::vector<int>* pt))[0] << std::endl;
        } |]

    Hspec.it "Template + Namespace" $ do
      pt <- [C.block| std::vector<Test::Test>* {
          return new std::vector<Test::Test>();
        } |] :: IO (Ptr (StdVector.CStdVector Test))
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

    Hspec.it "Template with 6 arguments" $ do
      pt <- [C.block| std::tuple<int,int,int,int,int,int>* {
          return NULL;
      } |] :: IO (Ptr (Tuple '(C.CInt,C.CInt,C.CInt,C.CInt,C.CInt,C.CInt)))
      [C.block| void {
          $(std::tuple<int,int,int,int,int,int>* pt) = NULL;
        } |]

  Hspec.describe "Exception handling" $ do
    Hspec.it "std::exceptions are caught" $ do
      result <- try [C.catchBlock|
        throw std::runtime_error("C++ error message");
        |]

      result `shouldBeCppStdException` ("C++ error message", Just "std::runtime_error")
      result `shouldBeLegacyCppStdException` "Exception: C++ error message; type: std::runtime_error"
      -- Test that we don't accidentally mess up formatting:
      result `shouldBeShownException` "CppStdException e \"C++ error message\" (Just \"std::runtime_error\")"

    Hspec.it "non-exceptions are caught (unsigned int)" $ do
      result <- try [C.catchBlock|
        throw 0xDEADBEEF;
        |]

      result `shouldBeCppNonStdException` (Just "unsigned int")
      result `shouldBeLegacyCppOtherException` (Just "unsigned int")

    Hspec.it "non-exceptions are caught (void *)" $ do
      result <- try [C.catchBlock|
        throw (void *)0xDEADBEEF;
        |]

      result `shouldBeCppNonStdException` (Just "void*")
      result `shouldBeLegacyCppOtherException` (Just "void*")

    Hspec.it "non-exceptions are caught (std::string)" $ do
      result <- try [C.catchBlock|
        throw std::string("FOOBAR");
        |]

      case result of
        Left (C.CppNonStdException ex (Just ty)) -> do
          ("string" `BS.isInfixOf` ty) `shouldBe` True
          [C.throwBlock| int {
            std::exception_ptr *e = $fptr-ptr:(std::exception_ptr *ex);
            if (!e) throw std::runtime_error("Exception was null");
            try {
              std::cerr << "throwing..." << std::endl;
              std::rethrow_exception(*e);
            } catch (std::string &foobar) {
              if (foobar == "FOOBAR")
                return 42;
              else
                return 1;
            } catch (...) {
              return 2;
            }
            return 3;
          }|] >>= \r -> r `shouldBe` 42
        _ -> error ("Expected Left CppOtherException with string type, but got " ++ show result)

    Hspec.it "catch without return (pure)" $ do
      result <- [C.tryBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeCppStdException` ("C++ error message", Just "std::runtime_error")
      result `shouldBeLegacyCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "try and return without throwing (pure)" $ do
      result <- [C.tryBlock| int {
          return 123;
        }
        |]

      result `shouldBeRight` 123

    Hspec.it "return maybe throwing (pure)" $ do
      result <- [C.tryBlock| int {
          if(1) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeRight` 123

    Hspec.it "return definitely throwing (pure)" $ do
      result <- [C.tryBlock| int {
          if(0) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeCppStdException` ("C++ error message", Just "std::runtime_error")
      result `shouldBeLegacyCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "catch without return (pure)" $ do
      result <- [C.tryBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeCppStdException` ("C++ error message", Just "std::runtime_error")
      result `shouldBeLegacyCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "try and return without throwing (throw)" $ do
      result :: Either C.CppException C.CInt <- try [C.throwBlock| int {
          return 123;
        }
        |]

      result `shouldBeRight` 123

    Hspec.it "return maybe throwing (throw)" $ do
      result :: Either C.CppException C.CInt <- try [C.throwBlock| int {
          if(1) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeRight` 123

    Hspec.it "return definitely throwing (throw)" $ do
      result <- try [C.throwBlock| int {
          if(0) return 123;
          else throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeCppStdException` ("C++ error message", Just "std::runtime_error")
      result `shouldBeLegacyCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "return throwing Haskell" $ do
      let exc = toException $ userError "This is from Haskell"

      let doIt = withCString (displayException exc) $ \renderedException -> do

                    stablePtr <- newStablePtr exc
                    let stablePtr' = castStablePtrToPtr stablePtr

                    [C.throwBlock| int {
                        if(0) return 123;
                        else throw HaskellException(HaskellException(std::string($(const char *renderedException)), $(void *stablePtr')));
                      }
                      |]

      let isTheError e | e == userError "This is from Haskell" = True
          isTheError _ = False

      doIt `Hspec.shouldThrow` isTheError

    Hspec.it "return throwing custom Haskell exception" $ do
      let exc = toException $ MyCustomException 42

      let doIt = withCString (displayException exc) $ \renderedException -> do

                    stablePtr <- newStablePtr exc
                    let stablePtr' = castStablePtrToPtr stablePtr

                    [C.throwBlock| int {
                        if(0) return 123;
                        else throw HaskellException(HaskellException(std::string($(const char *renderedException)), $(void *stablePtr')));
                      }
                      |]

      let isTheError (MyCustomException 42) = True
          isTheError _ = False

      doIt `Hspec.shouldThrow` isTheError

    Hspec.it "catch without return (throw)" $ do
      result <- try [C.throwBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeCppStdException` ("C++ error message", Just "std::runtime_error")
      result `shouldBeLegacyCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "code without exceptions works normally" $ do
      result :: Either C.CppException C.CInt <- try $ C.withPtr_ $ \resPtr -> [C.catchBlock|
          *$(int* resPtr) = 0xDEADBEEF;
        |]

      result `shouldBeRight` 0xDEADBEEF

    Hspec.it "code can contain preprocessor directives" $ do
      result <- try $ [C.throwBlock| int {
          #ifndef THE_MACRO_THAT_HAS_NOT_BEEN_DEFINED
          return 0xDEADBEEF;
          #else
          return 0xBEEFCAFE;
          #endif
        } |]

      result `shouldBeRight` 0xDEADBEEF

    {- Manual test cases for testing lineDirective and splitTypedC -- For CI, uncomment this line.

    Hspec.it "error reporting test case" $ do
      result <- try $ [C.throwBlock| int { 0 = 0; return 0xDEADBEEF; /* Test this line. */}|]
      result `shouldBeRight` 0xDEADBEEF

    Hspec.it "error reporting test case" $ do
      result <- try $ [C.throwBlock| int
        { 1 = 1; return 0xDEADBEEF; /* Test this line. */}
      |]
      result `shouldBeRight` 0xDEADBEEF

    Hspec.it "error reporting test case" $ do
      result <- try $ [C.throwBlock| int
        {
          2 = 2; /* Test this line. */
          return 0xDEADBEEF;
        }
      |]
      result `shouldBeRight` 0xDEADBEEF

    Hspec.it "error reporting test case" $ do
      result <- try $ [C.throwBlock|
        int
        {
          3 = 3;  /* Test this line. */
          return 0xDEADBEEF;
        }
      |]
      result `shouldBeRight` 0xDEADBEEF

    Hspec.it "error reporting test case" $ do
      result <- try $ [C.throwBlock|

        int
        {
          4 = 4;  /* Test this line. */
          return 0xDEADBEEF;
        }
      |]
      result `shouldBeRight` 0xDEADBEEF
    -- For CI, uncomment this line. -}

  Hspec.describe "Macros" $ do
    Hspec.it "generated std::vector instances work correctly" $ do
      intVec <- StdVector.new @C.CInt
      StdVector.pushBack intVec 4
      StdVector.pushBack intVec 5
      hsIntVec <- StdVector.toVector intVec
      VS.toList hsIntVec `shouldBe` [ 4, 5 ]

      doubleVec <- StdVector.new @C.CDouble
      StdVector.pushBack doubleVec 4.3
      StdVector.pushBack doubleVec 6.7
      hsDoubleVec <- StdVector.toVector doubleVec
      VS.toList hsDoubleVec `shouldBe` [ 4.3, 6.7 ]

  Hspec.it "Template with pointers" $ do
    pt <- [C.block| std::vector<int*>* {
        return new std::vector<int*>();
      } |] :: IO (Ptr (StdVector.CStdVector (Ptr C.CInt)))
    [C.block| void {
        int *a = new int;
        *a = 100;
        $(std::vector<int*>* pt)->push_back(a);
        std::cout << *((*$(std::vector<int*>* pt))[0]) << std::endl;
        delete a;
        delete $(std::vector<int*>* pt);
      } |]

tag :: C.CppException -> String
tag (C.CppStdException {}) = "CppStdException"
tag (C.CppHaskellException {}) = "CppHaskellException"
tag (C.CppNonStdException {}) = "CppNonStdException"

shouldBeShownException :: Either C.CppException a -> String -> IO ()
shouldBeShownException (Left e) expectedStr = show e `shouldBe` expectedStr
shouldBeShownException (Right _) _expectedStr = "Right _" `Hspec.shouldBe` "Left _"

shouldBeCppStdException :: Either C.CppException a -> (ByteString, Maybe ByteString) -> IO ()
shouldBeCppStdException (Left (C.CppStdException _ actualMsg actualType)) (expectedMsg, expectedType) = do
  (actualMsg, actualType) `shouldBe` (expectedMsg, expectedType)
shouldBeCppStdException (Left x) expectedMsg = tag x `Hspec.shouldBe` ("CppStdException " <> show expectedMsg)
shouldBeCppStdException (Right _) expectedMsg = "Right _" `Hspec.shouldBe` ("Left (CppStdException " <> show expectedMsg <> ")")

-- | Tests that the old, deprecated exception's module and error messages still work.
shouldBeLegacyCppStdException :: Either Legacy.CppException a -> String -> IO ()
shouldBeLegacyCppStdException (Left (Legacy.CppStdException actualMsg)) expectedMsg = do
  actualMsg `Hspec.shouldBe` expectedMsg
shouldBeLegacyCppStdException (Left x) expectedMsg = tag x `Hspec.shouldBe` ("CppStdException " <> show expectedMsg)
shouldBeLegacyCppStdException (Right _) expectedMsg = "Right _" `Hspec.shouldBe` ("Left (CppStdException " <> show expectedMsg <> ")")

shouldBeCppNonStdException :: Either C.CppException a -> Maybe ByteString -> IO ()
shouldBeCppNonStdException (Left (C.CppNonStdException _ actualType)) expectedType = do
  actualType `Hspec.shouldBe` expectedType
shouldBeCppNonStdException (Left x) expectedType = tag x `Hspec.shouldBe` ("CppOtherException " <> show expectedType)
shouldBeCppNonStdException (Right _) expectedType = "Right _" `Hspec.shouldBe` ("Left (CppOtherException " <> show expectedType <> ")")

-- | Tests that the old, deprecated exception's module and error messages still work.
shouldBeLegacyCppOtherException :: Either Legacy.CppException a -> Maybe String -> IO ()
shouldBeLegacyCppOtherException (Left (Legacy.CppOtherException actualType)) expectedType = do
  actualType `Hspec.shouldBe` expectedType
shouldBeLegacyCppOtherException (Left x) expectedType = tag x `Hspec.shouldBe` ("CppOtherException " <> show expectedType)
shouldBeLegacyCppOtherException (Right _) expectedType = "Right _" `Hspec.shouldBe` ("Left (CppOtherException " <> show expectedType <> ")")

shouldBeRight :: (Eq a, Show a) => Either C.CppException a -> a -> IO ()
shouldBeRight (Right actual) expected = actual `Hspec.shouldBe` expected
shouldBeRight (Left e) expected = ("Left (" <> tag e <> " {})") `Hspec.shouldBe` ("Right " <> (show expected))
