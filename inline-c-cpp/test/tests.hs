{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception.Safe
import           Control.Monad
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import           Foreign.C.String (withCString)
import           Foreign.StablePtr (StablePtr, newStablePtr, castStablePtrToPtr)
import qualified Test.Hspec as Hspec
import           Data.List (isInfixOf)

C.context C.cppCtx

C.include "<iostream>"
C.include "<stdexcept>"

data MyCustomException = MyCustomException Int
  deriving (Eq, Show, Typeable)
instance Exception MyCustomException

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

      result `shouldBeCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "non-exceptions are caught (unsigned int)" $ do
      result <- try [C.catchBlock|
        throw 0xDEADBEEF;
        |]

      result `shouldBeCppOtherException` (Just "unsigned int")

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

      result `shouldBeCppStdException` "Exception: C++ error message; type: std::runtime_error"

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

      result `shouldBeCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "catch without return (pure)" $ do
      result <- [C.tryBlock| void {
          throw std::runtime_error("C++ error message");
        }
        |]

      result `shouldBeCppStdException` "Exception: C++ error message; type: std::runtime_error"

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

      result `shouldBeCppStdException` "Exception: C++ error message; type: std::runtime_error"

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

      result `shouldBeCppStdException` "Exception: C++ error message; type: std::runtime_error"

    Hspec.it "code without exceptions works normally" $ do
      result :: Either C.CppException C.CInt <- try $ C.withPtr_ $ \resPtr -> [C.catchBlock|
          *$(int* resPtr) = 0xDEADBEEF;
        |]

      result `shouldBeRight` 0xDEADBEEF

tag :: C.CppException -> String
tag (C.CppStdException {}) = "CppStdException"
tag (C.CppHaskellException {}) = "CppHaskellException"
tag (C.CppOtherException {}) = "CppStdException"

shouldBeCppStdException :: Either C.CppException a -> String -> IO ()
shouldBeCppStdException (Left (C.CppStdException actualMsg)) expectedMsg = do
  actualMsg `Hspec.shouldBe` expectedMsg
shouldBeCppStdException (Left x) expectedMsg = tag x `Hspec.shouldBe` ("CppStdException " <> show expectedMsg)
shouldBeCppStdException (Right _) expectedMsg = "Right _" `Hspec.shouldBe` ("Left (CppStdException " <> show expectedMsg <> ")")

shouldBeCppOtherException :: Either C.CppException a -> Maybe String -> IO ()
shouldBeCppOtherException (Left (C.CppOtherException actualType)) expectedType = do
  actualType `Hspec.shouldBe` expectedType
shouldBeCppOtherException (Left x) expectedType = tag x `Hspec.shouldBe` ("CppOtherException " <> show expectedType)
shouldBeCppOtherException (Right _) expectedType = "Right _" `Hspec.shouldBe` ("Left (CppOtherException " <> show expectedType <> ")")

shouldBeRight :: (Eq a, Show a) => Either C.CppException a -> a -> IO ()
shouldBeRight (Right actual) expected = actual `Hspec.shouldBe` expected
shouldBeRight (Left e) expected = ("Left (" <> tag e <> " {})") `Hspec.shouldBe` ("Right " <> (show expected))
