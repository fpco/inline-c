-- | A module that contains exception-safe equivalents of @inline-c@ QuasiQuoters.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.C.Inline.Cpp.Exceptions
  ( CppException(..)
  , toSomeException
  , throwBlock
  , tryBlock
  , catchBlock
  ) where

import           Control.Exception.Safe
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Internal as C
import qualified Language.C.Inline.Cpp as Cpp
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Foreign
import           Foreign.C

C.context Cpp.cppCtx
C.include "HaskellException.hxx"

-- | An exception thrown in C++ code.
data CppException
  = CppStdException String
  | CppOtherException (Maybe String) -- contains the exception type, if available.
  | CppHaskellException SomeException
  deriving (Show)

-- | Like 'toException' but unwrap 'CppHaskellException'
toSomeException :: CppException -> SomeException
toSomeException (CppHaskellException e) = e
toSomeException x = toException x

instance Exception CppException

-- NOTE: Other C++ exception types (std::runtime_error etc) could be distinguished like this in the future.
pattern ExTypeNoException :: CInt
pattern ExTypeNoException = 0

pattern ExTypeStdException :: CInt
pattern ExTypeStdException = 1

pattern ExTypeHaskellException :: CInt
pattern ExTypeHaskellException = 2

pattern ExTypeOtherException :: CInt
pattern ExTypeOtherException = 3

handleForeignCatch :: (Ptr CInt -> Ptr CString -> Ptr (Ptr ()) -> IO a) -> IO (Either CppException a)
handleForeignCatch cont =
  alloca $ \exTypePtr ->
  alloca $ \msgPtrPtr ->
  alloca $ \haskellExPtrPtr -> do
    poke exTypePtr ExTypeNoException
    -- we need to mask this entire block because the C++ allocates the
    -- string for the exception message and we need to make sure that
    -- we free it (see the @free@ below). The foreign code would not be
    -- preemptable anyway, so I do not think this loses us anything.
    mask_ $ do
      res <- cont exTypePtr msgPtrPtr haskellExPtrPtr
      exType <- peek exTypePtr
      case exType of
        ExTypeNoException -> return (Right res)
        ExTypeStdException -> do
          msgPtr <- peek msgPtrPtr
          errMsg <- peekCString msgPtr
          free msgPtr
          return (Left (CppStdException errMsg))
        ExTypeHaskellException -> do
          haskellExPtr <- peek haskellExPtrPtr
          stablePtr <- [C.block| void * {
              return (static_cast<HaskellException *>($(void *haskellExPtr)))->haskellExceptionStablePtr->stablePtr;
            } |]
          someExc <- deRefStablePtr (castPtrToStablePtr stablePtr)
          [C.block| void{
              delete static_cast<HaskellException *>($(void *haskellExPtr));
            } |]
          return (Left (CppHaskellException someExc))
        ExTypeOtherException -> do
          msgPtr <- peek msgPtrPtr
          mbExcType <- if msgPtr == nullPtr
            then return Nothing
            else do
              excType <- peekCString msgPtr
              free msgPtr
              return (Just excType)
          return (Left (CppOtherException mbExcType))
        _ -> error "Unexpected C++ exception type."

-- | Like 'tryBlock', but will throw unwrapped 'CppHaskellException's or other 'CppException's rather than returning
-- them in an 'Either'
throwBlock :: QuasiQuoter
throwBlock = QuasiQuoter
  { quoteExp = \blockStr -> do
      [e| either (throwIO . toSomeException) return =<< $(tryBlockQuoteExp blockStr) |]
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
      unsupported _ = fail "Unsupported quasiquotation."

-- | Variant of 'throwBlock' for blocks which return 'void'.
catchBlock :: QuasiQuoter
catchBlock = QuasiQuoter
  { quoteExp = \blockStr -> quoteExp throwBlock ("void {" ++ blockStr ++ "}")
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
      unsupported _ = fail "Unsupported quasiquotation."
      

tryBlockQuoteExp :: String -> Q Exp
tryBlockQuoteExp blockStr = do
  let (ty, body) = C.splitTypedC blockStr
  _ <- C.include "<exception>"
  _ <- C.include "<cstring>"
  _ <- C.include "<cstdlib>"
  _ <- C.include "HaskellException.hxx"
  -- see
  -- <https://stackoverflow.com/questions/28166565/detect-gcc-as-opposed-to-msvc-clang-with-macro>
  -- regarding how to detect g++ or clang.
  --
  -- the defined(__clang__) should actually be redundant, since apparently it also
  -- defines GNUC, but but let's be safe.
  _ <- C.verbatim $ unlines
    [ "#if defined(__GNUC__) || defined(__clang__)"
    , "#include <cxxabi.h>"
    , "#include <string>"
    , "#endif"
    ]
  typePtrVarName <- newName "exTypePtr"
  msgPtrVarName <- newName "msgPtr"
  haskellExPtrVarName <- newName "haskellExPtr"
  -- see
  -- <https://stackoverflow.com/questions/561997/determining-exception-type-after-the-exception-is-caught/47164539#47164539>
  -- regarding how to show the type of an exception.
  let inlineCStr = unlines
        [ ty ++ " {"
        , "  int* __inline_c_cpp_exception_type__ = $(int* " ++ nameBase typePtrVarName ++ ");"
        , "  char** __inline_c_cpp_error_message__ = $(char** " ++ nameBase msgPtrVarName ++ ");"
        , "  HaskellException** __inline_c_cpp_haskellexception__ = (HaskellException**)($(void ** " ++ nameBase haskellExPtrVarName ++ "));"
        , "  try {"
        , body
        , "  } catch (HaskellException &e) {"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeHaskellException ++ ";"
        , "    *__inline_c_cpp_haskellexception__ = new HaskellException(e);"
        , if ty == "void" then "return;" else "return {};"
        , "  } catch (std::exception &e) {"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeStdException ++ ";"
        , "#if defined(__GNUC__) || defined(__clang__)"
        , "    int demangle_status;"
        , "    const char* demangle_result = abi::__cxa_demangle(abi::__cxa_current_exception_type()->name(), 0, 0, &demangle_status);"
        , "    std::string message = \"Exception: \" + std::string(e.what()) + \"; type: \" + std::string(demangle_result);"
        , "#else"
        , "    std::string message = \"Exception: \" + std::string(e.what()) + \"; type: not available (please use g++ or clang)\";"
        , "#endif"
        , "    size_t message_len = message.size() + 1;"
        , "    *__inline_c_cpp_error_message__ = static_cast<char*>(std::malloc(message_len));"
        , "    std::memcpy(*__inline_c_cpp_error_message__, message.c_str(), message_len);"
        , if ty == "void" then "return;" else "return {};"
        , "  } catch (...) {"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeOtherException ++ ";"
        , "#if defined(__GNUC__) || defined(__clang__)"
        , "    int demangle_status;"
        , "    const char* message = abi::__cxa_demangle(abi::__cxa_current_exception_type()->name(), 0, 0, &demangle_status);"
        , "    size_t message_len = strlen(message) + 1;"
        , "    *__inline_c_cpp_error_message__ = static_cast<char*>(std::malloc(message_len));"
        , "    std::memcpy(*__inline_c_cpp_error_message__, message, message_len);"
        , "#else"
        , "    *__inline_c_cpp_error_message__ = NULL;"
        , "#endif"
        , if ty == "void" then "return;" else "return {};"
        , "  }"
        , "}"
        ]
  [e| handleForeignCatch $ \ $(varP typePtrVarName) $(varP msgPtrVarName) $(varP haskellExPtrVarName) -> $(quoteExp C.block inlineCStr) |]
 
-- | Similar to `C.block`, but C++ exceptions will be caught and the result is (Either CppException value). The return type must be void or constructible with @{}@.
-- Using this will automatically include @exception@, @cstring@ and @cstdlib@.
tryBlock :: QuasiQuoter
tryBlock = QuasiQuoter
  { quoteExp = tryBlockQuoteExp
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
      unsupported _ = fail "Unsupported quasiquotation."
