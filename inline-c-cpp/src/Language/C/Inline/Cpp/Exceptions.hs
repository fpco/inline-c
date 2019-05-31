-- | A module that contains exception-safe equivalents of @inline-c@ QuasiQuoters.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.C.Inline.Cpp.Exceptions
  ( CppException(..)
  , throwBlock
  , tryBlock
  , catchBlock
  ) where

import           Control.Exception.Safe
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Internal as C
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Foreign
import           Foreign.C

-- | An exception thrown in C++ code.
data CppException
  = CppStdException String
  | CppOtherException (Maybe String) -- contains the exception type, if available.
  deriving (Eq, Ord, Show)

instance Exception CppException

-- NOTE: Other C++ exception types (std::runtime_error etc) could be distinguished like this in the future.
pattern ExTypeNoException :: CInt
pattern ExTypeNoException = 0

pattern ExTypeStdException :: CInt
pattern ExTypeStdException = 1

pattern ExTypeOtherException :: CInt
pattern ExTypeOtherException = 2

handleForeignCatch :: (Ptr CInt -> Ptr CString -> IO a) -> IO (Either CppException a)
handleForeignCatch cont =
  alloca $ \exTypePtr ->
  alloca $ \msgPtrPtr -> do
    poke exTypePtr ExTypeNoException
    -- we need to mask this entire block because the C++ allocates the
    -- string for the exception message and we need to make sure that
    -- we free it (see the @free@ below). The foreign code would not be
    -- preemptable anyway, so I do not think this loses us anything.
    mask_ $ do
      res <- cont exTypePtr msgPtrPtr
      exType <- peek exTypePtr
      case exType of
        ExTypeNoException -> return (Right res)
        ExTypeStdException -> do
          msgPtr <- peek msgPtrPtr
          errMsg <- peekCString msgPtr
          free msgPtr
          return (Left (CppStdException errMsg))
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

-- | Like 'tryBlock', but will throw 'CppException's rather than returning
-- them in an 'Either'
throwBlock :: QuasiQuoter
throwBlock = QuasiQuoter
  { quoteExp = \blockStr -> do
      [e| either throwIO return =<< $(tryBlockQuoteExp blockStr) |]
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

exceptionalValue :: String -> String
exceptionalValue typeStr =
  case typeStr of
    "void" -> ""
    "char" -> "0"
    "short" -> "0"
    "long" -> "0"
    "int" -> "0"
    "int8_t" -> "0"
    "int16_t" -> "0"
    "int32_t" -> "0"
    "int64_t" -> "0"
    "uint8_t" -> "0"
    "uint16_t" -> "0"
    "uint32_t" -> "0"
    "uint64_t" -> "0"
    "float" -> "0"
    "double" -> "0"
    "bool" -> "0"
    "signed char" -> "0"
    "signed short" -> "0"
    "signed int" -> "0"
    "signed long" -> "0"
    "unsigned char" -> "0"
    "unsigned short" -> "0"
    "unsigned int" -> "0"
    "unsigned long" -> "0"
    "size_t" -> "0"
    "wchar_t" -> "0"
    "ptrdiff_t" -> "0"
    "sig_atomic_t" -> "0"
    "intptr_t" -> "0"
    "uintptr_t" -> "0"
    "intmax_t" -> "0"
    "uintmax_t" -> "0"
    "clock_t" -> "0"
    "time_t" -> "0"
    "useconds_t" -> "0"
    "suseconds_t" -> "0"
    "FILE" -> "0"
    "fpos_t" -> "0"
    "jmp_buf" -> "0"
    _ -> "{}"

tryBlockQuoteExp :: String -> Q Exp
tryBlockQuoteExp blockStr = do
  let (ty, body) = C.splitTypedC blockStr
  _ <- C.include "<exception>"
  _ <- C.include "<cstring>"
  _ <- C.include "<cstdlib>"
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
  -- see
  -- <https://stackoverflow.com/questions/561997/determining-exception-type-after-the-exception-is-caught/47164539#47164539>
  -- regarding how to show the type of an exception.
  let inlineCStr = unlines
        [ ty ++ " {"
        , "  int* __inline_c_cpp_exception_type__ = $(int* " ++ nameBase typePtrVarName ++ ");"
        , "  char** __inline_c_cpp_error_message__ = $(char** " ++ nameBase msgPtrVarName ++ ");"
        , "  try {"
        , body
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
        , "    return " ++ exceptionalValue ty ++ ";"
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
        , "    return " ++ exceptionalValue ty ++ ";"
        , "  }"
        , "}"
        ]
  [e| handleForeignCatch $ \ $(varP typePtrVarName) $(varP msgPtrVarName) -> $(quoteExp C.block inlineCStr) |]
 
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
