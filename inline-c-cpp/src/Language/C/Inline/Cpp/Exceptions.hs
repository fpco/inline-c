-- | A module that contains exception-safe equivalents of @inline-c@ QuasiQuoters.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.C.Inline.Cpp.Exceptions
  ( CppException(..)
  , catchBlock
  ) where

import           Control.Exception.Safe
import qualified Language.C.Inline as C
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Foreign
import           Foreign.C

-- | An exception thrown in C++ code.
data CppException
  = CppStdException String
  | CppOtherException
  deriving (Eq, Ord, Show)

instance Exception CppException

-- NOTE: Other C++ exception types (std::runtime_error etc) could be distinguished like this in the future.
pattern ExTypeNoException :: CInt
pattern ExTypeNoException = 0

pattern ExTypeStdException :: CInt
pattern ExTypeStdException = 1

pattern ExTypeOtherException :: CInt
pattern ExTypeOtherException = 2

handleForeign :: (Ptr CInt -> Ptr CString -> IO ()) -> IO ()
handleForeign cont =
  alloca $ \exTypePtr ->
  alloca $ \msgPtrPtr -> do
    poke exTypePtr ExTypeNoException
    cont exTypePtr msgPtrPtr `finally` do
      exType <- peek exTypePtr
      case exType of
        ExTypeNoException -> return ()
        ExTypeStdException -> do
          msgPtr <- peek msgPtrPtr
          errMsg <- peekCString msgPtr
          free msgPtr
          throwM $ CppStdException errMsg
        ExTypeOtherException ->
          throwM CppOtherException
        _ -> error "Unexpected C++ exception type."

-- | Similar to `C.block`, but C++ exceptions will be caught and rethrown as `ForeignException`s.
-- Unlike `C.block`, the return type can only be @void@ (and doesn't need to be specified), but you can use `C.withPtr_` to extract a result yourself.
--
-- Using this will automatically include @exception@, @cstring@ and @cstdlib@.
catchBlock :: QuasiQuoter
catchBlock = QuasiQuoter
  { quoteExp = \blockStr -> do
      _ <- C.include "<exception>"
      _ <- C.include "<cstring>"
      _ <- C.include "<cstdlib>"
      typePtrVarName <- newName "exTypePtr"
      msgPtrVarName <- newName "msgPtr"
      let inlineCStr = unlines
            [ "void {"
            , "  int* __inline_c_cpp_exception_type__ = $(int* " ++ nameBase typePtrVarName ++ ");"
            , "  char** __inline_c_cpp_error_message__ = $(char** " ++ nameBase msgPtrVarName ++ ");"
            , "  try {"
            , blockStr
            , "  } catch (std::exception &e) {"
            , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeStdException ++ ";"
            , "    size_t whatLen = std::strlen(e.what()) + 1;"
            , "    *__inline_c_cpp_error_message__ = static_cast<char*>(std::malloc(whatLen));"
            , "    std::memcpy(*__inline_c_cpp_error_message__, e.what(), whatLen);"
            , "  } catch (...) {"
            , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeOtherException ++ ";"
            , "  }"
            , "}"
            ]
      [e| handleForeign $ \ $(varP typePtrVarName) $(varP msgPtrVarName) -> $(quoteExp C.block inlineCStr) |]

  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
      unsupported _ = fail "Unsupported quasiquotation."
