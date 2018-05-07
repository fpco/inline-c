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
        ExTypeOtherException ->
          return (Left CppOtherException)
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
      

tryBlockQuoteExp :: String -> Q Exp
tryBlockQuoteExp blockStr = do
  let (ty, body) = C.splitTypedC blockStr
  _ <- C.include "<exception>"
  _ <- C.include "<cstring>"
  _ <- C.include "<cstdlib>"
  typePtrVarName <- newName "exTypePtr"
  msgPtrVarName <- newName "msgPtr"
  let inlineCStr = unlines
        [ ty ++ " {"
        , "  int* __inline_c_cpp_exception_type__ = $(int* " ++ nameBase typePtrVarName ++ ");"
        , "  char** __inline_c_cpp_error_message__ = $(char** " ++ nameBase msgPtrVarName ++ ");"
        , "  try {"
        , body
        , "  } catch (std::exception &e) {"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeStdException ++ ";"
        , "    size_t whatLen = std::strlen(e.what()) + 1;"
        , "    *__inline_c_cpp_error_message__ = static_cast<char*>(std::malloc(whatLen));"
        , "    std::memcpy(*__inline_c_cpp_error_message__, e.what(), whatLen);"
        , "  } catch (...) {"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeOtherException ++ ";"
        , "  }"
        , if ty == "void" then "return;" else "return {};"
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
