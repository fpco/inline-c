-- | A module that contains exception-safe equivalents of @inline-c@ QuasiQuoters.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.C.Inline.Cpp.Exception
  ( CppException(..)
  , CppExceptionPtr
  , toSomeException
  , throwBlock
  , tryBlock
  , catchBlock
  ) where

import           Control.Exception.Safe
import qualified Data.ByteString.Unsafe as BS (unsafePackMallocCString)
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Internal as C
import qualified Language.C.Inline.Cpp as Cpp
import           Language.C.Inline.Cpp (AbstractCppExceptionPtr)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Foreign
import           Foreign.C
import           System.IO.Unsafe(unsafePerformIO)

C.context Cpp.cppCtx
C.include "HaskellException.hxx"

-- | An exception thrown in C++ code.
data CppException
  = CppStdException CppExceptionPtr ByteString (Maybe ByteString)
  | CppHaskellException SomeException
  | CppNonStdException CppExceptionPtr (Maybe ByteString)

instance Show CppException where
  showsPrec p (CppStdException _ msg typ) = showParen (p >= 11) (showString "CppStdException e " . showsPrec 11 msg . showsPrec 11 typ)
  showsPrec p (CppHaskellException e) = showParen (p >= 11) (showString "CppHaskellException " . showsPrec 11 e)
  showsPrec p (CppNonStdException _ typ) = showParen (p >= 11) (showString "CppOtherException e " . showsPrec 11 typ)

instance Exception CppException where
  displayException (CppStdException _ msg _typ) = bsToChars msg
  displayException (CppHaskellException e) = displayException e
  displayException (CppNonStdException _ (Just typ)) = "exception: Exception of type " <> bsToChars typ
  displayException (CppNonStdException _ Nothing) = "exception: Non-std exception of unknown type"

type CppExceptionPtr = ForeignPtr AbstractCppExceptionPtr

unsafeFromNewCppExceptionPtr :: Ptr AbstractCppExceptionPtr -> IO CppExceptionPtr
unsafeFromNewCppExceptionPtr p = newForeignPtr finalizeAbstractCppExceptionPtr p

finalizeAbstractCppExceptionPtr :: FinalizerPtr AbstractCppExceptionPtr
{-# NOINLINE finalizeAbstractCppExceptionPtr #-}
finalizeAbstractCppExceptionPtr =
  unsafePerformIO
    [C.exp|
      void (*)(std::exception_ptr *) {
        [](std::exception_ptr *v){ delete v; }
      }|]

-- | Like 'toException' but unwrap 'CppHaskellException'
toSomeException :: CppException -> SomeException
toSomeException (CppHaskellException e) = e
toSomeException x = toException x

-- NOTE: Other C++ exception types (std::runtime_error etc) could be distinguished like this in the future.
pattern ExTypeNoException :: CInt
pattern ExTypeNoException = 0

pattern ExTypeStdException :: CInt
pattern ExTypeStdException = 1

pattern ExTypeHaskellException :: CInt
pattern ExTypeHaskellException = 2

pattern ExTypeOtherException :: CInt
pattern ExTypeOtherException = 3


handleForeignCatch :: (Ptr CInt -> Ptr CString -> Ptr CString -> Ptr (Ptr AbstractCppExceptionPtr) -> Ptr (Ptr ()) -> IO a) -> IO (Either CppException a)
handleForeignCatch cont =
  alloca $ \exTypePtr ->
  alloca $ \msgCStringPtr ->
  alloca $ \typCStringPtr ->
  alloca $ \exPtr ->
  alloca $ \haskellExPtrPtr -> do
    poke exTypePtr ExTypeNoException
    -- we need to mask this entire block because the C++ allocates the
    -- string for the exception message and we need to make sure that
    -- we free it (see the @free@ below). The foreign code would not be
    -- preemptable anyway, so I do not think this loses us anything.
    mask_ $ do
      res <- cont exTypePtr msgCStringPtr typCStringPtr exPtr haskellExPtrPtr
      exType <- peek exTypePtr
      case exType of
        ExTypeNoException -> return (Right res)
        ExTypeStdException -> do
          ex <- unsafeFromNewCppExceptionPtr =<< peek exPtr
          errMsg <- BS.unsafePackMallocCString =<< peek msgCStringPtr
          mbExcType <- maybePeek BS.unsafePackMallocCString =<< peek typCStringPtr
          return (Left (CppStdException ex errMsg mbExcType))
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
          ex <- unsafeFromNewCppExceptionPtr =<< peek exPtr
          mbExcType <- maybePeek BS.unsafePackMallocCString =<< peek typCStringPtr
          return (Left (CppNonStdException ex mbExcType)) :: IO (Either CppException a)
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
  _ <- C.include "HaskellException.hxx"
  typePtrVarName <- newName "exTypePtr"
  msgPtrVarName <- newName "msgPtr"
  haskellExPtrVarName <- newName "haskellExPtr"
  exPtrVarName <- newName "exPtr"
  typeStrPtrVarName <- newName "typeStrPtr"
  let inlineCStr = unlines
        [ ty ++ " {"
        , "  int* __inline_c_cpp_exception_type__ = $(int* " ++ nameBase typePtrVarName ++ ");"
        , "  const char** __inline_c_cpp_error_message__ = $(const char** " ++ nameBase msgPtrVarName ++ ");"
        , "  const char** __inline_c_cpp_error_typ__ = $(const char** " ++ nameBase typeStrPtrVarName ++ ");"
        , "  HaskellException** __inline_c_cpp_haskellexception__ = (HaskellException**)($(void ** " ++ nameBase haskellExPtrVarName ++ "));"
        , "  std::exception_ptr** __inline_c_cpp_exception_ptr__ = (std::exception_ptr**)$(std::exception_ptr** " ++ nameBase exPtrVarName ++ ");"
        , "  try {"
        , body
        , "  } catch (const HaskellException &e) {"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeHaskellException ++ ";"
        , "    *__inline_c_cpp_haskellexception__ = new HaskellException(e);"
        , "    return " ++ exceptionalValue ty ++ ";"
        , "  } catch (const std::exception &e) {"
        , "    *__inline_c_cpp_exception_ptr__ = new std::exception_ptr(std::current_exception());"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeStdException ++ ";"
        , "    setMessageOfStdException(e, __inline_c_cpp_error_message__, __inline_c_cpp_error_typ__);"
        , "    return " ++ exceptionalValue ty ++ ";"
        , "  } catch (...) {"
        , "    *__inline_c_cpp_exception_ptr__ = new std::exception_ptr(std::current_exception());"
        , "    *__inline_c_cpp_exception_type__ = " ++ show ExTypeOtherException ++ ";"
        , "    setCppExceptionType(__inline_c_cpp_error_typ__);"
        , "    return " ++ exceptionalValue ty ++ ";"
        , "  }"
        , "}"
        ]
  [e| handleForeignCatch $ \ $(varP typePtrVarName) $(varP msgPtrVarName) $(varP typeStrPtrVarName) $(varP exPtrVarName) $(varP haskellExPtrVarName) -> $(quoteExp C.block inlineCStr) |]

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

bsToChars :: ByteString -> String
bsToChars = T.unpack . T.decodeUtf8With T.lenientDecode
