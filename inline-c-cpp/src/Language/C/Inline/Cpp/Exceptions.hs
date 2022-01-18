{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Language.C.Inline.Cpp.Exceptions {-# DEPRECATED "Language.C.Inline.Cpp.Exceptions is deprecated in favor of Language.C.Inline.Cpp.Exception which changes the CppException data type to preserve the exception for custom error handling." #-} (
    CppException(CppHaskellException)
  , pattern Language.C.Inline.Cpp.Exceptions.CppStdException
  , pattern Language.C.Inline.Cpp.Exceptions.CppOtherException
  , toSomeException
  , throwBlock
  , tryBlock
  , catchBlock
  ) where


import           Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text as T
import           Language.C.Inline.Cpp.Exception

bsToChars :: ByteString -> String
bsToChars = T.unpack . T.decodeUtf8With T.lenientDecode

cppStdExceptionMessage :: CppException -> Maybe String
cppStdExceptionMessage (Language.C.Inline.Cpp.Exception.CppStdException _ s (Just t)) = Just $ "Exception: " <> bsToChars s <> "; type: " <> bsToChars t
cppStdExceptionMessage (Language.C.Inline.Cpp.Exception.CppStdException _ s Nothing) = Just $ "Exception: " <> bsToChars s <> "; type: not available (please use g++ or clang)"
cppStdExceptionMessage _ = Nothing

cppNonStdExceptionType :: CppException -> Maybe (Maybe String)
cppNonStdExceptionType (CppNonStdException _ mt) = Just (fmap bsToChars mt)
cppNonStdExceptionType _ = Nothing

pattern CppStdException :: String -> CppException
pattern CppStdException s <- (cppStdExceptionMessage -> Just s)

pattern CppOtherException :: Maybe String -> CppException
pattern CppOtherException mt <- (cppNonStdExceptionType -> Just mt)
