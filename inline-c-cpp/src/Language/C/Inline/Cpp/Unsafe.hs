{-# LANGUAGE PatternSynonyms #-}
-- | A module that contains exception-safe equivalents of @inline-c@ QuasiQuoters.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.C.Inline.Cpp.Unsafe
  ( throwBlock
  , tryBlock
  , catchBlock
  , toSomeException
  ) where

import           Control.Exception.Safe
import qualified Language.C.Inline.Unsafe as Unsafe
import           Language.Haskell.TH.Quote
import           Language.C.Inline.Cpp.Exception (tryBlockQuoteExp)
import           Language.C.Inline.Cpp.Exception (tryBlockQuoteExp,toSomeException)

-- | Like 'tryBlock', but will throw unwrapped 'CppHaskellException's or other 'CppException's rather than returning
-- them in an 'Either'
throwBlock :: QuasiQuoter
throwBlock = QuasiQuoter
  { quoteExp = \blockStr -> do
      [e| either (throwIO . toSomeException) return =<< $(tryBlockQuoteExp Unsafe.block blockStr) |]
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

-- | Similar to `C.block`, but C++ exceptions will be caught and the result is (Either CppException value). The return type must be void or constructible with @{}@.
-- Using this will automatically include @exception@, @cstring@ and @cstdlib@.
tryBlock :: QuasiQuoter
tryBlock = QuasiQuoter
  { quoteExp = tryBlockQuoteExp Unsafe.block
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
      unsupported _ = fail "Unsupported quasiquotation."
