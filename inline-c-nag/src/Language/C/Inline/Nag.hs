{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Inline.Nag
  ( module Language.C.Inline
    -- * Types
  , Complex(..)
  , NagError(..)
  , _NAG_ERROR_BUF_LEN
  , _NE_NOERROR
  , Nag_Boolean
  , Nag_Integer
  , Nag_Comm
    -- * Context
  , nagCtx
    -- * Utilities
  , withNagError
  ) where

import           Data.Functor ((<$>))
import           Foreign.C.String (peekCStringLen)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))

import           Language.C.Inline.Nag.Internal
import           Language.C.Inline

setContext nagCtx
include "<nag.h>"

withNagError :: (Ptr NagError -> IO a) -> IO (Either String a)
withNagError f = alloca $ \ptr -> do
  [cexp| void{ INIT_FAIL(*$(NagError *ptr)) } |]
  x <- f ptr
  nagErr <- peek ptr
  if nagErrorCode nagErr /= _NE_NOERROR
    then Left <$> peekCStringLen (nagErrorMessage nagErr, fromIntegral _NAG_ERROR_BUF_LEN)
    else return $ Right x
