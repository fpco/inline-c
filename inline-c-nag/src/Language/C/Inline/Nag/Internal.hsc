{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Inline.Nag.Internal
  ( -- * Types
    Complex(..)
  , NagError(..)
  , _NAG_ERROR_BUF_LEN
  , _NE_NOERROR
  , Nag_Boolean
  , Nag_Integer
  , Nag_Comm
    -- * Context
  , nagCtx
  ) where

import qualified Data.Map as Map
import           Data.Monoid ((<>), mempty)
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)
import           Foreign.Storable (Storable(..))
import qualified Language.Haskell.TH as TH
import           Control.Applicative ((<*>))
import           Data.Functor ((<$>))

import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types as C

#include <nag.h>

-- * Records

data Complex = Complex
  { complRe :: {-# UNPACK #-} !CDouble
  , complIm :: {-# UNPACK #-} !CDouble
  } deriving (Show, Read, Eq, Ord)

instance Storable Complex where
  sizeOf _ = (#size Complex)
  alignment _ = alignment (undefined :: Ptr CDouble)
  peek ptr = do
    re <- (#peek Complex, re) ptr
    im <- (#peek Complex, im) ptr
    return Complex{complRe = re, complIm = im}
  poke ptr Complex{..} = do
    (#poke Complex, re) ptr complRe
    (#poke Complex, im) ptr complIm

data NagError = NagError
  { nagErrorCode :: {-# UNPACK #-} !CInt
  , nagErrorPrint :: {-# UNPACK #-} !Nag_Boolean
  , nagErrorMessage :: {-# UNPACK #-} !CString
  , nagErrorHandler :: {-# UNPACK #-} !(FunPtr (Ptr CChar -> CInt -> Ptr CChar -> IO ()))
  , nagErrorErrNum :: {-# UNPACK #-} !Nag_Integer
  , nagErrorIFlag :: {-# UNPACK #-} !Nag_Integer
  , nagErrorIVal :: {-# UNPACK #-} !Nag_Integer
  } deriving (Show, Eq)

instance Storable NagError where
    sizeOf _ = (#size NagError)
    alignment _ = alignment (undefined :: Ptr ())
    peek ptr = do
      NagError
        <$> (#peek NagError, code) ptr
        <*> (#peek NagError, print) ptr
        <*> (#peek NagError, message) ptr
        <*> (#peek NagError, handler) ptr
        <*> (#peek NagError, errnum) ptr
        <*> (#peek NagError, iflag) ptr
        <*> (#peek NagError, ival) ptr
    poke _ _ = error "poke not implemented for NagError"

_NE_NOERROR :: CInt
_NE_NOERROR = (#const NE_NOERROR)

_NAG_ERROR_BUF_LEN :: CInt
_NAG_ERROR_BUF_LEN = (#const NAG_ERROR_BUF_LEN)

data Nag_Comm
instance Storable Nag_Comm where
    sizeOf _ = (#size Nag_Comm)
    alignment _ = alignment (undefined :: Ptr ())
    peek _ = error "peek not implemented for Nag_Comm"
    poke _ _ = error "poke not implemented for Nag_Comm"

-- * Enums

type Nag_Boolean = CInt
type Nag_Integer = CLong

-- * Context

nagCtx :: Context
nagCtx = baseCtx <> funCtx <> vecCtx <> ctx
  where
    ctx = mempty
      { ctxCTypesTable = nagCTypesTable
      }

nagCTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
nagCTypesTable = Map.fromList
  [ -- TODO this might not be a long, see nag_types.h
    (C.TypeName "Integer", [t| Nag_Integer |])
  , (C.TypeName "Complex", [t| Complex |])
  , (C.TypeName "NagError", [t| NagError |])
  , (C.TypeName "Nag_Boolean", [t| Nag_Boolean |])
  , (C.TypeName "Nag_Comm", [t| Nag_Comm |])
  ]
