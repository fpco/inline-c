{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Inline.Nag.Internal
  ( -- * Types
    Complex(..)
  , NagError
  , _NAG_ERROR_BUF_LEN
  , _NE_NOERROR
  , Nag_Boolean
  , Nag_ErrorControl
  , Nag_Integer
  , Nag_Comm
  , Nag_User
    -- * Context
  , nagCtx
  ) where

import           Control.Applicative ((<*>))
import           Data.Functor ((<$>))
import qualified Data.Map as Map
import           Data.Monoid ((<>), mempty)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import qualified Language.Haskell.TH as TH

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

data NagError

instance Storable NagError where
    sizeOf _ = (#size NagError)
    alignment _ = alignment (undefined :: Ptr ())
    peek = error "peek not implemented for NagError"
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

data Nag_User
instance Storable Nag_User where
    sizeOf _ = (#size Nag_User)
    alignment _ = alignment (undefined :: Ptr ())
    peek _ = error "peek not implemented for Nag_User"
    poke _ _ = error "poke not implemented for Nag_User"

-- * Enums

type Nag_Boolean = CInt
type Nag_ErrorControl = CInt

-- * Utils

type Nag_Integer = CLong

-- * Context

nagCtx :: Context
nagCtx = baseCtx <> funCtx <> vecCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = nagTypesTable
      }

nagTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
nagTypesTable = Map.fromList
  [ -- TODO this might not be a long, see nag_types.h
    (C.TypeName "Integer", [t| Nag_Integer |])
  , (C.TypeName "Complex", [t| Complex |])
  , (C.TypeName "NagError", [t| NagError |])
  , (C.TypeName "Nag_Boolean", [t| Nag_Boolean |])
  , (C.TypeName "Nag_Comm", [t| Nag_Comm |])
  , (C.TypeName "Nag_User", [t| Nag_User |])
  , (C.TypeName "Nag_ErrorControl", [t| Nag_ErrorControl |])
  ]
