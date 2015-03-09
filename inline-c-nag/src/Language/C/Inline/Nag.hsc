{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Inline.Nag
  ( module Language.C.Inline
    -- * Types
  , Complex(..)
  , NagError
  , Nag_Boolean(..)
  , Nag_Comm
  , Nag_E05State
    -- * Context
  , nagCtx
  ) where

import qualified Language.Haskell.TH as TH
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Data.Monoid ((<>))
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (mzero, guard, msum)
import           Foreign.Storable (Storable(..))
import           Data.List (isSuffixOf)
import           Data.Monoid (mempty)
import qualified Data.Map as Map

import           Language.C.Inline
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
    peek _ = error "peek not implemented for NagError"
    poke _ _ = error "poke not implemented for NagError"

data Nag_Comm
instance Storable Nag_Comm where
    sizeOf _ = (#size Nag_Comm)
    alignment _ = alignment (undefined :: Ptr ())
    peek _ = error "peek not implemented for Nag_Comm"
    poke _ _ = error "poke not implemented for Nag_Comm"

data Nag_E05State
instance Storable Nag_E05State where
    sizeOf _ = (#size Nag_E05State)
    alignment _ = alignment (undefined :: Ptr ())
    peek _ = error "peek not implemented for Nag_E05State"
    poke _ _ = error "poke not implemented for Nag_E05State"

-- * Enums

type Nag_Boolean = CInt
type Nag_BoundType = CInt
type Nag_MCSInitMethod = CInt

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
    (C.TypeName "Integer", [t| CLong |])
  , (C.TypeName "Complex", [t| Complex |])
  , (C.TypeName "NagError", [t| NagError |])
  , (C.TypeName "Nag_Boolean", [t| Nag_Boolean |])
  , (C.TypeName "Nag_Comm", [t| Nag_Comm |])
  , (C.TypeName "Nag_E05State", [t| Nag_E05State |])
  ]
