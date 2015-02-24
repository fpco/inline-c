{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Inline.Nag
  ( -- * Types
    Complex(..)
  , NagError
  , Nag_Boolean(..)
  , Nag_Comm
  , Nag_E05State
    -- * Context
  , nagCtx
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.C as C
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Data.Monoid ((<>))
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (mzero, guard, msum)
import           Data.Loc (noLoc)
import           Foreign.Storable (Storable(..))
import           Data.List (isSuffixOf)

import           Language.C.Inline
import           Language.C.Quote.Nag

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
nagCtx = baseCtx <> Context
  { ctxCTypes = nagTypes
  , ctxConvertCTypeSpec = nagConvertCTypeSpec
  , ctxGetSuffixType = nagGetSuffixType
  }

nagConvertCTypeSpec :: C.TypeSpec -> TH.Q (Maybe TH.Type)
nagConvertCTypeSpec cspec = runMaybeT $
  case C.Type (C.DeclSpec [] [] cspec noLoc) (C.DeclRoot noLoc) noLoc of
    -- TODO this might not be a long, see nag_types.h
    [cty| Integer |] -> lift [t| CLong |]
    [cty| Complex |] -> lift [t| Complex |]
    [cty| NagError |] -> lift [t| NagError |]
    [cty| Nag_Boolean |] -> lift [t| Nag_Boolean |]
    [cty| Nag_Comm |] -> lift [t| Nag_Comm |]
    [cty| Nag_E05State |] -> lift [t| Nag_E05State |]
    _ -> mzero

nagGetSuffixType :: String -> Maybe (String, C.Type)
nagGetSuffixType s = msum
  [ do guard (('_' : suff) `isSuffixOf` s)
       return (take (length s - length suff - 1) s, ctype)
  | (suff, ctype) <- table
  ]
  where
  table =
    [ ("nint", [cty| Integer |])
    , ("ncompl", [cty| Complex |])
    , ("nbool", [cty| Nag_Boolean |])
    , ("nfail", [cty| NagError |])

    , ("nint_ptr", [cty| Integer* |])
    , ("ncompl_ptr", [cty| Complex* |])
    , ("nbool_ptr", [cty| Nag_Boolean* |])
    , ("nfail_ptr", [cty| NagError* |])
    ]
