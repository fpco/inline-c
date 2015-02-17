{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Context.Nag
  ( -- * Types
    Complex(..)
  , NagError
  , Nag_Boolean(..)
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
import           Control.Monad (mzero)
import           Data.Loc (noLoc)
import           Foreign.Storable (Storable(..))

import           Language.C.Context
import           Language.C.Quote.Nag

#include <nag.h>

newtype Nag_Boolean = Nag_Boolean CInt

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

nagCtx :: Context
nagCtx = baseCtx <> Context
  { ctxCTypes = nagTypes
  , ctxConvertCTypeSpec = nagConvertCTypeSpec
  , ctxSuffixTypes = \s -> lookup s nagSuffixTypes
  }

nagConvertCTypeSpec :: C.TypeSpec -> TH.Q (Maybe TH.Type)
nagConvertCTypeSpec cspec = runMaybeT $
  case C.Type (C.DeclSpec [] [] cspec noLoc) (C.DeclRoot noLoc) noLoc of
    -- TODO this might not be a long, see nag_types.h
    [cty| Integer |] -> lift [t| CLong |]
    [cty| Nag_Boolean |] -> lift [t| Nag_Boolean |]
    [cty| NagError |] -> lift [t| NagError |]
    [cty| Complex |] -> lift [t| Complex |]
    _ -> mzero

nagSuffixTypes :: [(String, C.Type)]
nagSuffixTypes =
  [ ("nint", [cty| Integer |])
  , ("ncompl", [cty| Complex* |])
  , ("nbool", [cty| Nag_Boolean |])
  , ("nfail", [cty| NagError* |])
  ]
