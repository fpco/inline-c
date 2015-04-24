-- | Module exposing a 'Context' to inline C++ code.  We only have used
-- this for experiments, so use with caution.  See the C++ tests to see
-- how to build inline C++ code.
module Language.C.Inline.Cpp
  ( module Language.C.Inline
  , cppCtx
  , using
  ) where

import           Data.Monoid ((<>), mempty)
import qualified Language.Haskell.TH as TH

import           Language.C.Inline
import           Language.C.Inline.Context

cppCtx :: Context
cppCtx = baseCtx <> mempty
  { ctxFileExtension = Just "cpp"
  , ctxOutput = Just $ \s -> "extern \"C\" {\n" ++ s ++ "\n}"
  }

using :: String -> TH.DecsQ
using s = literal $ "using " ++ s ++ ";"
