-- | Module exposing a 'Context' to inline C++ code.  We only have used
-- this for experiments, so use with caution.  See the C++ tests to see
-- how to build inline C++ code.
module Language.C.Inline.Cpp
  ( module Language.C.Inline
  , cppCtx
  ) where

import           Language.C.Inline
import           Language.C.Inline.Context
import           Data.Monoid ((<>), mempty)

cppCtx :: Context
cppCtx = baseCtx <> mempty
  { ctxFileExtension = Just "cpp"
  , ctxOutput = Just $ \s -> "extern \"C\" {\n" ++ s ++ "\n}"
  }
