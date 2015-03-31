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
