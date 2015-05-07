{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Module exposing a 'Context' to inline C++ code.  We only have used
-- this for experiments, so use with caution.  See the C++ tests to see
-- how to build inline C++ code.
module Language.C.Inline.Cpp.Internal
  ( cppCtx
  ) where

import           Data.Monoid ((<>), mempty)

import           Language.C.Inline
import           Language.C.Inline.Context

cppCtx :: Context
cppCtx = baseCtx <> mempty
  { ctxFileExtension = Just "cpp"
  , ctxOutput = Just $ \s -> "extern \"C\" {\n" ++ s ++ "\n}"
  }
