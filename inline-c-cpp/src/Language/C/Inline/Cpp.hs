-- | Module exposing a 'Context' to inline C++ code.  We only have used
-- this for experiments, so use with caution.  See the C++ tests to see
-- how to build inline C++ code.
module Language.C.Inline.Cpp
  ( module Language.C.Inline
  , cppCtx
  , cppTypePairs
  , using
  ) where

import           Data.Monoid ((<>), mempty)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types as CT

import qualified Data.Map as Map

-- | The equivalent of 'C.baseCtx' for C++.  It specifies the @.cpp@
-- file extension for the C file, so that g++ will decide to build C++
-- instead of C.  See the @.cabal@ test target for an example on how to
-- build.
cppCtx :: Context
cppCtx = baseCtx <> mempty
  { ctxForeignSrcLang = Just TH.LangCxx
  , ctxOutput = Just $ \s -> "extern \"C\" {\n" ++ s ++ "\n}"
  }

-- | Emits an @using@ directive, e.g.
--
-- @
-- C.using "namespace std" ==> using namespace std
-- @
using :: String -> TH.DecsQ
using s = verbatim $ "using " ++ s ++ ";"


cppTypePairs :: [(CT.CIdentifier, TH.TypeQ)] -> Context
cppTypePairs typePairs =  mempty {
  ctxTypesTable = Map.fromList $ map (\(cpp_sym, haskell_sym) -> (CT.TypeName cpp_sym, haskell_sym)) typePairs
  }
