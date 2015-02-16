module Language.C.Context
  ( Context(..)
  , baseCtx
  ) where

import           Language.C.Context.Types
import           Language.C.Context.Base

    -- hasSuffixType :: String -> Maybe (String, C.Type)
    -- hasSuffixType s = msum
    --   [ do guard (('_' : suff) `isSuffixOf` s)
    --        return (take (length s - length suff - 1) s, ctype)
    --   | (suff, ctype) <- suffixTypes
    --   ]
