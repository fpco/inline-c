module Language.C.Context.Types
  ( Context(..)
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.C as C
import           Data.Monoid (Monoid(mempty, mappend))
import           Control.Applicative (empty, (<|>))
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

data Context = Context
  { ctxCTypes :: [String]
    -- ^ Additional types for the C parser
  , ctxCToHs :: C.Type -> TH.Q (Maybe TH.Type)
    -- ^ Tries to convert a C type to an Haskell one
  , ctxSuffixTypes :: String -> Maybe C.Type
    -- ^ Tries to get the C type corresponding to the given suffix
  }

instance Monoid Context where
  mempty = Context
    { ctxCTypes = mempty
    , ctxCToHs = \_ -> runMaybeT empty
    , ctxSuffixTypes = \_ -> empty
    }

  mappend ctx1 ctx2 = Context
    { ctxCTypes = ctxCTypes ctx1 ++ ctxCTypes ctx2
    , ctxCToHs = \cty -> runMaybeT $
        MaybeT (ctxCToHs ctx1 cty) <|> MaybeT (ctxCToHs ctx2 cty)
    , ctxSuffixTypes = \suff ->
        ctxSuffixTypes ctx1 suff <|> ctxSuffixTypes ctx2 suff
    }
