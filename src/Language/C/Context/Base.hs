{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.C.Context.Base
  ( baseCtx
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.C as C
import qualified Language.C.Quote.C as C
import           Foreign.C.Types
import           Data.Functor ((<$>))

import           Language.C.Context.Types

-- | Context useful to work with vanilla C.  Used by default.
baseCtx :: Context
baseCtx = Context{ctxCTypes = [], ctxCToHs = cToHs, ctxSuffixTypes = suffixTypes}
  where
    cToHs :: C.Type -> TH.Q (Maybe TH.Type)
    cToHs [C.cty| void |] = Just <$> [t| () |]
    cToHs [C.cty| int |] = Just <$> [t| CInt |]
    cToHs [C.cty| double |] = Just <$> [t| CDouble |]
    cToHs _ = error "TODO cTypeToHsType"

    suffixTypes :: String -> Maybe C.Type
    suffixTypes s = lookup s
      [ ("int", [C.cty| int |])
      , ("char", [C.cty| char |])
      , ("float", [C.cty| float |])
      , ("double", [C.cty| double |])
      ]

