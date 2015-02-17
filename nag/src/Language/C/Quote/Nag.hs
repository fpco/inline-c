module Language.C.Quote.Nag
  ( cty
  , cunit

  , nagTypes
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.C as C
import qualified Language.C.Quote.C as C
import qualified Language.C.Quote.Base as C

nagTypes :: [String]
nagTypes = ["Integer", "Complex", "Nag_Boolean", "NagError"]

cty :: TH.QuasiQuoter
cty = C.quasiquote [C.Antiquotation] nagTypes C.parseType

cunit :: TH.QuasiQuoter
cunit = C.quasiquote [C.Antiquotation] nagTypes C.parseUnit
