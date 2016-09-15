{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Language.C.Types.ParseSpec (spec) where

import           Control.Applicative
import           Control.Monad.Trans.Class (lift)
import           Data.Hashable (Hashable)
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QC
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Language.C.Types.Parse
import qualified Language.C.Types as Types
import           Language.C.Inline.HaskellIdentifier

import Prelude -- Fix for 7.10 unused warnings.

spec :: Hspec.SpecWith ()
spec = do
  Hspec.it "parses everything which is pretty-printable (C)" $ do
    QC.property $ QC.again $ do -- Work around <https://github.com/nick8325/quickcheck/issues/113>
      ParameterDeclarationWithTypeNames typeNames ty <-
        arbitraryParameterDeclarationWithTypeNames unCIdentifier
      return $ isGoodType ty QC.==>
        let ty' = assertParse (cCParserContext typeNames) parameter_declaration (prettyOneLine ty)
        in Types.untangleParameterDeclaration ty == Types.untangleParameterDeclaration ty'
  Hspec.it "parses everything which is pretty-printable (Haskell)" $ do
    QC.property $ QC.again $ do -- Work around <https://github.com/nick8325/quickcheck/issues/113>
      ParameterDeclarationWithTypeNames typeNames ty <-
        arbitraryParameterDeclarationWithTypeNames unHaskellIdentifier
      return $ isGoodType ty QC.==>
        let ty' = assertParse (haskellCParserContext typeNames) parameter_declaration (prettyOneLine ty)
        in Types.untangleParameterDeclaration ty == Types.untangleParameterDeclaration ty'

------------------------------------------------------------------------
-- Utils

assertParse
  :: (Hashable i)
  => CParserContext i -> (forall m. CParser i m => m a) -> String -> a
assertParse ctx p s =
  case runCParser ctx "spec" s (lift spaces *> p <* lift eof) of
    Left err -> error $ "Parse error (assertParse): " ++ show err
    Right x -> x

prettyOneLine :: PP.Pretty a => a -> String
prettyOneLine x = PP.displayS (PP.renderCompact (PP.pretty x)) ""

isGoodType :: ParameterDeclaration i -> Bool
isGoodType ty = case Types.untangleParameterDeclaration ty of
  Left _ -> False
  Right _ -> True
