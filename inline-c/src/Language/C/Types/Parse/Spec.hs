{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.C.Types.Parse.Spec (spec) where

import           Control.Applicative ((<*), (*>))
import           Control.Monad.Trans.Class (lift)
import qualified Data.Set as Set
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.QuickCheck as QC
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Language.C.Types.Parse
import qualified Language.C.Types as Types

spec :: Hspec.SpecWith ()
spec = do
  Hspec.it "parses everything which is pretty-printable (SmallCheck)" $ do
    SC.property $ \ty ->
      let ty' = assertParse (const False) parameter_declaration (prettyOneLine ty)
      in Types.untangleParameterDeclaration ty == Types.untangleParameterDeclaration ty'
  Hspec.it "parses everything which is pretty-printable (QuickCheck)" $ do
    QC.property $ \(ParameterDeclarationWithTypeNames typeNames ty) ->
      let ty' = assertParse (`Set.member` typeNames) parameter_declaration (prettyOneLine ty)
      in Types.untangleParameterDeclaration ty == Types.untangleParameterDeclaration ty'

------------------------------------------------------------------------
-- Utils

assertParse :: (Id -> Bool) -> (forall m. CParser m => m a) -> String -> a
assertParse isTypeName p s =
  case runCParser isTypeName "spec" s (lift spaces *> p <* lift eof) of
    Left err -> error $ "Parse error (assertParse): " ++ show err
    Right x -> x

prettyOneLine :: PP.Pretty a => a -> String
prettyOneLine x = PP.displayS (PP.renderCompact (PP.pretty x)) ""
