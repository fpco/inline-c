{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.C.Types.ParseSpec (spec) where

import           Control.Applicative
import           Control.Monad.Trans.Class (lift)
import           Data.Hashable (Hashable)
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QC
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Typeable (Typeable)
import qualified Data.HashSet as HashSet
import           Data.List (intercalate)
import           Data.String (fromString)
import           Data.Maybe (mapMaybe)

import           Language.C.Types.Parse
import qualified Language.C.Types as Types
import           Language.C.Inline.HaskellIdentifier

import Prelude -- Fix for 7.10 unused warnings.

spec :: Hspec.SpecWith ()
spec = do
  Hspec.it "parses everything which is pretty-printable (C)" $ do
#if MIN_VERSION_QuickCheck(2,9,0)
    QC.property $ QC.again $ do -- Work around <https://github.com/nick8325/quickcheck/issues/113>
#else
    QC.property $ do
#endif
      ParameterDeclarationWithTypeNames typeNames ty <-
        arbitraryParameterDeclarationWithTypeNames unCIdentifier
      return $ isGoodType ty QC.==>
        let ty' = assertParse (cCParserContext typeNames) parameter_declaration (prettyOneLine ty)
        in Types.untangleParameterDeclaration ty == Types.untangleParameterDeclaration ty'
  Hspec.it "parses everything which is pretty-printable (Haskell)" $ do
#if MIN_VERSION_QuickCheck(2,9,0)
    QC.property $ QC.again $ do -- Work around <https://github.com/nick8325/quickcheck/issues/113>
#else
    QC.property $ do
#endif
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

------------------------------------------------------------------------
-- Arbitrary

data OneOfSized a
  = Anyhow a
  | IfPositive a
  deriving (Typeable, Eq, Show)

-- | Precondition: there is at least one 'Anyhow' in the list.
oneOfSized :: [OneOfSized (QC.Gen a)] -> QC.Gen a
oneOfSized xs = QC.sized $ \n -> do
  let f (Anyhow a) = Just a
      f (IfPositive x) | n > 0 = Just x
      f (IfPositive _) = Nothing
  QC.oneof $ mapMaybe f xs

halveSize :: QC.Gen a -> QC.Gen a
halveSize m = QC.sized $ \n -> QC.resize (n `div` 2) m

instance QC.Arbitrary CIdentifier where
  arbitrary = do
    s <- ((:) <$> QC.elements cIdentStart <*> QC.listOf (QC.elements cIdentLetter))
    if HashSet.member s cReservedWords
      then QC.arbitrary
      else return $ fromString s

-- | Type used to generate an 'QC.Arbitrary' 'ParameterDeclaration' with
-- arbitrary allowed type names.
data ParameterDeclarationWithTypeNames i = ParameterDeclarationWithTypeNames
  { _pdwtnTypeNames :: HashSet.HashSet CIdentifier
  , _pdwtnParameterDeclaration :: (ParameterDeclaration i)
  } deriving (Typeable, Eq, Show)

data ArbitraryContext i = ArbitraryContext
  { acTypeNames :: TypeNames
  , acIdentToString :: i -> String
  }

arbitraryParameterDeclarationWithTypeNames
  :: (QC.Arbitrary i, Hashable i)
  => (i -> String)
  -> QC.Gen (ParameterDeclarationWithTypeNames i)
arbitraryParameterDeclarationWithTypeNames identToString = do
    names <- HashSet.fromList <$> QC.listOf QC.arbitrary
    let ctx = ArbitraryContext names identToString
    decl <- arbitraryParameterDeclarationFrom ctx
    return $ ParameterDeclarationWithTypeNames names decl

arbitraryDeclarationSpecifierFrom
  :: (QC.Arbitrary i, Hashable i) => ArbitraryContext i -> QC.Gen DeclarationSpecifier
arbitraryDeclarationSpecifierFrom typeNames = QC.oneof $
  [ StorageClassSpecifier <$> QC.arbitrary
  , TypeQualifier <$> QC.arbitrary
  , FunctionSpecifier <$> QC.arbitrary
  , TypeSpecifier <$> arbitraryTypeSpecifierFrom typeNames
  ]

instance QC.Arbitrary StorageClassSpecifier where
  arbitrary = QC.oneof
    [ return TYPEDEF
    , return EXTERN
    , return STATIC
    , return AUTO
    , return REGISTER
    ]

arbitraryTypeSpecifierFrom :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen TypeSpecifier
arbitraryTypeSpecifierFrom ctx = QC.oneof $
  [ return VOID
  , return CHAR
  , return SHORT
  , return INT
  , return LONG
  , return FLOAT
  , return DOUBLE
  , return SIGNED
  , return UNSIGNED
  , Struct <$> arbitraryCIdentifierFrom ctx
  , Enum <$> arbitraryCIdentifierFrom ctx
  ] ++ if HashSet.null (acTypeNames ctx) then []
       else [TypeName <$> QC.elements (HashSet.toList (acTypeNames ctx))]

instance QC.Arbitrary TypeQualifier where
  arbitrary = QC.oneof
    [ return CONST
    , return RESTRICT
    , return VOLATILE
    ]

instance QC.Arbitrary FunctionSpecifier where
  arbitrary = QC.oneof
    [ return INLINE
    ]

arbitraryDeclaratorFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen (Declarator i)
arbitraryDeclaratorFrom typeNames = halveSize $
  Declarator <$> QC.arbitrary <*> arbitraryDirectDeclaratorFrom typeNames

arbitraryCIdentifierFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen CIdentifier
arbitraryCIdentifierFrom ctx =
  arbitraryIdentifierFrom ctx{acIdentToString = unCIdentifier}

arbitraryIdentifierFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen i
arbitraryIdentifierFrom ctx = do
  id' <- QC.arbitrary
  if isTypeName (acTypeNames ctx) (acIdentToString ctx id')
    then arbitraryIdentifierFrom ctx
    else return id'

arbitraryDirectDeclaratorFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen (DirectDeclarator i)
arbitraryDirectDeclaratorFrom typeNames = halveSize $ oneOfSized $
  [ Anyhow $ DeclaratorRoot <$> arbitraryIdentifierFrom typeNames
  , IfPositive $ DeclaratorParens <$> arbitraryDeclaratorFrom typeNames
  , IfPositive $ ArrayOrProto
      <$> arbitraryDirectDeclaratorFrom typeNames
      <*> arbitraryArrayOrProtoFrom typeNames
  ]

arbitraryArrayOrProtoFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen (ArrayOrProto i)
arbitraryArrayOrProtoFrom typeNames = halveSize $ oneOfSized $
  [ Anyhow $ Array <$> arbitraryArrayTypeFrom typeNames
  , IfPositive $ Proto <$> QC.listOf (arbitraryParameterDeclarationFrom typeNames)
  ]

arbitraryArrayTypeFrom :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen (ArrayType i)
arbitraryArrayTypeFrom typeNames = QC.oneof
  [ return VariablySized
  , SizedByInteger . QC.getNonNegative <$> QC.arbitrary
  , SizedByIdentifier <$> arbitraryIdentifierFrom typeNames
  , return Unsized
  ]

instance QC.Arbitrary Pointer where
  arbitrary = Pointer <$> QC.arbitrary

arbitraryParameterDeclarationFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen (ParameterDeclaration i)
arbitraryParameterDeclarationFrom typeNames = halveSize $
  ParameterDeclaration
    <$> QC.listOf1 (arbitraryDeclarationSpecifierFrom typeNames)
    <*> QC.oneof
          [ IsDeclarator <$> arbitraryDeclaratorFrom typeNames
          , IsAbstractDeclarator <$> arbitraryAbstractDeclaratorFrom typeNames
          ]

arbitraryAbstractDeclaratorFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen (AbstractDeclarator i)
arbitraryAbstractDeclaratorFrom typeNames = halveSize $ do
  ptrs <- QC.arbitrary
  decl <- if null ptrs
    then Just <$> arbitraryDirectAbstractDeclaratorFrom typeNames
    else oneOfSized
      [ Anyhow $ return Nothing
      , IfPositive $ Just <$> arbitraryDirectAbstractDeclaratorFrom typeNames
      ]
  return $ AbstractDeclarator ptrs decl

arbitraryDirectAbstractDeclaratorFrom
  :: (Hashable i, QC.Arbitrary i) => ArbitraryContext i -> QC.Gen (DirectAbstractDeclarator i)
arbitraryDirectAbstractDeclaratorFrom typeNames = halveSize $ oneOfSized $
  [ Anyhow $ ArrayOrProtoHere <$> arbitraryArrayOrProtoFrom typeNames
  , IfPositive $ AbstractDeclaratorParens <$> arbitraryAbstractDeclaratorFrom typeNames
  , IfPositive $ ArrayOrProtoThere
      <$> arbitraryDirectAbstractDeclaratorFrom typeNames
      <*> arbitraryArrayOrProtoFrom typeNames
  ]

instance QC.Arbitrary HaskellIdentifier where
  arbitrary = do
    modIds <- QC.listOf arbitraryModId
    id_ <- QC.oneof [arbitraryConId, arbitraryVarId]
    if HashSet.member id_ haskellReservedWords
      then QC.arbitrary
      else return $ fromString $ intercalate "." $ modIds ++ [id_]
    where
      arbitraryModId = arbitraryConId

      arbitraryConId =
        ((:) <$> QC.elements large <*> QC.listOf (QC.elements (small ++ large ++ digit' ++ ['\''])))

      arbitraryVarId =
        ((:) <$> QC.elements small <*> QC.listOf (QC.elements (small ++ large ++ digit' ++ ['\''])))

      -- We currently do not generate unicode identifiers.
      large = ['A'..'Z']
      small = ['a'..'z'] ++ ['_']
      digit' = ['0'..'9']
