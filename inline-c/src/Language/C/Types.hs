{-# LANGUAGE OverloadedStrings #-}
module Language.C.Types
  ( -- * Types
    P.Id
  , P.TypeQual(..)
  , TypeSpec(..)
  , Type(..)
  , P.ArraySize
  , Sign(..)
  , Declaration(..)

    -- * Parsing
  , parseDeclaration
  , parseAbstractDeclaration
  ) where

import qualified Language.C.Types.Parse as P
import           Control.Monad (when, unless)
import           Data.Maybe (fromMaybe)
import           Data.List (partition)
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Monoid ((<>))
import           Text.Trifecta

------------------------------------------------------------------------
-- Proper types

data TypeSpec
  = Void
  | Char (Maybe Sign)
  | Short Sign
  | Int Sign
  | Long Sign
  | LLong Sign
  | Float
  | Double
  | LDouble
  | TypeName P.Id
  | Struct P.Id
  | Enum P.Id
  deriving (Show, Eq)

data Type
  = TypeSpec TypeSpec
  | Ptr [P.TypeQual] Type
  | Array (Maybe P.ArraySize) Type
  | Proto Type [Declaration]
  deriving (Show, Eq)

data Sign
  = Signed
  | Unsigned
  deriving (Show, Eq)

-- | If the 'P.Id' is not present, the declaration is abstract.
data Declaration = Declaration (Maybe P.Id) [P.TypeQual] Type
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Conversion

data ConversionErr
  = MultipleDataTypes [P.TypeSpec]
  | IllegalSpecifiers String [P.TypeSpec]
  deriving (Show, Eq)

failConversion :: ConversionErr -> Either ConversionErr a
failConversion = Left

processParsedDecl :: P.Declaration -> Either ConversionErr Declaration
processParsedDecl (P.Declaration (P.DeclarationSpec quals pTySpecs) declarator) = do
  tySpec <- processParsedTySpecs pTySpecs
  (s, type_) <- processDeclarator (TypeSpec tySpec) declarator
  return $ Declaration s quals type_

processParsedTySpecs :: [P.TypeSpec] -> Either ConversionErr TypeSpec
processParsedTySpecs pTySpecs = do
  -- Split data type and specifiers
  let (dataTypes, specs) =
        partition (\x -> not (x `elem` [P.Signed, P.Unsigned, P.Long, P.Short])) pTySpecs
  let illegalSpecifiers s = failConversion $ IllegalSpecifiers s specs
  -- Find out sign, if present
  mbSign0 <- case filter (== P.Signed) specs of
    []  -> return Nothing
    [_] -> return $ Just Signed
    _:_ -> illegalSpecifiers "conflicting/duplicate sign information"
  mbSign <- case (mbSign0, filter (== P.Unsigned) specs) of
    (Nothing, []) -> return Nothing
    (Nothing, [_]) -> return $ Just Unsigned
    (Just b, []) -> return $ Just b
    _ -> illegalSpecifiers "conflicting/duplicate sign information"
  let sign = fromMaybe Signed mbSign
  -- Find out length
  let longs = length $ filter (== P.Long) specs
  let shorts = length $ filter (== P.Short) specs
  when (longs > 0 && shorts > 0) $ illegalSpecifiers "both long and short"
  -- Find out data type
  dataType <- case dataTypes of
    [x] -> return x
    [] | longs > 0 || shorts > 0 -> return P.Int
    _ -> failConversion $ MultipleDataTypes dataTypes
  -- Check if things are compatible with one another
  let checkNoSpecs =
        unless (null specs) $ illegalSpecifiers "expecting no specifiers"
  let checkNoLength =
        when (longs > 0 || shorts > 0) $ illegalSpecifiers "unexpected long/short"
  case dataType of
    P.TypeName s -> do
      checkNoSpecs
      return $ TypeName s
    P.Struct s -> do
      checkNoSpecs
      return $ Struct s
    P.Enum s -> do
      checkNoSpecs
      return $ Enum s
    P.Void -> do
      checkNoSpecs
      return Void
    P.Char -> do
      checkNoLength
      return $ Char mbSign
    P.Int | longs == 0 && shorts == 0 -> do
      return $ Int sign
    P.Int | longs == 1 -> do
      return $ Long sign
    P.Int | longs == 2 -> do
      return $ LLong sign
    P.Int | shorts == 1 -> do
      return $ Short sign
    P.Int -> do
      illegalSpecifiers "too many long/short"
    P.Float -> do
      checkNoLength
      return Float
    P.Double | longs == 1 -> do
      if longs == 1
        then return LDouble
        else do
          checkNoLength         -- TODO `long double` is acceptable
          return Double
    _ -> do
      error $ "processParsedDecl: impossible: " ++ show dataType

processDeclarator
  :: Type -> P.Declarator -> Either ConversionErr (Maybe P.Id, Type)
processDeclarator ty declarator0 = case declarator0 of
  P.DeclaratorRoot mbS -> return (mbS, ty)
  P.Ptr quals declarator -> processDeclarator (Ptr quals ty) declarator
  P.Array mbSize declarator -> processDeclarator (Array mbSize ty) declarator
  P.Proto declarator declarations -> do
    args <- mapM processParsedDecl declarations
    processDeclarator (Proto ty args) declarator

------------------------------------------------------------------------
-- Parsing

parseDeclaration :: Parser Declaration
parseDeclaration = do
  pDecl <- P.parseDeclaration
  case processParsedDecl pDecl of
    Left e -> fail $ PP.displayS (PP.renderPretty 0.8 80 (PP.pretty e)) ""
    Right decl -> return decl

parseAbstractDeclaration :: Parser Declaration
parseAbstractDeclaration = do
  pDecl <- P.parseAbstractDeclaration
  case processParsedDecl pDecl of
    Left e -> fail $ PP.displayS (PP.renderPretty 0.8 80 (PP.pretty e)) ""
    Right decl -> return decl

------------------------------------------------------------------------
-- Pretty printing

instance PP.Pretty ConversionErr where
  pretty e = case e of
    MultipleDataTypes types ->
      "Multiple data types in declaration:" <+> PP.prettyList types
    IllegalSpecifiers msg specs ->
      "Illegal specifiers," <+> PP.text msg <> ":" <> PP.prettyList specs

instance PP.Pretty TypeSpec where
  pretty tySpec = case tySpec of
    Void -> "void"
    Char Nothing -> "char"
    Char (Just Signed) -> "signed char"
    Char (Just Unsigned) -> "unsigned char"
    Short Signed -> "short"
    Short Unsigned -> "unsigned short"
    Int Signed -> "int"
    Int Unsigned -> "unsigned"
    Long Signed -> "long"
    Long Unsigned -> "unsigned long"
    LLong Signed -> "long long"
    LLong Unsigned -> "unsigned long long"
    Float -> "float"
    Double -> "double"
    LDouble -> "long double"
    TypeName s -> PP.text s
    Struct s -> "struct" <+> PP.text s
    Enum s -> "enum" <+> PP.text s

-- instance PP.Pretty Declaration where
--   pretty (Declaration s quals ty0) = PP.hsep quals <+> go ty0
--     where
--       go :: Type -> PP.Doc
--       go ty = case ty of
        
