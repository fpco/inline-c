{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.C.Types
  ( -- * Types
    P.Id(..)
  , P.StorageClassSpecifier(..)
  , P.TypeQualifier(..)
  , P.FunctionSpecifier(..)
  , P.ArrayType(..)
  , Specifiers(..)
  , Type(..)
  , Sign(..)
  , ParameterDeclaration(..)

    -- * Parsing
  , P.CParser
  , parseParameterDeclaration
  , parseParameterList
  , P.parseIdentifier

    -- * To english
  , readParameterDeclaration
  , readType

    -- * Manual conversion
  , convertParsedParameterDeclaration
  -- , distillParameterDeclaration
  ) where

import           Control.Lens (_1, _2, _3, _4, (%=))
import           Control.Monad (when, unless, forM_)
import           Data.Functor ((<$>))
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import           Control.Monad.State (execState)
import           Text.PrettyPrint.ANSI.Leijen ((</>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Monoid ((<>))

import qualified Language.C.Types.Parse as P

------------------------------------------------------------------------
-- Proper types

data TypeSpecifier
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

data Specifiers = Specifiers
  { storageClassSpecifiers :: [P.StorageClassSpecifier]
  , typeQualifiers :: [P.TypeQualifier]
  , functionSpecifiers :: [P.FunctionSpecifier]
  } deriving (Show, Eq)

data Type
  = TypeSpecifier Specifiers TypeSpecifier
  | Ptr [P.TypeQualifier] Type
  | Array P.ArrayType Type
  | Proto Type [ParameterDeclaration]
  deriving (Show, Eq)

data Sign
  = Signed
  | Unsigned
  deriving (Show, Eq)

data ParameterDeclaration = ParameterDeclaration
  { parameterDeclarationId :: Maybe P.Id
  , parameterDeclarationType :: Type
  } deriving (Show, Eq)

------------------------------------------------------------------------
-- Conversion

data ConversionErr
  = MultipleDataTypes [P.TypeSpecifier]
  | IllegalSpecifiers String [P.TypeSpecifier]
  deriving (Show, Eq)

failConversion :: ConversionErr -> Either ConversionErr a
failConversion = Left

convertParsedParameterDeclaration
  :: P.ParameterDeclaration -> Either ConversionErr ParameterDeclaration
convertParsedParameterDeclaration P.ParameterDeclaration{..} = do
  (specs, tySpec) <- convertParsedDeclarationSpecifiers parameterDeclarationSpecifiers
  let baseTy = TypeSpecifier specs tySpec
  (mbS, ty) <- case parameterDeclarationDeclarator of
    Left decltor -> do
      (s, ty) <- convertParsedDeclarator baseTy decltor
      return (Just s, ty)
    Right decltor -> (Nothing, ) <$> convertParsedAbstractDeclarator baseTy decltor
  return $ ParameterDeclaration mbS ty

convertParsedDeclarationSpecifiers
  :: [P.DeclarationSpecifier] -> Either ConversionErr (Specifiers, TypeSpecifier)
convertParsedDeclarationSpecifiers declSpecs = do
  let (pStorage, pTySpecs, pTyQuals, pFunSpecs) = flip execState ([], [], [], []) $ do
        forM_ (reverse declSpecs) $ \declSpec -> case declSpec of
          P.StorageClassSpecifier x -> _1 %= (x :)
          P.TypeSpecifier x -> _2 %= (x :)
          P.TypeQualifier x -> _3 %= (x :)
          P.FunctionSpecifier x -> _4 %= (x :)
  -- Split data type and specifiers
  let (dataTypes, specs) =
        partition (\x -> not (x `elem` [P.SIGNED, P.UNSIGNED, P.LONG, P.SHORT])) pTySpecs
  let illegalSpecifiers s = failConversion $ IllegalSpecifiers s specs
  -- Find out sign, if present
  mbSign0 <- case filter (== P.SIGNED) specs of
    []  -> return Nothing
    [_] -> return $ Just Signed
    _:_ -> illegalSpecifiers "conflicting/duplicate sign information"
  mbSign <- case (mbSign0, filter (== P.UNSIGNED) specs) of
    (Nothing, []) -> return Nothing
    (Nothing, [_]) -> return $ Just Unsigned
    (Just b, []) -> return $ Just b
    _ -> illegalSpecifiers "conflicting/duplicate sign information"
  let sign = fromMaybe Signed mbSign
  -- Find out length
  let longs = length $ filter (== P.LONG) specs
  let shorts = length $ filter (== P.SHORT) specs
  when (longs > 0 && shorts > 0) $ illegalSpecifiers "both long and short"
  -- Find out data type
  dataType <- case dataTypes of
    [x] -> return x
    [] | longs > 0 || shorts > 0 -> return P.INT
    _ -> failConversion $ MultipleDataTypes dataTypes
  -- Check if things are compatible with one another
  let checkNoSpecs =
        unless (null specs) $ illegalSpecifiers "expecting no specifiers"
  let checkNoLength =
        when (longs > 0 || shorts > 0) $ illegalSpecifiers "unexpected long/short"
  tySpec <- case dataType of
    P.TypeName s -> do
      checkNoSpecs
      return $ TypeName s
    P.Struct s -> do
      checkNoSpecs
      return $ Struct s
    P.Enum s -> do
      checkNoSpecs
      return $ Enum s
    P.VOID -> do
      checkNoSpecs
      return Void
    P.CHAR -> do
      checkNoLength
      return $ Char mbSign
    P.INT | longs == 0 && shorts == 0 -> do
      return $ Int sign
    P.INT | longs == 1 -> do
      return $ Long sign
    P.INT | longs == 2 -> do
      return $ LLong sign
    P.INT | shorts == 1 -> do
      return $ Short sign
    P.INT -> do
      illegalSpecifiers "too many long/short"
    P.FLOAT -> do
      checkNoLength
      return Float
    P.DOUBLE -> do
      if longs == 1
        then return LDouble
        else do
          checkNoLength
          return Double
    _ -> do
      error $ "convertParsedDeclarationSpecifiers impossible: " ++ show dataType
  return (Specifiers pStorage pTyQuals pFunSpecs, tySpec)

convertParsedDeclarator
  :: Type -> P.Declarator -> Either ConversionErr (P.Id, Type)
convertParsedDeclarator ty0 (P.Declarator ptrs0 directDecltor) = go ty0 ptrs0
  where
    go :: Type -> [P.Pointer] -> Either ConversionErr (P.Id, Type)
    go ty [] = goDirect ty directDecltor
    go ty (P.Pointer quals : ptrs) = go (Ptr quals ty) ptrs

    goDirect :: Type -> P.DirectDeclarator -> Either ConversionErr (P.Id, Type)
    goDirect ty direct0 = case direct0 of
      P.DeclaratorRoot s -> return (s, ty)
      P.ArrayOrProto direct (P.Array arrayType) ->
        goDirect (Array arrayType ty) direct
      P.ArrayOrProto direct (P.Proto params) -> do
        params' <- mapM convertParsedParameterDeclaration params
        goDirect (Proto ty params') direct
      P.DeclaratorParens decltor ->
        convertParsedDeclarator ty decltor

convertParsedAbstractDeclarator
  :: Type -> P.AbstractDeclarator -> Either ConversionErr Type
convertParsedAbstractDeclarator ty0 (P.AbstractDeclarator ptrs0 mbDirectDecltor) =
  go ty0 ptrs0
  where
    go :: Type -> [P.Pointer] -> Either ConversionErr Type
    go ty [] = case mbDirectDecltor of
      Nothing -> return ty
      Just directDecltor -> goDirect ty directDecltor
    go ty (P.Pointer quals : ptrs) = go (Ptr quals ty) ptrs

    goDirect :: Type -> P.DirectAbstractDeclarator -> Either ConversionErr Type
    goDirect ty direct0 = case direct0 of
      P.ArrayOrProtoThere direct (P.Array arrayType) ->
        goDirect (Array arrayType ty) direct
      P.ArrayOrProtoThere direct (P.Proto params) -> do
        params' <- mapM convertParsedParameterDeclaration params
        goDirect (Proto ty params') direct
      P.ArrayOrProtoHere (P.Array arrayType) ->
        return $ Array arrayType ty
      P.ArrayOrProtoHere (P.Proto params) -> do
        params' <- mapM convertParsedParameterDeclaration params
        return $ Proto ty params'
      P.ParensAbstractDeclarator decltor ->
        convertParsedAbstractDeclarator ty decltor

------------------------------------------------------------------------
-- Distilling

-- distillParameterDeclaration :: ParameterDeclaration -> ParameterDeclaration
-- distillParameterDeclaration (ParameterDeclaration mbId ty00) =
--     uncurry ParameterDeclaration $ case mbId of
--       Nothing -> over _2 Right $ goAbstract ty00
--       Just id' -> over _2 Left $ goConcrete id' ty00
--   where
--     goAbstract
--        :: Type -> ([P.DeclarationSpecifier], P.AbstractDeclarator)
--     goAbstract ty0 = case ty0 of
      

------------------------------------------------------------------------
-- To english

readParameterDeclaration :: ParameterDeclaration -> PP.Doc
readParameterDeclaration (ParameterDeclaration mbId ty) =
  let idDoc = case mbId of
        Nothing -> ""
        Just id' -> PP.pretty id' <+> "is a "
  in idDoc <> readType ty

readType :: Type -> PP.Doc
readType ty0 = case ty0 of
  TypeSpecifier specs tySpec -> engSpecs specs <> PP.pretty tySpec
  Ptr quals ty -> engQuals quals <> "ptr to" <+> readType ty
  Array arrTy ty -> engArrTy arrTy <> "of" <+> readType ty
  Proto retTy params ->
     "function from" <+> engParams params <> "returning" <+> readType retTy
  where
    engSpecs (Specifiers [] [] []) = ""
    engSpecs (Specifiers x y z) =
      let xs = map P.StorageClassSpecifier x ++ map P.TypeQualifier y ++
               map P.FunctionSpecifier z
      in PP.hsep (map PP.pretty xs) <> " "

    engQuals = PP.hsep . map PP.pretty

    engArrTy arrTy = case arrTy of
      P.VariablySized -> "variably sized array "
      P.SizedByInteger n -> "array of size" <+> PP.text (show n) <> " "
      P.SizedByIdentifier s -> "array of size" <+> PP.pretty s <> " "
      P.Unsized -> "array "

    engParams [] = ""
    engParams params0 = "(" <> go params0 <> ") "
      where
        go xs = case xs of
          [] -> ""
          [x] -> readParameterDeclaration x
          (x:xs') -> readParameterDeclaration x <> "," <+> go xs'

------------------------------------------------------------------------
-- Convenient parsing

convertParsedParameterDeclaration'
  :: P.CParser m => P.ParameterDeclaration -> m ParameterDeclaration
convertParsedParameterDeclaration' pDecl =
  case convertParsedParameterDeclaration pDecl of
    Left err -> fail $ pretty80 $
      "Error while parsing declaration:" </> PP.pretty err </> PP.pretty pDecl
    Right x -> return x

parseParameterDeclaration :: P.CParser m => m ParameterDeclaration
parseParameterDeclaration =
  convertParsedParameterDeclaration' =<< P.parseParameterDeclaration

parseParameterList :: P.CParser m => m [ParameterDeclaration]
parseParameterList =
  mapM convertParsedParameterDeclaration' =<< P.parseParameterList

------------------------------------------------------------------------
-- Pretty

instance PP.Pretty TypeSpecifier where
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
    TypeName s -> PP.pretty s
    Struct s -> "struct" <+> PP.pretty s
    Enum s -> "enum" <+> PP.pretty s

instance PP.Pretty ConversionErr where
  pretty err = case err of
    MultipleDataTypes specs ->
      "Multiple data types in" </> PP.prettyList specs
    IllegalSpecifiers s specs ->
      "Illegal specifiers, " <+> PP.text s <+> ", in" </> PP.prettyList specs

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Doc -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 x) ""
