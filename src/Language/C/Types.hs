{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Views of C datatypes. While "Language.C.Types.Parse" defines datatypes for
-- representing the concrete syntax tree of C types, this module provides
-- friendlier views of C types, by turning them into a data type matching more
-- closely how we read and think about types, both in Haskell and in C. To
-- appreciate the difference, look at the difference between
-- 'P.ParameterDeclaration' and 'ParameterDeclaration'.
--
-- As a bonus, routines are provided for describing types in natural language
-- (English) -- see 'describeParameterDeclaration' and 'describeType'.

module Language.C.Types
  ( -- * Types
    P.CIdentifier
  , P.unCIdentifier
  , P.cIdentifierFromString
  , P.StorageClassSpecifier(..)
  , P.TypeQualifier(..)
  , P.FunctionSpecifier(..)
  , P.ArrayType(..)
  , Specifiers(..)
  , Type(..)
  , TypeSpecifier(..)
  , Sign(..)
  , ParameterDeclaration(..)

    -- * Parsing
  , P.TypeNames
  , P.CParser
  , P.CParserContext
  , P.cCParserContext
  , P.runCParser
  , P.quickCParser
  , P.quickCParser_
  , parseParameterDeclaration
  , parseParameterList
  , parseIdentifier
  , parseType

    -- * Convert to and from high-level views
  , UntangleErr(..)
  , untangleParameterDeclaration
  , tangleParameterDeclaration

    -- * To english
  , describeParameterDeclaration
  , describeType
  ) where

import           Control.Arrow (second)
import           Control.Monad (when, unless, forM_)
import           Control.Monad.State (execState, modify)
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
import           Text.PrettyPrint.ANSI.Leijen ((</>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable (Foldable)
import           Data.Functor ((<$>))
import           Data.Monoid (Monoid(..))
import           Data.Traversable (Traversable)
#endif

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
  | TypeName P.CIdentifier
  | Struct P.CIdentifier
  | Enum P.CIdentifier
  deriving (Typeable, Show, Eq, Ord)

data Specifiers = Specifiers
  { storageClassSpecifiers :: [P.StorageClassSpecifier]
  , typeQualifiers :: [P.TypeQualifier]
  , functionSpecifiers :: [P.FunctionSpecifier]
  } deriving (Typeable, Show, Eq)

instance Monoid Specifiers where
  mempty = Specifiers [] [] []

  mappend (Specifiers x1 y1 z1) (Specifiers x2 y2 z2) =
    Specifiers (x1 ++ x2) (y1 ++ y2) (z1 ++ z2)

data Type i
  = TypeSpecifier Specifiers TypeSpecifier
  | Ptr [P.TypeQualifier] (Type i)
  | Array (P.ArrayType i) (Type i)
  | Proto (Type i) [ParameterDeclaration i]
  deriving (Typeable, Show, Eq, Functor, Foldable, Traversable)

data Sign
  = Signed
  | Unsigned
  deriving (Typeable, Show, Eq, Ord)

data ParameterDeclaration i = ParameterDeclaration
  { parameterDeclarationId :: Maybe i
  , parameterDeclarationType :: (Type i)
  } deriving (Typeable, Show, Eq, Functor, Foldable, Traversable)

------------------------------------------------------------------------
-- Conversion

data UntangleErr
  = MultipleDataTypes [P.DeclarationSpecifier]
  | NoDataTypes [P.DeclarationSpecifier]
  | IllegalSpecifiers String [P.TypeSpecifier]
  deriving (Typeable, Show, Eq)

failConversion :: UntangleErr -> Either UntangleErr a
failConversion = Left

untangleParameterDeclaration
  :: P.ParameterDeclaration i -> Either UntangleErr (ParameterDeclaration i)
untangleParameterDeclaration P.ParameterDeclaration{..} = do
  (specs, tySpec) <- untangleDeclarationSpecifiers parameterDeclarationSpecifiers
  let baseTy = TypeSpecifier specs tySpec
  (mbS, ty) <- case parameterDeclarationDeclarator of
    P.IsDeclarator decltor -> do
      (s, ty) <- untangleDeclarator baseTy decltor
      return (Just s, ty)
    P.IsAbstractDeclarator decltor ->
      (Nothing, ) <$> untangleAbstractDeclarator baseTy decltor
  return $ ParameterDeclaration mbS ty

untangleDeclarationSpecifiers
  :: [P.DeclarationSpecifier] -> Either UntangleErr (Specifiers, TypeSpecifier)
untangleDeclarationSpecifiers declSpecs = do
  let (pStorage, pTySpecs, pTyQuals, pFunSpecs) = flip execState ([], [], [], []) $ do
        forM_ (reverse declSpecs) $ \declSpec -> case declSpec of
          P.StorageClassSpecifier x -> modify $ \(a, b, c, d) -> (x:a, b, c, d)
          P.TypeSpecifier x -> modify $ \(a, b, c, d) -> (a, x:b, c, d)
          P.TypeQualifier x -> modify $ \(a, b, c, d) -> (a, b, x:c, d)
          P.FunctionSpecifier x -> modify $ \(a, b, c, d) -> (a, b, c, x:d)
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
    [] -> failConversion $ NoDataTypes declSpecs
    _:_ -> failConversion $ MultipleDataTypes declSpecs
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
      error $ "untangleDeclarationSpecifiers impossible: " ++ show dataType
  return (Specifiers pStorage pTyQuals pFunSpecs, tySpec)

untangleDeclarator
  :: forall i. Type i -> P.Declarator i -> Either UntangleErr (i, Type i)
untangleDeclarator ty0 (P.Declarator ptrs0 directDecltor) = go ty0 ptrs0
  where
    go :: Type i -> [P.Pointer] -> Either UntangleErr (i, Type i)
    go ty [] = goDirect ty directDecltor
    go ty (P.Pointer quals : ptrs) = go (Ptr quals ty) ptrs

    goDirect :: Type i -> P.DirectDeclarator i -> Either UntangleErr (i, Type i)
    goDirect ty direct0 = case direct0 of
      P.DeclaratorRoot s -> return (s, ty)
      P.ArrayOrProto direct (P.Array arrayType) ->
        goDirect (Array arrayType ty) direct
      P.ArrayOrProto direct (P.Proto params) -> do
        params' <- mapM untangleParameterDeclaration params
        goDirect (Proto ty params') direct
      P.DeclaratorParens decltor ->
        untangleDeclarator ty decltor

untangleAbstractDeclarator
  :: forall i. Type i -> P.AbstractDeclarator i -> Either UntangleErr (Type i)
untangleAbstractDeclarator ty0 (P.AbstractDeclarator ptrs0 mbDirectDecltor) =
  go ty0 ptrs0
  where
    go :: Type i -> [P.Pointer] -> Either UntangleErr (Type i)
    go ty [] = case mbDirectDecltor of
      Nothing -> return ty
      Just directDecltor -> goDirect ty directDecltor
    go ty (P.Pointer quals : ptrs) = go (Ptr quals ty) ptrs

    goDirect :: Type i -> P.DirectAbstractDeclarator i -> Either UntangleErr (Type i)
    goDirect ty direct0 = case direct0 of
      P.ArrayOrProtoThere direct (P.Array arrayType) ->
        goDirect (Array arrayType ty) direct
      P.ArrayOrProtoThere direct (P.Proto params) -> do
        params' <- mapM untangleParameterDeclaration params
        goDirect (Proto ty params') direct
      P.ArrayOrProtoHere (P.Array arrayType) ->
        return $ Array arrayType ty
      P.ArrayOrProtoHere (P.Proto params) -> do
        params' <- mapM untangleParameterDeclaration params
        return $ Proto ty params'
      P.AbstractDeclaratorParens decltor ->
        untangleAbstractDeclarator ty decltor

------------------------------------------------------------------------
-- Tangling

tangleParameterDeclaration
  :: forall i. ParameterDeclaration i -> P.ParameterDeclaration i
tangleParameterDeclaration (ParameterDeclaration mbId ty00) =
    uncurry P.ParameterDeclaration $ case mbId of
      Nothing -> second P.IsAbstractDeclarator $ goAbstractDirect ty00 Nothing
      Just id' -> second P.IsDeclarator $ goConcreteDirect ty00 $ P.DeclaratorRoot id'
  where
    goAbstractDirect
      :: Type i -> Maybe (P.DirectAbstractDeclarator i)
      -> ([P.DeclarationSpecifier], P.AbstractDeclarator i)
    goAbstractDirect ty0 mbDirect = case ty0 of
      TypeSpecifier specifiers tySpec ->
        let declSpecs = tangleTypeSpecifier specifiers tySpec
        in (declSpecs, P.AbstractDeclarator [] mbDirect)
      Ptr tyQuals ty ->
        goAbstract ty [P.Pointer tyQuals] mbDirect
      Array arrType ty ->
        let arr = P.Array arrType
        in case mbDirect of
          Nothing ->
            goAbstractDirect ty $ Just $ P.ArrayOrProtoHere arr
          Just decltor ->
            goAbstractDirect ty $ Just $ P.ArrayOrProtoThere decltor arr
      Proto ty params ->
        let proto = P.Proto $ map tangleParameterDeclaration params
        in case mbDirect of
          Nothing ->
            goAbstractDirect ty $ Just $ P.ArrayOrProtoHere proto
          Just decltor ->
            goAbstractDirect ty $ Just $ P.ArrayOrProtoThere decltor proto

    goAbstract
      :: Type i -> [P.Pointer] -> Maybe (P.DirectAbstractDeclarator i)
      -> ([P.DeclarationSpecifier], P.AbstractDeclarator i)
    goAbstract ty0 ptrs mbDirect = case ty0 of
      TypeSpecifier specifiers tySpec ->
        let declSpecs = tangleTypeSpecifier specifiers tySpec
        in (declSpecs, P.AbstractDeclarator ptrs mbDirect)
      Ptr tyQuals ty ->
        goAbstract ty (P.Pointer tyQuals : ptrs) mbDirect
      Array{} ->
        goAbstractDirect ty0 $ Just $ P.AbstractDeclaratorParens $
          P.AbstractDeclarator ptrs mbDirect
      Proto{} ->
        goAbstractDirect ty0 $ Just $ P.AbstractDeclaratorParens $
          P.AbstractDeclarator ptrs mbDirect

    goConcreteDirect
      :: Type i -> P.DirectDeclarator i
      -> ([P.DeclarationSpecifier], P.Declarator i)
    goConcreteDirect ty0 direct = case ty0 of
      TypeSpecifier specifiers tySpec ->
        let declSpecs = tangleTypeSpecifier specifiers tySpec
        in (declSpecs, P.Declarator [] direct)
      Ptr tyQuals ty ->
        goConcrete ty [P.Pointer tyQuals] direct
      Array arrType ty ->
        goConcreteDirect ty $ P.ArrayOrProto direct $ P.Array arrType
      Proto ty params ->
        goConcreteDirect ty $ P.ArrayOrProto direct $
          P.Proto $ map tangleParameterDeclaration params

    goConcrete
      :: Type i -> [P.Pointer] -> P.DirectDeclarator i
      -> ([P.DeclarationSpecifier], P.Declarator i)
    goConcrete ty0 ptrs direct = case ty0 of
      TypeSpecifier specifiers tySpec ->
        let declSpecs = tangleTypeSpecifier specifiers tySpec
        in (declSpecs, P.Declarator ptrs direct)
      Ptr tyQuals ty ->
        goConcrete ty (P.Pointer tyQuals : ptrs) direct
      Array{} ->
        goConcreteDirect ty0 $ P.DeclaratorParens $ P.Declarator ptrs direct
      Proto{} ->
        goConcreteDirect ty0 $ P.DeclaratorParens $ P.Declarator ptrs direct

tangleTypeSpecifier :: Specifiers -> TypeSpecifier -> [P.DeclarationSpecifier]
tangleTypeSpecifier (Specifiers storages tyQuals funSpecs) tySpec =
  let pTySpecs = case tySpec of
        Void -> [P.VOID]
        Char Nothing -> [P.CHAR]
        Char (Just Signed) -> [P.SIGNED, P.CHAR]
        Char (Just Unsigned) -> [P.UNSIGNED, P.CHAR]
        Short Signed -> [P.SHORT]
        Short Unsigned -> [P.UNSIGNED, P.SHORT]
        Int Signed -> [P.INT]
        Int Unsigned -> [P.UNSIGNED]
        Long Signed -> [P.LONG]
        Long Unsigned -> [P.UNSIGNED, P.LONG]
        LLong Signed -> [P.LONG, P.LONG]
        LLong Unsigned -> [P.UNSIGNED, P.LONG, P.LONG]
        Float -> [P.FLOAT]
        Double -> [P.DOUBLE]
        LDouble -> [P.LONG, P.DOUBLE]
        TypeName s -> [P.TypeName s]
        Struct s -> [P.Struct s]
        Enum s -> [P.Enum s]
  in map P.StorageClassSpecifier storages ++
     map P.TypeQualifier tyQuals ++
     map P.FunctionSpecifier funSpecs ++
     map P.TypeSpecifier pTySpecs

------------------------------------------------------------------------
-- To english

describeParameterDeclaration :: PP.Pretty i => ParameterDeclaration i -> PP.Doc
describeParameterDeclaration (ParameterDeclaration mbId ty) =
  let idDoc = case mbId of
        Nothing -> ""
        Just id' -> PP.pretty id' <+> "is a "
  in idDoc <> describeType ty

describeType :: PP.Pretty i => Type i -> PP.Doc
describeType ty0 = case ty0 of
  TypeSpecifier specs tySpec -> engSpecs specs <> PP.pretty tySpec
  Ptr quals ty -> engQuals quals <> "ptr to" <+> describeType ty
  Array arrTy ty -> engArrTy arrTy <> "of" <+> describeType ty
  Proto retTy params ->
     "function from" <+> engParams params <> "returning" <+> describeType retTy
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
          [x] -> describeParameterDeclaration x
          (x:xs') -> describeParameterDeclaration x <> "," <+> go xs'

------------------------------------------------------------------------
-- Convenient parsing

untangleParameterDeclaration'
  :: (P.CParser i m, PP.Pretty i)
  => P.ParameterDeclaration i -> m (ParameterDeclaration i)
untangleParameterDeclaration' pDecl =
  case untangleParameterDeclaration pDecl of
    Left err -> fail $ pretty80 $
      "Error while parsing declaration:" </> PP.pretty err </> PP.pretty pDecl
    Right x -> return x

parseParameterDeclaration
  :: (P.CParser i m, PP.Pretty i) => m (ParameterDeclaration i)
parseParameterDeclaration =
  untangleParameterDeclaration' =<< P.parameter_declaration

parseParameterList
  :: (P.CParser i m, PP.Pretty i)
  => m [ParameterDeclaration i]
parseParameterList =
  mapM untangleParameterDeclaration' =<< P.parameter_list

parseIdentifier :: P.CParser i m => m i
parseIdentifier = P.identifier_no_lex

parseType :: (P.CParser i m, PP.Pretty i) => m (Type i)
parseType = parameterDeclarationType <$> parseParameterDeclaration

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

instance PP.Pretty UntangleErr where
  pretty err = case err of
    MultipleDataTypes specs ->
      "Multiple data types in" </> PP.prettyList specs
    IllegalSpecifiers s specs ->
      "Illegal specifiers, " <+> PP.text s <+> ", in" </> PP.prettyList specs
    NoDataTypes specs ->
      "No data types in " </> PP.prettyList specs

instance PP.Pretty i => PP.Pretty (ParameterDeclaration i) where
  pretty = PP.pretty . tangleParameterDeclaration

instance PP.Pretty i => PP.Pretty (Type i) where
  pretty ty =
    PP.pretty $ tangleParameterDeclaration $ ParameterDeclaration Nothing ty

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Doc -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 x) ""
