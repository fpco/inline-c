{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | While "Language.C.Types.Parse" provides access to routines that
-- parse C declarations and present them as they are, this module gives
-- a much friendlier view to C types, by turning them in a data type
-- matching more closely to how we read and think about types, both in
-- Haskell and in C.  To appreciate the difference, look at the
-- difference between 'P.ParameterDeclaration' and
-- 'ParameterDeclaration'.
--
-- Routines are provided to convert back and forth between the parsed
-- output and the friendly output -- see 'untangleParameterDeclaration'
-- and 'tangleParameterDeclaration'.  We also provide convenient parsers
-- returning directly friendly output -- see 'parseParameterDeclaration'
-- and 'parseParameterList'.
--
-- As a bonus, routines are provided "reading" the types in english --
-- see 'readParameterDeclaration' and 'readType'.

module Language.C.Types
  ( -- * Types
    P.Id(..)
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
  , P.IsTypeName
  , P.CParser
  , P.runCParser
  , P.quickCParser
  , P.quickCParser_
  , parseParameterDeclaration
  , parseParameterList
  , parseIdentifier
  , parseType

    -- * Back and forth
  , UntangleErr(..)
  , untangleParameterDeclaration
  , tangleParameterDeclaration

    -- * To english
  , readParameterDeclaration
  , readType
  ) where

import           Control.Arrow (second)
import           Control.Monad (when, unless, forM_)
import           Control.Monad.State (execState, modify)
import           Data.Functor ((<$>))
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>), Monoid(..))
import           Data.Typeable (Typeable)
import           Text.PrettyPrint.ANSI.Leijen ((</>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

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

data Type
  = TypeSpecifier Specifiers TypeSpecifier
  | Ptr [P.TypeQualifier] Type
  | Array P.ArrayType Type
  | Proto Type [ParameterDeclaration]
  deriving (Typeable, Show, Eq)

data Sign
  = Signed
  | Unsigned
  deriving (Typeable, Show, Eq, Ord)

data ParameterDeclaration = ParameterDeclaration
  { parameterDeclarationId :: Maybe P.Id
  , parameterDeclarationType :: Type
  } deriving (Typeable, Show, Eq)

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
  :: P.ParameterDeclaration -> Either UntangleErr ParameterDeclaration
untangleParameterDeclaration P.ParameterDeclaration{..} = do
  (specs, tySpec) <- untangleDeclarationSpecifiers parameterDeclarationSpecifiers
  let baseTy = TypeSpecifier specs tySpec
  (mbS, ty) <- case parameterDeclarationDeclarator of
    Left decltor -> do
      (s, ty) <- untangleDeclarator baseTy decltor
      return (Just s, ty)
    Right decltor -> (Nothing, ) <$> untangleAbstractDeclarator baseTy decltor
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
  :: Type -> P.Declarator -> Either UntangleErr (P.Id, Type)
untangleDeclarator ty0 (P.Declarator ptrs0 directDecltor) = go ty0 ptrs0
  where
    go :: Type -> [P.Pointer] -> Either UntangleErr (P.Id, Type)
    go ty [] = goDirect ty directDecltor
    go ty (P.Pointer quals : ptrs) = go (Ptr quals ty) ptrs

    goDirect :: Type -> P.DirectDeclarator -> Either UntangleErr (P.Id, Type)
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
  :: Type -> P.AbstractDeclarator -> Either UntangleErr Type
untangleAbstractDeclarator ty0 (P.AbstractDeclarator ptrs0 mbDirectDecltor) =
  go ty0 ptrs0
  where
    go :: Type -> [P.Pointer] -> Either UntangleErr Type
    go ty [] = case mbDirectDecltor of
      Nothing -> return ty
      Just directDecltor -> goDirect ty directDecltor
    go ty (P.Pointer quals : ptrs) = go (Ptr quals ty) ptrs

    goDirect :: Type -> P.DirectAbstractDeclarator -> Either UntangleErr Type
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
-- Tangleing

tangleParameterDeclaration :: ParameterDeclaration -> P.ParameterDeclaration
tangleParameterDeclaration (ParameterDeclaration mbId ty00) =
    uncurry P.ParameterDeclaration $ case mbId of
      Nothing -> second Right $ goAbstractDirect ty00 Nothing
      Just id' -> second Left $ goConcreteDirect ty00 $ P.DeclaratorRoot id'
  where
    goAbstractDirect
      :: Type -> Maybe P.DirectAbstractDeclarator
      -> ([P.DeclarationSpecifier], P.AbstractDeclarator)
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
      :: Type -> [P.Pointer] -> Maybe P.DirectAbstractDeclarator
      -> ([P.DeclarationSpecifier], P.AbstractDeclarator)
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
      :: Type -> P.DirectDeclarator
      -> ([P.DeclarationSpecifier], P.Declarator)
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
      :: Type -> [P.Pointer] -> P.DirectDeclarator
      -> ([P.DeclarationSpecifier], P.Declarator)
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

untangleParameterDeclaration'
  :: P.CParser m => P.ParameterDeclaration -> m ParameterDeclaration
untangleParameterDeclaration' pDecl =
  case untangleParameterDeclaration pDecl of
    Left err -> fail $ pretty80 $
      "Error while parsing declaration:" </> PP.pretty err </> PP.pretty pDecl
    Right x -> return x

parseParameterDeclaration :: P.CParser m => m ParameterDeclaration
parseParameterDeclaration =
  untangleParameterDeclaration' =<< P.parameter_declaration

parseParameterList :: P.CParser m => m [ParameterDeclaration]
parseParameterList =
  mapM untangleParameterDeclaration' =<< P.parameter_list

parseIdentifier :: P.CParser m => m P.Id
parseIdentifier = P.identifier_no_lex

parseType :: P.CParser m => m Type
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

instance PP.Pretty ParameterDeclaration where
  pretty = PP.pretty . tangleParameterDeclaration

instance PP.Pretty Type where
  pretty ty =
    PP.pretty $ tangleParameterDeclaration $ ParameterDeclaration Nothing ty

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Doc -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 x) ""
