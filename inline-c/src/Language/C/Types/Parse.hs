module Language.C.Types.Parse
  ( Lex.Identifier
  , TypeQual(..)
  , TypeSpec(..)
  , DeclarationSpec(..)
  , Declarator(..)
  , ArraySize
  , Declaration(..)

  , Parser
  , parseDeclaration
  , parseParams
  ) where

import           Text.Parsec
import           Control.Monad (msum)
import           Data.Functor ((<$>))
import           Control.Applicative ((<*>))

import qualified Language.C.Types.Lex as Lex

------------------------------------------------------------------------
-- Types

data TypeQual = Const
  deriving (Eq, Show)

data TypeSpec
  = Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Signed
  | Unsigned
  | TypeName String
  deriving (Eq, Show)

data DeclarationSpec = DeclarationSpec [TypeQual] TypeSpec
  deriving (Eq, Show)

data Declarator
  = DeclaratorRoot String
  | Ptr [TypeQual] Declarator
  | Array (Maybe ArraySize) Declarator
  | Proto Declarator [Declaration]
  deriving (Eq, Show)

type ArraySize = Integer

data Declaration
  = Declaration DeclarationSpec Declarator
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Parse

type Parser = Parsec [(SourcePos, Lex.Token)] ()

parseToken :: Lex.Token -> a -> Parser a
parseToken tok x =
  token (Lex.prettyToken . snd) fst $ \(_, tok') ->
    if tok == tok' then Just x else Nothing

parseIdent :: (Lex.Identifier -> a) -> Parser a
parseIdent f =
  token (Lex.prettyToken . snd) fst $ \(_, tok') ->
    case tok' of
      Lex.IDENTIFIER s -> Just $ f s
      _ -> Nothing

parseInteger :: (Integer -> a) -> Parser a
parseInteger f =
  token (Lex.prettyToken . snd) fst $ \(_, tok') ->
    case tok' of
      Lex.INTEGER s -> Just $ f s
      _ -> Nothing

parseTypeSpec :: Parser TypeSpec
parseTypeSpec = msum
  [ parseToken Lex.VOID Void
  , parseToken Lex.CHAR Char
  , parseToken Lex.SHORT Short
  , parseToken Lex.INT Int
  , parseToken Lex.LONG Long
  , parseToken Lex.FLOAT Float
  , parseToken Lex.DOUBLE Double
  , parseToken Lex.SIGNED Signed
  , parseToken Lex.UNSIGNED Unsigned
  , parseIdent TypeName
  ]

parseTypeQual :: Parser TypeQual
parseTypeQual = msum
  [ parseToken Lex.CONST Const ]

parseDeclarationSpec :: Parser DeclarationSpec
parseDeclarationSpec =
  DeclarationSpec <$> many parseTypeQual <*> parseTypeSpec

-- Intermediate structure to parse damned declarations

data RawDeclarator
  = RawDeclarator [[TypeQual]] RawDeclaratorRoot [RawDeclaratorTrailing]

data RawDeclaratorRoot
  = RawDeclaratorRoot String
  | RawDeclaratorParens Declarator

data RawDeclaratorTrailing
  = RawDeclaratorArray (Maybe ArraySize)
  | RawDeclaratorProto [Declaration]

fromRawDecl :: RawDeclarator -> Declarator
fromRawDecl (RawDeclarator ptrs0 root trailings0) = goTrailing trailings0
  where
    goPtrs :: [[TypeQual]] -> Declarator
    goPtrs [] = case root of
      RawDeclaratorRoot s -> DeclaratorRoot s
      RawDeclaratorParens decl -> decl
    goPtrs (quals : ptrs) =
      Ptr quals $ goPtrs ptrs

    goTrailing :: [RawDeclaratorTrailing] -> Declarator
    goTrailing [] =
      goPtrs ptrs0
    goTrailing (trailing : trailings) = case trailing of
      RawDeclaratorArray mbSize -> Array mbSize $ goTrailing trailings
      RawDeclaratorProto decls -> Proto (goTrailing trailings) decls

parseRawDeclarator :: Parser RawDeclarator
parseRawDeclarator =
  RawDeclarator <$> many pointer <*> root <*> many trailing
  where
    pointer :: Parser [TypeQual]
    pointer = do
      parseToken Lex.PTR ()
      many parseTypeQual

    root :: Parser RawDeclaratorRoot
    root = msum
      [ do parseIdent RawDeclaratorRoot
      , do parseToken Lex.LPAREN ()
           dec <- parseDeclarator
           parseToken Lex.RPAREN ()
           return $ RawDeclaratorParens dec
      ]

    trailing :: Parser RawDeclaratorTrailing
    trailing = msum
      [ do parseToken Lex.LBRACK ()
           mbSize <- parseInteger Just <|> return Nothing
           parseToken Lex.RBRACK ()
           return $ RawDeclaratorArray mbSize
      , do parseToken Lex.LPAREN ()
           decs <- parseParams
           parseToken Lex.RPAREN ()
           return $ RawDeclaratorProto decs
      ]

parseDeclarator :: Parser Declarator
parseDeclarator = fromRawDecl <$> parseRawDeclarator

parseDeclaration :: Parser Declaration
parseDeclaration =
  Declaration <$> parseDeclarationSpec <*> parseDeclarator

parseParams :: Parser [Declaration]
parseParams = sepBy parseDeclaration $ parseToken Lex.COMMA ()
