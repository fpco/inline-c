{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A parser for C99 declarations. Currently, the parser has the following limitations:
--
-- * Array sizes can only be @*@, @n@ (where n is a positive integer), @x@
-- (where @x@ is a C identifier). In C99 they can be arbitrary expressions. See
-- the @'ArrayType'@ data type.
--
-- * @_Bool@, @_Complex@, and @_Imaginary@ are not present.
--
-- * Untyped parameter lists (pre-K&R C) are not allowed.
--
-- The parser is incremental and generic (see 'CParser').  'PP.Pretty'
-- and 'QC.Arbitrary' instances are provided for all the data types.
--
-- The entry point if you want to parse C declarations is
-- @'parameter_declaration'@.

module Language.C.Types.Parse
  ( -- * Parser configuration
    TypeNames
  , CParserContext(..)
    -- ** Default configuration
  , CIdentifier
  , unCIdentifier
  , cIdentifierFromString
  , cCParserContext

    -- * Parser type
  , CParser
  , runCParser
  , quickCParser
  , quickCParser_

    -- * Types and parsing
  -- , identifier
  , identifier_no_lex
  , DeclarationSpecifier(..)
  , declaration_specifiers
  , StorageClassSpecifier(..)
  , storage_class_specifier
  , TypeSpecifier(..)
  , type_specifier
  , TypeQualifier(..)
  , type_qualifier
  , FunctionSpecifier(..)
  , function_specifier
  , Declarator(..)
  , declarator
  , DirectDeclarator(..)
  , direct_declarator
  , ArrayOrProto(..)
  , array_or_proto
  , ArrayType(..)
  , array_type
  , Pointer(..)
  , pointer
  , ParameterDeclaration(..)
  , DeclaratorOrAbstractDeclarator(..)
  , parameter_declaration
  , parameter_list
  , AbstractDeclarator(..)
  , abstract_declarator
  , DirectAbstractDeclarator(..)
  , direct_abstract_declarator

    -- * YACC grammar
    -- $yacc

    -- * Testing utilities
  , cIdentStart
  , cIdentLetter
  , cReservedWords
  , isTypeName
  ) where

import           Control.Applicative
import           Control.Monad (msum, void, MonadPlus, unless, when)
import           Control.Monad.Reader (MonadReader, runReaderT, ReaderT, asks, ask)
import           Data.List (intersperse)
import           Data.Functor.Identity (Identity)
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import           Data.Typeable (Typeable)
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import qualified Text.Parser.Token.Highlight as Highlight
import           Prettyprinter (Pretty(..), (<+>), Doc, hsep)
import qualified Prettyprinter as PP

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)
#endif

------------------------------------------------------------------------
-- Config

-- | A collection of named types (typedefs)
type TypeNames = HashSet.HashSet CIdentifier

data CParserContext i = CParserContext
  { cpcIdentName :: String
  , cpcTypeNames :: TypeNames
    -- ^ Function used to determine whether an identifier is a type name.
  , cpcParseIdent :: forall m. CParser i m => m i
    -- ^ Parses an identifier, *without consuming whitespace afterwards*.
  , cpcIdentToString :: i -> String
  , cpcEnableCpp :: Bool
  }

-- | A type for C identifiers.
newtype CIdentifier = CIdentifier {unCIdentifier :: String}
  deriving (Typeable, Eq, Ord, Show, Hashable)

cIdentifierFromString :: Bool -> String -> Either String CIdentifier
cIdentifierFromString useCpp s =
  -- Note: it's important not to use 'cidentifier_raw' here, otherwise
  -- we go in a loop:
  --
  -- @
  -- cIdentifierFromString => fromString => cIdentifierFromString => ...
  -- @
  case Parsec.parse (identNoLex useCpp cIdentStyle <* eof) "cIdentifierFromString" s of
    Left err -> Left $ show err
    Right x -> Right $ CIdentifier x

instance IsString CIdentifier where
  fromString s =
    case cIdentifierFromString True s of
      Left err -> error $ "CIdentifier fromString: invalid string " ++ show s ++ "\n" ++ err
      Right x -> x

cCParserContext :: Bool -> TypeNames -> CParserContext CIdentifier
cCParserContext useCpp typeNames = CParserContext
  { cpcTypeNames = typeNames
  , cpcParseIdent = cidentifier_no_lex
  , cpcIdentToString = unCIdentifier
  , cpcIdentName = "C identifier"
  , cpcEnableCpp = useCpp
  }

------------------------------------------------------------------------
-- Parser

-- | All the parsing is done using the type classes provided by the
-- @parsers@ package. You can use the parsing routines with any of the parsers
-- that implement the classes, such as @parsec@ or @trifecta@.
--
-- We parametrize the parsing by the type of the variable identifiers,
-- @i@.  We do so because we use this parser to implement anti-quoters
-- referring to Haskell variables, and thus we need to parse Haskell
-- identifiers in certain positions.
type CParser i m =
  ( Monad m
  , Functor m
  , Applicative m
  , MonadPlus m
  , Parsing m
  , CharParsing m
  , TokenParsing m
  , LookAheadParsing m
  , MonadReader (CParserContext i) m
#if (MIN_VERSION_base(4,13,0))
  , MonadFail m
#endif
  , Hashable i
  )

-- | Runs a @'CParser'@ using @parsec@.
runCParser
  :: Parsec.Stream s Identity Char
  => CParserContext i
  -> String
  -- ^ Source name.
  -> s
  -- ^ String to parse.
  -> (ReaderT (CParserContext i) (Parsec.Parsec s ()) a)
  -- ^ Parser.  Anything with type @forall m. CParser i m => m a@ is a
  -- valid argument.
  -> Either Parsec.ParseError a
runCParser typeNames fn s p = Parsec.parse (runReaderT p typeNames) fn s

-- | Useful for quick testing.  Uses @\"quickCParser\"@ as source name, and throws
-- an 'error' if parsing fails.
quickCParser
  :: CParserContext i
  -> String
  -- ^ String to parse.
  -> (ReaderT (CParserContext i) (Parsec.Parsec String ()) a)
  -- ^ Parser.  Anything with type @forall m. CParser i m => m a@ is a
  -- valid argument.
  -> a
quickCParser typeNames s p = case runCParser typeNames "quickCParser" s p of
  Left err -> error $ "quickCParser: " ++ show err
  Right x -> x

-- | Like 'quickCParser', but uses @'cCParserContext' ('const' 'False')@ as
-- 'CParserContext'.
quickCParser_
  :: Bool
  -> String
  -- ^ String to parse.
  -> (ReaderT (CParserContext CIdentifier) (Parsec.Parsec String ()) a)
  -- ^ Parser.  Anything with type @forall m. CParser i m => m a@ is a
  -- valid argument.
  -> a
quickCParser_ useCpp = quickCParser (cCParserContext useCpp HashSet.empty)

cReservedWords :: HashSet.HashSet String
cReservedWords = HashSet.fromList
  [ "auto", "else", "long", "switch"
  , "break", "enum", "register", "typedef"
  , "case", "extern", "return", "union"
  , "char", "float", "short", "unsigned"
  , "const", "for", "signed", "void"
  , "continue", "goto", "sizeof", "volatile"
  , "default", "if", "static", "while"
  , "do", "int", "struct", "double"
  ]

cIdentStart :: [Char]
cIdentStart = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

cIdentLetter :: [Char]
cIdentLetter = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']

cIdentStyle :: (TokenParsing m, Monad m) => IdentifierStyle m
cIdentStyle = IdentifierStyle
  { _styleName = "C identifier"
  , _styleStart = oneOf cIdentStart
  , _styleLetter = oneOf cIdentLetter
  , _styleReserved = cReservedWords
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

data DeclarationSpecifier
  = StorageClassSpecifier StorageClassSpecifier
  | TypeSpecifier TypeSpecifier
  | TypeQualifier TypeQualifier
  | FunctionSpecifier FunctionSpecifier
  deriving (Typeable, Eq, Show)

declaration_specifiers :: CParser i m => m [DeclarationSpecifier]
declaration_specifiers = many1 $ msum
  [ StorageClassSpecifier <$> storage_class_specifier
  , TypeSpecifier <$> type_specifier
  , TypeQualifier <$> type_qualifier
  , FunctionSpecifier <$> function_specifier
  ]

data StorageClassSpecifier
  = TYPEDEF
  | EXTERN
  | STATIC
  | AUTO
  | REGISTER
  deriving (Typeable, Eq, Show)

storage_class_specifier :: CParser i m => m StorageClassSpecifier
storage_class_specifier = msum
  [ TYPEDEF <$ reserve cIdentStyle "typedef"
  , EXTERN <$ reserve cIdentStyle "extern"
  , STATIC <$ reserve cIdentStyle "static"
  , AUTO <$ reserve cIdentStyle "auto"
  , REGISTER <$ reserve cIdentStyle "register"
  ]

data TypeSpecifier
  = VOID
  | BOOL
  | CHAR
  | SHORT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  | SIGNED
  | UNSIGNED
  | Struct CIdentifier
  | Enum CIdentifier
  | TypeName CIdentifier
  | Template CIdentifier [[TypeSpecifier]]
  | TemplateConst String
  | TemplatePointer TypeSpecifier
  deriving (Typeable, Eq, Show)

type_specifier :: CParser i m => m TypeSpecifier
type_specifier = msum
  [ VOID <$ reserve cIdentStyle "void"
  , BOOL <$ reserve cIdentStyle "bool"
  , CHAR <$ reserve cIdentStyle "char"
  , SHORT <$ reserve cIdentStyle "short"
  , INT <$ reserve cIdentStyle "int"
  , LONG <$ reserve cIdentStyle "long"
  , FLOAT <$ reserve cIdentStyle "float"
  , DOUBLE <$ reserve cIdentStyle "double"
  , SIGNED <$ reserve cIdentStyle "signed"
  , UNSIGNED <$ reserve cIdentStyle "unsigned"
  , Struct <$> (reserve cIdentStyle "struct" >> cidentifier)
  , Enum <$> (reserve cIdentStyle "enum" >> cidentifier)
  , template_parser
  , TypeName <$> type_name
  ]

identifier :: CParser i m => m i
identifier = token identifier_no_lex

isTypeName :: Bool -> TypeNames -> String -> Bool
isTypeName useCpp typeNames id_ =
  case cIdentifierFromString useCpp id_ of
    -- If it's not a valid C identifier, then it's definitely not a C type name.
    Left _err -> False
    Right s -> HashSet.member s typeNames

identifier_no_lex :: CParser i m => m i
identifier_no_lex = try $ do
  ctx <- ask
  id_ <- cpcParseIdent ctx <?> cpcIdentName ctx
  when (isTypeName (cpcEnableCpp ctx) (cpcTypeNames ctx) (cpcIdentToString ctx id_)) $
    unexpected $ "type name " ++ cpcIdentToString ctx id_
  return id_

-- | Same as 'cidentifier_no_lex', but does not check that the
-- identifier is not a type name.
cidentifier_raw :: (TokenParsing m, Monad m) => Bool -> m CIdentifier
cidentifier_raw useCpp = identNoLex useCpp cIdentStyle

-- | This parser parses a 'CIdentifier' and nothing else -- it does not consume
-- trailing spaces and the like.
cidentifier_no_lex :: CParser i m => m CIdentifier
cidentifier_no_lex = try $ do
  ctx <- ask
  s <- cidentifier_raw (cpcEnableCpp ctx)
  typeNames <- asks cpcTypeNames
  when (HashSet.member s typeNames) $
    unexpected $ "type name " ++ unCIdentifier s
  return s

cidentifier :: CParser i m => m CIdentifier
cidentifier = token cidentifier_no_lex

type_name :: CParser i m => m CIdentifier
type_name = try $ do
  ctx <- ask
  s <- ident' (cpcEnableCpp ctx) cIdentStyle <?> "type name"
  typeNames <- asks cpcTypeNames
  unless (HashSet.member s typeNames) $
    unexpected $ "identifier  " ++ unCIdentifier s
  return s

templateParser :: (Monad m, CharParsing m, CParser i m) => IdentifierStyle m -> m TypeSpecifier
templateParser s = parse'
  where
    parse' = do
      id' <- cidentParserWithNamespace
      _ <- string "<"
      args <- templateArgParser
      _ <- string ">"
      return $ Template (CIdentifier id') args
    cidentParser = ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
    cidentParserWithNamespace =
      try (concat <$> sequence [cidentParser, (string "::"), cidentParserWithNamespace]) <|>
      cidentParser
    templateArgType = try ((TemplatePointer <$> (type_specifier)) <* (string "*")) <|> try type_specifier <|> (TemplateConst <$> (some $ oneOf ['0'..'9']))
    templateArgParser' = do
      t <- some (token templateArgType)
      _ <- string ","
      tt <- templateArgParser
      return $ t:tt
    templateArgParser =
      try (templateArgParser') <|> ((:) <$> some (token templateArgType) <*> return [])

template_parser :: CParser i m => m TypeSpecifier
template_parser = try $ templateParser cIdentStyle <?> "template name"

data TypeQualifier
  = CONST
  | RESTRICT
  | VOLATILE
  deriving (Typeable, Eq, Show)

type_qualifier :: CParser i m => m TypeQualifier
type_qualifier = msum
  [ CONST <$ reserve cIdentStyle "const"
  , RESTRICT <$ reserve cIdentStyle "restrict"
  , VOLATILE <$ reserve cIdentStyle "volatile"
  ]

data FunctionSpecifier
  = INLINE
  deriving (Typeable, Eq, Show)

function_specifier :: CParser i m => m FunctionSpecifier
function_specifier = msum
  [ INLINE <$ reserve cIdentStyle "inline"
  ]

data Declarator i = Declarator
  { declaratorPointers :: [Pointer]
  , declaratorDirect :: (DirectDeclarator i)
  } deriving (Typeable, Eq, Show, Functor, Foldable, Traversable)

declarator :: CParser i m => m (Declarator i)
declarator = (Declarator <$> many pointer <*> direct_declarator) <?> "declarator"

data DirectDeclarator i
  = DeclaratorRoot i
  | ArrayOrProto (DirectDeclarator i) (ArrayOrProto i)
  | DeclaratorParens (Declarator i)
  deriving (Typeable, Eq, Show, Functor, Foldable, Traversable)

data ArrayOrProto i
  = Array (ArrayType i)
  | Proto [ParameterDeclaration i] -- We don't include old prototypes.
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

array_or_proto :: CParser i m => m (ArrayOrProto i)
array_or_proto = msum
  [ Array <$> brackets array_type
  , Proto <$> parens parameter_list
  ]

-- TODO handle more stuff in array brackets
data ArrayType i
  = VariablySized
  | Unsized
  | SizedByInteger Integer
  | SizedByIdentifier i
  deriving (Typeable, Eq, Show, Functor, Foldable, Traversable)

array_type :: CParser i m => m (ArrayType i)
array_type = msum
  [ VariablySized <$ symbolic '*'
  , SizedByInteger <$> natural
  , SizedByIdentifier <$> identifier
  , return Unsized
  ]

direct_declarator :: CParser i m => m (DirectDeclarator i)
direct_declarator = do
  ddecltor <- msum
    [ DeclaratorRoot <$> identifier
    , DeclaratorParens <$> parens declarator
    ]
  aops <- many array_or_proto
  return $ foldl ArrayOrProto ddecltor aops

data Pointer
  = Pointer [TypeQualifier]
  deriving (Typeable, Eq, Show)

pointer :: CParser i m => m Pointer
pointer = do
  void $ symbolic '*'
  Pointer <$> many type_qualifier

parameter_list :: CParser i m => m [ParameterDeclaration i]
parameter_list =
  sepBy parameter_declaration $ symbolic ','

data ParameterDeclaration i = ParameterDeclaration
  { parameterDeclarationSpecifiers :: [DeclarationSpecifier]
  , parameterDeclarationDeclarator :: DeclaratorOrAbstractDeclarator i
  } deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

data DeclaratorOrAbstractDeclarator i
  = IsDeclarator (Declarator i)
  | IsAbstractDeclarator (AbstractDeclarator i)
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

parameter_declaration :: CParser i m => m (ParameterDeclaration i)
parameter_declaration =
  ParameterDeclaration
    <$> declaration_specifiers
    <*> mbabstract
  where
   mbabstract =
     IsDeclarator <$> try declarator <|>
     IsAbstractDeclarator <$> try abstract_declarator <|>
     return (IsAbstractDeclarator (AbstractDeclarator [] Nothing))

data AbstractDeclarator i = AbstractDeclarator
  { abstractDeclaratorPointers :: [Pointer]
  , abstractDeclaratorDirect :: Maybe (DirectAbstractDeclarator i)
  } deriving (Typeable, Eq, Show, Functor, Foldable, Traversable)

abstract_declarator :: CParser i m => m (AbstractDeclarator i)
abstract_declarator = do
  ptrs <- many pointer
  -- If there are no pointers, there must be an abstract declarator.
  let p = if null ptrs
        then Just <$> direct_abstract_declarator
        else (Just <$> try direct_abstract_declarator) <|> return Nothing
  AbstractDeclarator ptrs <$> p

data DirectAbstractDeclarator i
  = ArrayOrProtoHere (ArrayOrProto i)
  | ArrayOrProtoThere (DirectAbstractDeclarator i) (ArrayOrProto i)
  | AbstractDeclaratorParens (AbstractDeclarator i)
  deriving (Typeable, Eq, Show, Functor, Foldable, Traversable)

direct_abstract_declarator :: CParser i m => m (DirectAbstractDeclarator i)
direct_abstract_declarator = do
  ddecltor <- msum
    [ try (ArrayOrProtoHere <$> array_or_proto)
    , AbstractDeclaratorParens <$> parens abstract_declarator
    ] <?> "array, prototype, or parenthesised abstract declarator"
  aops <- many array_or_proto
  return $ foldl ArrayOrProtoThere ddecltor aops

------------------------------------------------------------------------
-- Pretty printing

instance Pretty CIdentifier where
  pretty = fromString . unCIdentifier

instance Pretty DeclarationSpecifier where
  pretty dspec = case dspec of
    StorageClassSpecifier x -> pretty x
    TypeSpecifier x -> pretty x
    TypeQualifier x -> pretty x
    FunctionSpecifier x -> pretty x

instance Pretty StorageClassSpecifier where
  pretty storage = case storage of
    TYPEDEF -> "typedef"
    EXTERN -> "extern"
    STATIC -> "static"
    AUTO -> "auto"
    REGISTER -> "register"

instance Pretty TypeSpecifier where
  pretty tySpec = case tySpec of
   VOID -> "void"
   BOOL -> "bool"
   CHAR -> "char"
   SHORT -> "short"
   INT -> "int"
   LONG -> "long"
   FLOAT -> "float"
   DOUBLE -> "double"
   SIGNED -> "signed"
   UNSIGNED -> "unsigned"
   Struct x -> "struct" <+> pretty x
   Enum x -> "enum" <+> pretty x
   TypeName x -> pretty x
   Template x args ->
     -- This code generates a c++ code of "template-identifier<template-argument1,template-argument2,..>" like "std::vector<int>".
     -- concat_with_space is used to concat multiple terms like "unsigned int".
     let concat_with_space = mconcat . (intersperse " ") . (map pretty)
     in pretty x <+> "<" <+> mconcat (intersperse "," (map concat_with_space args))  <+> ">"
   TemplateConst x -> pretty x
   TemplatePointer x -> pretty x <+> "*"

instance Pretty TypeQualifier where
  pretty tyQual = case tyQual of
    CONST -> "const"
    RESTRICT -> "restrict"
    VOLATILE -> "volatile"

instance Pretty FunctionSpecifier where
  pretty funSpec = case funSpec of
    INLINE -> "inline"

instance Pretty i => Pretty (Declarator i) where
  pretty (Declarator ptrs ddecltor) = case ptrs of
    [] -> pretty ddecltor
    _:_ -> prettyPointers ptrs <+> pretty ddecltor

prettyPointers :: [Pointer] -> Doc ann
prettyPointers [] = ""
prettyPointers (x : xs) = pretty x <> prettyPointers xs

instance Pretty Pointer where
  pretty (Pointer tyQual) = "*" <> hsep (map pretty tyQual)

instance Pretty i => Pretty (DirectDeclarator i) where
  pretty decltor = case decltor of
    DeclaratorRoot x -> pretty x
    DeclaratorParens x -> "(" <> pretty x <> ")"
    ArrayOrProto ddecltor aorp -> pretty ddecltor <> pretty aorp

instance Pretty i => Pretty (ArrayOrProto i) where
  pretty aorp = case aorp of
    Array x -> "[" <> pretty x <> "]"
    Proto x -> "(" <> prettyParams x <> ")"

prettyParams :: (Pretty a) => [a] -> Doc ann
prettyParams xs = case xs of
  [] -> ""
  [x] -> pretty x
  x : xs'@(_:_) -> pretty x <> "," <+> prettyParams xs'

instance Pretty i => Pretty (ArrayType i) where
  pretty at = case at of
    VariablySized -> "*"
    SizedByInteger n -> pretty n
    SizedByIdentifier s -> pretty s
    Unsized -> ""

instance Pretty i => Pretty (ParameterDeclaration i) where
  pretty (ParameterDeclaration declSpecs decltor) = case declSpecs of
    [] -> decltorDoc
    _:_ -> hsep (map pretty declSpecs) <+> decltorDoc
    where
      decltorDoc = case decltor of
        IsDeclarator x -> pretty x
        IsAbstractDeclarator x -> pretty x

instance Pretty i => Pretty (AbstractDeclarator i) where
  pretty (AbstractDeclarator ptrs mbDecltor) = case (ptrs, mbDecltor) of
    (_, Nothing) -> prettyPointers ptrs
    ([], Just x) -> pretty x
    (_:_, Just x) -> prettyPointers ptrs <+> pretty x

instance Pretty i => Pretty (DirectAbstractDeclarator i) where
  pretty ddecltor = case ddecltor of
    AbstractDeclaratorParens x -> "(" <> pretty x <> ")"
    ArrayOrProtoHere aop -> pretty aop
    ArrayOrProtoThere ddecltor' aop -> pretty ddecltor' <> pretty aop

------------------------------------------------------------------------
-- Utils

many1 :: CParser i m => m a -> m [a]
many1 p = (:) <$> p <*> many p

------------------------------------------------------------------------
-- YACC grammar

-- $yacc
--
-- The parser above is derived from a modification of the YACC grammar
-- for C99 found at <http://www.quut.com/c/ANSI-C-grammar-y-1999.html>,
-- reproduced below.
--
-- @
-- %token IDENTIFIER TYPE_NAME INTEGER
--
-- %token TYPEDEF EXTERN STATIC AUTO REGISTER INLINE RESTRICT
-- %token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
-- %token BOOL COMPLEX IMAGINARY
-- %token STRUCT UNION ENUM
--
-- %start parameter_list
-- %%
--
-- declaration_specifiers
-- 	: storage_class_specifier
-- 	| storage_class_specifier declaration_specifiers
-- 	| type_specifier
-- 	| type_specifier declaration_specifiers
-- 	| type_qualifier
-- 	| type_qualifier declaration_specifiers
-- 	| function_specifier
-- 	| function_specifier declaration_specifiers
-- 	;
--
-- storage_class_specifier
-- 	: TYPEDEF
-- 	| EXTERN
-- 	| STATIC
-- 	| AUTO
-- 	| REGISTER
-- 	;
--
-- type_specifier
-- 	: VOID
-- 	| CHAR
-- 	| SHORT
-- 	| INT
-- 	| LONG
-- 	| FLOAT
-- 	| DOUBLE
-- 	| SIGNED
-- 	| UNSIGNED
-- 	| BOOL
-- 	| COMPLEX
-- 	| IMAGINARY
--  	| STRUCT IDENTIFIER
-- 	| UNION IDENTIFIER
-- 	| ENUM IDENTIFIER
-- 	| TYPE_NAME
-- 	;
--
-- type_qualifier
-- 	: CONST
-- 	| RESTRICT
-- 	| VOLATILE
-- 	;
--
-- function_specifier
-- 	: INLINE
-- 	;
--
-- declarator
-- 	: pointer direct_declarator
-- 	| direct_declarator
-- 	;
--
-- direct_declarator
-- 	: IDENTIFIER
-- 	| '(' declarator ')'
-- 	| direct_declarator '[' type_qualifier_list ']'
-- 	| direct_declarator '[' type_qualifier_list '*' ']'
-- 	| direct_declarator '[' '*' ']'
--  	| direct_declarator '[' IDENTIFIER ']'
-- 	| direct_declarator '[' INTEGER ']'
-- 	| direct_declarator '[' ']'
-- 	| direct_declarator '(' parameter_list ')'
-- 	| direct_declarator '(' ')'
-- 	;
--
-- pointer
-- 	: '*'
-- 	| '*' type_qualifier_list
-- 	| '*' pointer
-- 	| '*' type_qualifier_list pointer
-- 	;
--
-- type_qualifier_list
-- 	: type_qualifier
-- 	| type_qualifier_list type_qualifier
-- 	;
--
-- parameter_list
-- 	: parameter_declaration
-- 	| parameter_list ',' parameter_declaration
-- 	;
--
-- parameter_declaration
-- 	: declaration_specifiers declarator
-- 	| declaration_specifiers abstract_declarator
-- 	| declaration_specifiers
-- 	;
--
-- abstract_declarator
-- 	: pointer
-- 	| direct_abstract_declarator
-- 	| pointer direct_abstract_declarator
-- 	;
--
-- direct_abstract_declarator
-- 	: '(' abstract_declarator ')'
-- 	| '[' ']'
-- 	| direct_abstract_declarator '[' ']'
-- 	| '[' '*' ']'
-- 	| direct_abstract_declarator '[' '*' ']'
-- 	| '[' IDENTIFIER ']'
-- 	| direct_abstract_declarator '[' IDENTIFIER ']'
-- 	| '[' INTEGER ']'
-- 	| direct_abstract_declarator '[' INTEGER ']'
-- 	| '(' ')'
-- 	| '(' parameter_list ')'
-- 	| direct_abstract_declarator '(' ')'
-- 	| direct_abstract_declarator '(' parameter_list ')'
-- 	;
--
-- %%
-- #include \<stdio.h\>
--
-- extern char yytext[];
-- extern int column;
--
-- void yyerror(char const *s)
-- {
-- 	fflush(stdout);
-- 	printf("\n%*s\n%*s\n", column, "^", column, s);
-- }
-- @

-- Utils
------------------------------------------------------------------------

cppIdentParser :: (Monad m, CharParsing m) => Bool -> IdentifierStyle m -> m [Char]
cppIdentParser useCpp s = cidentParserWithNamespace
  where
    cidentParser = ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
    cidentParserWithNamespace =
      if useCpp
      then
        try (concat <$> sequence [cidentParser, (string "::"), cidentParserWithNamespace]) <|>
        cidentParser
      else
        cidentParser

identNoLex :: (TokenParsing m, Monad m, IsString s) => Bool -> IdentifierStyle m -> m s
identNoLex useCpp s = fmap fromString $ try $ do
  name <- highlight (_styleHighlight s) (cppIdentParser useCpp s)
  when (HashSet.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
  return name

ident' :: (TokenParsing m, Monad m, IsString s) => Bool -> IdentifierStyle m -> m s
ident' useCpp s = fmap fromString $ token $ try $ do
  name <- highlight (_styleHighlight s) (cppIdentParser useCpp s)
  when (HashSet.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
  return name
