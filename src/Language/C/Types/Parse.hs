{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( -- * Parser type
    CParser
  , IsTypeName
  , runCParser
  , quickCParser
  , quickCParser_

    -- * Types and parsing
  , Id(..)
  , identifier
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
  , parameter_declaration
  , parameter_list
  , AbstractDeclarator(..)
  , abstract_declarator
  , DirectAbstractDeclarator(..)
  , direct_abstract_declarator

    -- * YACC grammar
    -- $yacc

    -- * Testing utilities
  , ParameterDeclarationWithTypeNames(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (msum, void, MonadPlus, unless, when)
import           Control.Monad.Reader (MonadReader, ask, runReaderT, ReaderT)
import           Data.Functor.Identity (Identity)
import qualified Data.HashSet as HashSet
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.String (IsString(..))
import           Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..), (<+>), Doc, hsep)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

------------------------------------------------------------------------
-- Parser

-- | Function used to determine whether an 'C.Id' is a type name.
type IsTypeName = Id -> Bool

-- | All the parsing is done using the type classes provided by the
-- @parsers@ package. You can use the parsing routines with any of the parsers
-- that implement the classes, such as @parsec@ or @trifecta@.
--
-- The 'MonadReader' with 'IsTypeName' is required for parsing C, see
-- <http://en.wikipedia.org/wiki/The_lexer_hack>.
type CParser m = (Monad m, Functor m, Applicative m, MonadPlus m, Parsing m, CharParsing m, TokenParsing m, LookAheadParsing m, MonadReader IsTypeName m)

-- | Runs a @'CParser'@ using @parsec@.
runCParser
  :: Parsec.Stream s Identity Char
  => IsTypeName
  -- ^ Function determining if an identifier is a type name.
  -> String
  -- ^ Source name.
  -> s
  -- ^ String to parse.
  -> (ReaderT IsTypeName (Parsec.Parsec s ()) a)
  -- ^ Parser.  Anything with type @forall m. CParser m => m a@ is a
  -- valid argument.
  -> Either Parsec.ParseError a
runCParser isTypeName fn s p = Parsec.parse (runReaderT p isTypeName) fn s

-- | Useful for quick testing.  Uses @\"quickCParser\"@ as source name, and throws
-- an 'error' if parsing fails.
quickCParser
  :: IsTypeName
  -- ^ Function determining if an identifier is a type name.
  -> String
  -- ^ String to parse.
  -> (ReaderT IsTypeName (Parsec.Parsec String ()) a)
  -- ^ Parser.  Anything with type @forall m. CParser m => m a@ is a
  -- valid argument.
  -> a
quickCParser isTypeName s p = case runCParser isTypeName "quickCParser" s p of
  Left err -> error $ "quickCParser: " ++ show err
  Right x -> x

-- | Like 'quickCParser', but uses @'const' 'False'@ as 'IsTypeName'.
quickCParser_
  :: String
  -- ^ String to parse.
  -> (ReaderT IsTypeName (Parsec.Parsec String ()) a)
  -- ^ Parser.  Anything with type @forall m. CParser m => m a@ is a
  -- valid argument.
  -> a
quickCParser_ = quickCParser (const False)

newtype Id = Id {unId :: String}
  deriving (Typeable, Eq, Ord, Show)

instance IsString Id where
  fromString s =
    case runCParser (const False) "fromString" s (identifier_no_lex <* eof) of
      Left _err -> error $ "Id fromString: invalid string " ++ show s
      Right x -> x

identLetter :: CParser m => m Char
identLetter = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

reservedWords :: HashSet.HashSet String
reservedWords = HashSet.fromList
  [ "auto", "else", "long", "switch"
  , "break", "enum", "register", "typedef"
  , "case", "extern", "return", "union"
  , "char", "float", "short", "unsigned"
  , "const", "for", "signed", "void"
  , "continue", "goto", "sizeof", "volatile"
  , "default", "if", "static", "while"
  , "do", "int", "struct", "double"
  ]

identStyle :: CParser m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "C identifier"
  , _styleStart = identLetter
  , _styleLetter = identLetter <|> digit
  , _styleReserved = reservedWords
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

data DeclarationSpecifier
  = StorageClassSpecifier StorageClassSpecifier
  | TypeSpecifier TypeSpecifier
  | TypeQualifier TypeQualifier
  | FunctionSpecifier FunctionSpecifier
  deriving (Typeable, Eq, Show)

declaration_specifiers :: forall m. CParser m => m [DeclarationSpecifier]
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

storage_class_specifier :: CParser m => m StorageClassSpecifier
storage_class_specifier = msum
  [ TYPEDEF <$ reserve identStyle "typedef"
  , EXTERN <$ reserve identStyle "extern"
  , STATIC <$ reserve identStyle "static"
  , AUTO <$ reserve identStyle "auto"
  , REGISTER <$ reserve identStyle "register"
  ]

data TypeSpecifier
  = VOID
  | CHAR
  | SHORT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  | SIGNED
  | UNSIGNED
  | Struct Id
  | Enum Id
  | TypeName Id
  deriving (Typeable, Eq, Show)

type_specifier :: CParser m => m TypeSpecifier
type_specifier = msum
  [ VOID <$ reserve identStyle "void"
  , CHAR <$ reserve identStyle "char"
  , SHORT <$ reserve identStyle "short"
  , INT <$ reserve identStyle "int"
  , LONG <$ reserve identStyle "long"
  , FLOAT <$ reserve identStyle "float"
  , DOUBLE <$ reserve identStyle "double"
  , SIGNED <$ reserve identStyle "signed"
  , UNSIGNED <$ reserve identStyle "unsigned"
  , Struct <$> (reserve identStyle "struct" >> identifier)
  , Enum <$> (reserve identStyle "enum" >> identifier)
  , TypeName <$> type_name
  ]

identifier :: CParser m => m Id
identifier =
  try (do s <- ident identStyle
          isTypeName <- ask
          when (isTypeName s) $
            fail "expecting identifier, got type name"
          return s)
  <?> "identifier"

type_name :: CParser m => m Id
type_name =
  try (do s <- ident identStyle
          isTypeName <- ask
          unless (isTypeName s) $
            fail "expecting type name, got identifier"
          return s)
  <?> "type name"

data TypeQualifier
  = CONST
  | RESTRICT
  | VOLATILE
  deriving (Typeable, Eq, Show)

type_qualifier :: CParser m => m TypeQualifier
type_qualifier = msum
  [ CONST <$ reserve identStyle "const"
  , RESTRICT <$ reserve identStyle "restrict"
  , VOLATILE <$ reserve identStyle "volatile"
  ]

data FunctionSpecifier
  = INLINE
  deriving (Typeable, Eq, Show)

function_specifier :: CParser m => m FunctionSpecifier
function_specifier = msum
  [ INLINE <$ reserve identStyle "inline"
  ]

data Declarator = Declarator
  { declaratorPointers :: [Pointer]
  , declaratorDirect :: DirectDeclarator
  } deriving (Typeable, Eq, Show)

declarator :: CParser m => m Declarator
declarator = (Declarator <$> many pointer <*> direct_declarator) <?> "declarator"

data DirectDeclarator
  = DeclaratorRoot Id
  | ArrayOrProto DirectDeclarator ArrayOrProto
  | DeclaratorParens Declarator
  deriving (Typeable, Eq, Show)

data ArrayOrProto
  = Array ArrayType
  | Proto [ParameterDeclaration] -- We don't include old prototypes.
  deriving (Typeable, Eq, Show)

array_or_proto :: CParser m => m ArrayOrProto
array_or_proto = msum
  [ Array <$> brackets array_type
  , Proto <$> parens parameter_list
  ]

-- TODO handle more stuff in array brackets
data ArrayType
  = VariablySized
  | Unsized
  | SizedByInteger Integer
  | SizedByIdentifier Id
  deriving (Typeable, Eq, Show)

array_type :: CParser m => m ArrayType
array_type = msum
  [ VariablySized <$ symbolic '*'
  , SizedByInteger <$> natural
  , SizedByIdentifier <$> identifier
  , return Unsized
  ]

direct_declarator :: CParser m => m DirectDeclarator
direct_declarator =
  (do ddecltor <- msum
        [ DeclaratorRoot <$> identifier
        , DeclaratorParens <$> parens declarator
        ]
      aops <- many array_or_proto
      return $ foldl ArrayOrProto ddecltor aops)

data Pointer
  = Pointer [TypeQualifier]
  deriving (Typeable, Eq, Show)

pointer :: CParser m => m Pointer
pointer = do
  void $ symbolic '*'
  Pointer <$> many type_qualifier

parameter_list :: CParser m => m [ParameterDeclaration]
parameter_list =
  sepBy parameter_declaration $ symbolic ','

data ParameterDeclaration = ParameterDeclaration
  { parameterDeclarationSpecifiers :: [DeclarationSpecifier]
  , parameterDeclarationDeclarator :: Either Declarator AbstractDeclarator
  } deriving (Typeable, Eq, Show)

parameter_declaration :: CParser m => m ParameterDeclaration
parameter_declaration =
  ParameterDeclaration
    <$> declaration_specifiers
    <*> mbabstract
  where
   mbabstract =
     Left <$> try declarator <|>
     Right <$> try abstract_declarator <|>
     return (Right (AbstractDeclarator [] Nothing))

data AbstractDeclarator = AbstractDeclarator
  { abstractDeclaratorPointers :: [Pointer]
  , abstractDeclaratorDirect :: Maybe DirectAbstractDeclarator
  } deriving (Typeable, Eq, Show)

abstract_declarator :: CParser m => m AbstractDeclarator
abstract_declarator = do
  ptrs <- many pointer
  -- If there are no pointers, there must be an abstract declarator.
  let p = if null ptrs
        then Just <$> direct_abstract_declarator
        else (Just <$> try direct_abstract_declarator) <|> return Nothing
  AbstractDeclarator ptrs <$> p

data DirectAbstractDeclarator
  = ArrayOrProtoHere ArrayOrProto
  | ArrayOrProtoThere DirectAbstractDeclarator ArrayOrProto
  | AbstractDeclaratorParens AbstractDeclarator
  deriving (Typeable, Eq, Show)

direct_abstract_declarator :: CParser m => m DirectAbstractDeclarator
direct_abstract_declarator =
  (do ddecltor <- msum
        [ try (ArrayOrProtoHere <$> array_or_proto)
        , AbstractDeclaratorParens <$> parens abstract_declarator
        ] <?> "array, prototype, or parenthesised abstract declarator"
      aops <- many array_or_proto
      return $ foldl ArrayOrProtoThere ddecltor aops)

-- | This parser parses an 'Id' and nothing else -- it does not consume
-- trailing spaces and the like.
identifier_no_lex :: CParser m => m Id
identifier_no_lex =
  try (do s <- Id <$> ((:) <$> identLetter <*> many (identLetter <|> digit))
          isTypeName <- ask
          when (isTypeName s) $
            fail "expecting identifier, got type name"
          return s)
  <?> "identifier"

------------------------------------------------------------------------
-- Pretty printing

instance Pretty Id where
  pretty = PP.text . unId

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

instance Pretty TypeQualifier where
  pretty tyQual = case tyQual of
    CONST -> "const"
    RESTRICT -> "restrict"
    VOLATILE -> "volatile"

instance Pretty FunctionSpecifier where
  pretty funSpec = case funSpec of
    INLINE -> "inline"

instance Pretty Declarator where
  pretty (Declarator ptrs ddecltor) = case ptrs of
    [] -> pretty ddecltor
    _:_ -> prettyPointers ptrs <+> pretty ddecltor

prettyPointers :: [Pointer] -> Doc
prettyPointers [] = ""
prettyPointers (x : xs) = pretty x <> prettyPointers xs

instance Pretty Pointer where
  pretty (Pointer tyQual) = "*" <> hsep (map pretty tyQual)

instance Pretty DirectDeclarator where
  pretty decltor = case decltor of
    DeclaratorRoot x -> pretty x
    DeclaratorParens x -> "(" <> pretty x <> ")"
    ArrayOrProto ddecltor aorp -> pretty ddecltor <> pretty aorp

instance Pretty ArrayOrProto where
  pretty aorp = case aorp of
    Array x -> "[" <> pretty x <> "]"
    Proto x -> "(" <> prettyParams x <> ")"

prettyParams :: (Pretty a) => [a] -> Doc
prettyParams xs = case xs of
  [] -> ""
  [x] -> pretty x
  x : xs'@(_:_) -> pretty x <> "," <+> prettyParams xs'

instance Pretty ArrayType where
  pretty at = case at of
    VariablySized -> "*"
    SizedByInteger n -> pretty n
    SizedByIdentifier s -> pretty s
    Unsized -> ""

instance Pretty ParameterDeclaration where
  pretty (ParameterDeclaration declSpecs decltor) = case declSpecs of
    [] -> decltorDoc
    _:_ -> hsep (map pretty declSpecs) <+> decltorDoc
    where
      decltorDoc = case decltor of
        Left x -> pretty x
        Right x -> pretty x

instance Pretty AbstractDeclarator where
  pretty (AbstractDeclarator ptrs mbDecltor) = case (ptrs, mbDecltor) of
    (_, Nothing) -> prettyPointers ptrs
    ([], Just x) -> pretty x
    (_:_, Just x) -> prettyPointers ptrs <+> pretty x

instance Pretty DirectAbstractDeclarator where
  pretty ddecltor = case ddecltor of
    AbstractDeclaratorParens x -> "(" <> pretty x <> ")"
    ArrayOrProtoHere aop -> pretty aop
    ArrayOrProtoThere ddecltor' aop -> pretty ddecltor' <> pretty aop

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

arbitraryId :: QC.Gen Id
arbitraryId = do
    s <- ((:) <$> QC.elements letters <*> QC.listOf (QC.elements (letters ++ digits)))
    if HashSet.member s reservedWords
      then arbitraryId
      else return $ Id s
  where
    letters = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    digits = ['0'..'9']

-- | Type used to generate an 'QC.Arbitrary' 'ParameterDeclaration' with
-- arbitrary allowed type names.
data ParameterDeclarationWithTypeNames = ParameterDeclarationWithTypeNames
  { pdwtnTypeNames :: Set.Set Id
  , pdwtnParameterDeclaration :: ParameterDeclaration
  } deriving (Typeable, Eq, Show)

instance QC.Arbitrary ParameterDeclarationWithTypeNames where
  arbitrary = do
    names <- Set.fromList <$> QC.listOf arbitraryId
    decl <- arbitraryParameterDeclaration names
    return $ ParameterDeclarationWithTypeNames names decl

arbitraryDeclarationSpecifier :: Set.Set Id -> QC.Gen DeclarationSpecifier
arbitraryDeclarationSpecifier typeNames = QC.oneof $
  [ StorageClassSpecifier <$> QC.arbitrary
  , TypeQualifier <$> QC.arbitrary
  , FunctionSpecifier <$> QC.arbitrary
  , TypeSpecifier <$> arbitraryTypeSpecifier typeNames
  ]

instance QC.Arbitrary StorageClassSpecifier where
  arbitrary = QC.oneof
    [ return TYPEDEF
    , return EXTERN
    , return STATIC
    , return AUTO
    , return REGISTER
    ]

arbitraryTypeSpecifier :: Set.Set Id -> QC.Gen TypeSpecifier
arbitraryTypeSpecifier typeNames = QC.oneof $
  [ return VOID
  , return CHAR
  , return SHORT
  , return INT
  , return LONG
  , return FLOAT
  , return DOUBLE
  , return SIGNED
  , return UNSIGNED
  , Struct <$> arbitraryIdentifier typeNames
  , Enum <$> arbitraryIdentifier typeNames
  ] ++ if Set.null typeNames then []
       else [TypeName <$> QC.elements (Set.toList typeNames)]

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

arbitraryDeclarator :: Set.Set Id -> QC.Gen Declarator
arbitraryDeclarator typeNames = halveSize $
  Declarator <$> QC.arbitrary <*> arbitraryDirectDeclarator typeNames

arbitraryIdentifier :: Set.Set Id -> QC.Gen Id
arbitraryIdentifier typeNames = do
  id' <- arbitraryId
  if Set.member id' typeNames
    then arbitraryIdentifier typeNames
    else return id'

arbitraryDirectDeclarator :: Set.Set Id -> QC.Gen DirectDeclarator
arbitraryDirectDeclarator typeNames = halveSize $ oneOfSized $
  [ Anyhow $ DeclaratorRoot <$> arbitraryIdentifier typeNames
  , IfPositive $ DeclaratorParens <$> arbitraryDeclarator typeNames
  , IfPositive $ ArrayOrProto
      <$> arbitraryDirectDeclarator typeNames
      <*> arbitraryArrayOrProto typeNames
  ]

arbitraryArrayOrProto :: Set.Set Id -> QC.Gen ArrayOrProto
arbitraryArrayOrProto typeNames = halveSize $ oneOfSized $
  [ Anyhow $ Array <$> arbitraryArrayType typeNames
  , IfPositive $ Proto <$> QC.listOf (arbitraryParameterDeclaration typeNames)
  ]

arbitraryArrayType :: Set.Set Id -> QC.Gen ArrayType
arbitraryArrayType typeNames = QC.oneof
  [ return VariablySized
  , SizedByInteger . QC.getNonNegative <$> QC.arbitrary
  , SizedByIdentifier <$> arbitraryIdentifier typeNames
  , return Unsized
  ]

instance QC.Arbitrary Pointer where
  arbitrary = Pointer <$> QC.arbitrary

arbitraryParameterDeclaration :: Set.Set Id -> QC.Gen ParameterDeclaration
arbitraryParameterDeclaration typeNames = halveSize $
  ParameterDeclaration
    <$> QC.listOf1 (arbitraryDeclarationSpecifier typeNames)
    <*> QC.oneof
          [ Left <$> arbitraryDeclarator typeNames
          , Right <$> arbitraryAbstractDeclarator typeNames
          ]

arbitraryAbstractDeclarator :: Set.Set Id -> QC.Gen AbstractDeclarator
arbitraryAbstractDeclarator typeNames = halveSize $ do
  ptrs <- QC.arbitrary
  decl <- if null ptrs
    then Just <$> arbitraryDirectAbstractDeclarator typeNames
    else oneOfSized
      [ Anyhow $ return Nothing
      , IfPositive $ Just <$> arbitraryDirectAbstractDeclarator typeNames
      ]
  return $ AbstractDeclarator ptrs decl

arbitraryDirectAbstractDeclarator
  :: Set.Set Id -> QC.Gen DirectAbstractDeclarator
arbitraryDirectAbstractDeclarator typeNames = halveSize $ oneOfSized $
  [ Anyhow $ ArrayOrProtoHere <$> arbitraryArrayOrProto typeNames
  , IfPositive $ AbstractDeclaratorParens <$> arbitraryAbstractDeclarator typeNames
  , IfPositive $ ArrayOrProtoThere
      <$> arbitraryDirectAbstractDeclarator typeNames
      <*> arbitraryArrayOrProto typeNames
  ]

------------------------------------------------------------------------
-- Utils

many1 :: CParser m => m a -> m [a]
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
