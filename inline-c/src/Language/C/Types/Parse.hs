{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.C.Types.Parse
  ( CParser

    -- * Types
  , Id(..)
  , DeclarationSpecifier(..)
  , StorageClassSpecifier(..)
  , TypeSpecifier(..)
  , TypeQualifier(..)
  , FunctionSpecifier(..)
  , Declarator(..)
  , DirectDeclarator(..)
  , ArrayOrProto(..)
  , ArrayType(..)
  , Pointer(..)
  , ParameterDeclaration(..)
  , AbstractDeclarator(..)
  , DirectAbstractDeclarator(..)

    -- * Parsing
  , parseIdentifier
  , parseParameterDeclaration
  , parseParameterList
  ) where

import           Control.Applicative (Applicative, (<*>), (<|>))
import           Control.Monad (msum, void, MonadPlus, unless, when)
import           Control.Monad.Logic.Class ((>>-))
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import           Data.Functor ((<$>), (<$))
import qualified Data.HashSet as HashSet
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Test.QuickCheck as QC
import           Test.SmallCheck.Series ((\/), (<~>))
import qualified Test.SmallCheck.Series as SC
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Parser.LookAhead
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..), (<+>), Doc, hsep)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

------------------------------------------------------------------------
-- Parser

type CParser m = (Monad m, Functor m, Applicative m, MonadPlus m, Parsing m, CharParsing m, TokenParsing m, LookAheadParsing m, MonadReader (Id -> Bool) m)

newtype Id = Id {unId :: String}
  deriving (Eq, Ord, Show)

instance IsString Id where
  fromString s =
    let res = Parsec.parse (runReaderT parseIdentifier (const False)) "fromString" s
    in case res of
      Left _err -> error $ "Id fromString: invalid string " ++ show s
      Right x -> x

identLetter :: CParser m => m Char
identLetter = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

identStyle :: CParser m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "C identifier"
  , _styleStart = identLetter
  , _styleLetter = identLetter <|> digit
  , _styleReserved = HashSet.fromList
      [ "auto", "else", "long", "switch"
      , "break", "enum", "register", "typedef"
      , "case", "extern", "return", "union"
      , "char", "float", "short", "unsigned"
      , "const", "for", "signed", "void"
      , "continue", "goto", "sizeof", "volatile"
      , "default", "if", "static", "while"
      , "do", "int", "struct", "double"
      ]
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

data DeclarationSpecifier
  = StorageClassSpecifier StorageClassSpecifier
  | TypeSpecifier TypeSpecifier
  | TypeQualifier TypeQualifier
  | FunctionSpecifier FunctionSpecifier
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

type_qualifier :: CParser m => m TypeQualifier
type_qualifier = msum
  [ CONST <$ reserve identStyle "const"
  , RESTRICT <$ reserve identStyle "restrict"
  , VOLATILE <$ reserve identStyle "volatile"
  ]

data FunctionSpecifier
  = INLINE
  deriving (Eq, Show)

function_specifier :: CParser m => m FunctionSpecifier
function_specifier = msum
  [ INLINE <$ reserve identStyle "inline"
  ]

data Declarator = Declarator
  { declaratorPointers :: [Pointer]
  , declaratorDirect :: DirectDeclarator
  } deriving (Eq, Show)

declarator :: CParser m => m Declarator
declarator = (Declarator <$> many pointer <*> direct_declarator) <?> "declarator"

data DirectDeclarator
  = DeclaratorRoot Id
  | ArrayOrProto DirectDeclarator ArrayOrProto
  | DeclaratorParens Declarator
  deriving (Eq, Show)

data ArrayOrProto
  = Array ArrayType
  | Proto [ParameterDeclaration] -- We don't include old prototypes.
  deriving (Eq, Show)

-- TODO handle more stuff in array brackets
data ArrayType
  = VariablySized
  | Unsized
  | SizedByInteger Integer
  | SizedByIdentifier Id
  deriving (Eq, Show)

direct_declarator :: CParser m => m DirectDeclarator
direct_declarator =
  (do ddecltor <- msum
        [ DeclaratorRoot <$> identifier
        , DeclaratorParens <$> parens declarator
        ]
      aops <- many array_or_proto
      return $ foldl ArrayOrProto ddecltor aops)

array_or_proto :: CParser m => m ArrayOrProto
array_or_proto = msum
  [ Array <$> brackets array_type
  , Proto <$> parens parameter_list
  ]

array_type :: CParser m => m ArrayType
array_type = msum
  [ VariablySized <$ symbolic '*'
  , SizedByInteger <$> natural
  , SizedByIdentifier <$> identifier
  , return Unsized
  ]

data Pointer
  = Pointer [TypeQualifier]
  deriving (Eq, Show)

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
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

abstract_declarator :: CParser m => m AbstractDeclarator
abstract_declarator = do
  ptrs <- many pointer
  -- If there are no pointers, there must be an abstract declarator.
  let p = if null ptrs
        then Just <$> direct_abstract_declarator
        else Just <$> try direct_abstract_declarator <|>
             return Nothing <?>
             "direct abstract declarator"
  AbstractDeclarator ptrs <$> p

data DirectAbstractDeclarator
  = ArrayOrProtoHere ArrayOrProto
  | ArrayOrProtoThere DirectAbstractDeclarator ArrayOrProto
  | ParensAbstractDeclarator AbstractDeclarator
  deriving (Eq, Show)

direct_abstract_declarator :: CParser m => m DirectAbstractDeclarator
direct_abstract_declarator =
  (do ddecltor <- msum
        [ try (ArrayOrProtoHere <$> array_or_proto)
        , ParensAbstractDeclarator <$> parens abstract_declarator
        ] <?> "array, prototype, or parenthesised abstract declarator"
      aops <- many array_or_proto
      return $ foldl ArrayOrProtoThere ddecltor aops)

------------------------------------------------------------------------
-- Parse exports

-- | This parser parses an 'Id' and nothing else -- it does not consume
-- trailing spaces and the like.
parseIdentifier :: CParser m => m Id
parseIdentifier =
  try (do s <- Id <$> ((:) <$> identLetter <*> many (identLetter <|> digit))
          isTypeName <- ask
          when (isTypeName s) $
            fail "expecting identifier, got type name"
          return s)
  <?> "identifier"

parseParameterDeclaration :: CParser m => m ParameterDeclaration
parseParameterDeclaration = parameter_declaration

parseParameterList :: CParser m => m [ParameterDeclaration]
parseParameterList = parameter_list

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
    ParensAbstractDeclarator x -> "(" <> pretty x <> ")"
    ArrayOrProtoHere aop -> pretty aop
    ArrayOrProtoThere ddecltor' aop -> pretty ddecltor' <> pretty aop

------------------------------------------------------------------------
-- Arbitrary

instance QC.Arbitrary Id where
  arbitrary =
      Id <$> ((:) <$> QC.oneof letters <*> QC.listOf (QC.oneof (letters ++ digits)))
    where
      letters = map return $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
      digits = map return ['0'..'9']

instance QC.Arbitrary DeclarationSpecifier where
  arbitrary = QC.oneof
    [ StorageClassSpecifier <$> QC.arbitrary
    , TypeSpecifier <$> QC.arbitrary
    , TypeQualifier <$> QC.arbitrary
    , FunctionSpecifier <$> QC.arbitrary
    ]

instance QC.Arbitrary StorageClassSpecifier where
  arbitrary = QC.oneof
    [ return TYPEDEF
    , return EXTERN
    , return STATIC
    , return AUTO
    , return REGISTER
    ]

instance QC.Arbitrary TypeSpecifier where
  arbitrary = QC.oneof
    [ return VOID
    , return CHAR
    , return SHORT
    , return INT
    , return LONG
    , return FLOAT
    , return DOUBLE
    , return SIGNED
    , return UNSIGNED
    , Struct <$> QC.arbitrary
    , Enum <$> QC.arbitrary
    ]

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

instance QC.Arbitrary Declarator where
  arbitrary = Declarator <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary DirectDeclarator where
  arbitrary = QC.oneof
    [ DeclaratorRoot <$> QC.arbitrary
    , DeclaratorParens <$> QC.arbitrary
    , ArrayOrProto <$> QC.arbitrary <*> QC.arbitrary
    ]

instance QC.Arbitrary ArrayOrProto where
  arbitrary = QC.oneof
    [ Array <$> QC.arbitrary
    , Proto <$> QC.arbitrary
    ]

instance QC.Arbitrary ArrayType where
  arbitrary = QC.oneof
    [ return VariablySized
    , SizedByInteger . QC.getNonNegative <$> QC.arbitrary
    , SizedByIdentifier <$> QC.arbitrary
    , return Unsized
    ]

instance QC.Arbitrary Pointer where
  arbitrary = Pointer <$> QC.arbitrary

instance QC.Arbitrary ParameterDeclaration where
  arbitrary = ParameterDeclaration <$> QC.listOf1 QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary AbstractDeclarator where
  arbitrary = do
    ptrs <- QC.arbitrary
    let g = if null ptrs
          then Just <$> QC.arbitrary
          else QC.arbitrary
    AbstractDeclarator ptrs <$> g

instance QC.Arbitrary DirectAbstractDeclarator where
  arbitrary = QC.oneof
    [ ParensAbstractDeclarator <$> QC.arbitrary
    , ArrayOrProtoHere <$> QC.arbitrary
    , ArrayOrProtoThere <$> QC.arbitrary <*> QC.arbitrary
    ]

------------------------------------------------------------------------
-- Serial

instance Monad m => SC.Serial m Id where
  series = Id <$> idStart
    where
      idLetter =
        foldr1 (\/) $ map SC.cons0 $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
      idAlphaNum =
        foldr1 (\/) $ map SC.cons0 $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']

      idStart = SC.decDepth ((:) <$> idLetter <~> idRest)

      idRest =
        SC.cons0 [] \/
        SC.decDepth ((:) <$> idAlphaNum <~> idRest)

instance Monad m => SC.Serial m DeclarationSpecifier where
  series =
    SC.cons1 StorageClassSpecifier \/
    SC.cons1 TypeSpecifier \/
    SC.cons1 TypeQualifier \/
    SC.cons1 FunctionSpecifier

instance Monad m => SC.Serial m StorageClassSpecifier where
  series =
    SC.cons0 TYPEDEF \/
    SC.cons0 EXTERN \/
    SC.cons0 STATIC \/
    SC.cons0 AUTO \/
    SC.cons0 REGISTER

instance Monad m => SC.Serial m TypeSpecifier where
  series =
    SC.cons0 VOID \/
    SC.cons0 CHAR \/
    SC.cons0 SHORT \/
    SC.cons0 INT \/
    SC.cons0 LONG \/
    SC.cons0 FLOAT \/
    SC.cons0 DOUBLE \/
    SC.cons0 SIGNED \/
    SC.cons0 UNSIGNED \/
    SC.cons1 Struct \/
    SC.cons1 Enum

instance Monad m => SC.Serial m TypeQualifier where
  series =
    SC.cons0 CONST \/
    SC.cons0 RESTRICT \/
    SC.cons0 VOLATILE

instance Monad m => SC.Serial m FunctionSpecifier where
  series =
    SC.cons0 INLINE

instance Monad m => SC.Serial m Declarator where
  series =
    SC.cons2 Declarator

instance Monad m => SC.Serial m DirectDeclarator where
  series =
    SC.cons1 DeclaratorRoot \/
    SC.cons1 DeclaratorParens \/
    SC.cons2 ArrayOrProto

instance Monad m => SC.Serial m ArrayOrProto where
  series =
    SC.cons1 Array \/
    SC.cons1 Proto

instance Monad m => SC.Serial m ArrayType where
  series =
    SC.cons0 VariablySized \/
    SC.cons0 Unsized \/
    SC.decDepth (SizedByInteger . SC.getNonNegative <$> SC.series) \/
    SC.cons1 SizedByIdentifier

instance Monad m => SC.Serial m Pointer where
  series =
    SC.cons1 Pointer

instance Monad m => SC.Serial m ParameterDeclaration where
  series = SC.decDepth $ do
    SC.series >>- \(SC.NonEmpty specs) -> do
      ParameterDeclaration specs <$> SC.series

instance Monad m => SC.Serial m AbstractDeclarator where
  series = SC.decDepth $ do
    SC.series >>- \ptrs -> do
      let g = if null ptrs
            then Just <$> SC.series
            else SC.series
      AbstractDeclarator ptrs <$> g

instance Monad m => SC.Serial m DirectAbstractDeclarator where
  series =
    SC.cons1 ParensAbstractDeclarator \/
    SC.cons1 ArrayOrProtoHere \/
    SC.cons2 ArrayOrProtoThere

{-
------------------------------------------------------------------------
-- Tests

tests :: Hspec.SpecWith ()
tests = do
  Hspec.describe "parsing" $ do
    Hspec.it "parses everything which is pretty-printable" $ do
      SC.property $ \ty ->
        ty == assertParse (const False) parameter_declaration (prettyOneLine ty)

------------------------------------------------------------------------
-- Utils

many1 :: CParser m => m a -> m [a]
many1 p = (:) <$> p <*> many p

prettyOneLine :: PP.Pretty a => a -> String
prettyOneLine x = PP.displayS (PP.renderCompact (PP.pretty x)) ""

assertParse :: (Id -> Bool) -> (forall m. CParser m => m a) -> String -> a
assertParse isTypeName p s =
  let run x = Parsec.parse (runReaderT x isTypeName) "test" s
  in case run (lift spaces *> p <* lift eof) of
    Left err -> error $ "Parse error (assertParse): " ++ show err
    Right x -> x
-}

------------------------------------------------------------------------
-- Utils

many1 :: CParser m => m a -> m [a]
many1 p = (:) <$> p <*> many p
