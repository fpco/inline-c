module Language.C.Types.Lex
  ( Identifier
  , Token(..)
  , lexC
  , lexToken
  , prettyToken
  ) where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor ((<$>), (<$))
import           Control.Monad (msum)

data Token
  -- Symbols
  = LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | PTR
  | COMMA
  -- Storage qualifiers
  | CONST
  -- Type qualifiers
  | SIGNED
  | UNSIGNED
  -- Primitive types
  | CHAR
  | SHORT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  | VOID
  -- Identifiers
  | IDENTIFIER Identifier
  -- Integer constants (array sizes)
  | INTEGER Integer
  deriving (Eq, Show)

type Identifier = String

lexC :: Parser [(SourcePos, Token)]
lexC = many lexToken

lexToken :: Parser (SourcePos, Token)
lexToken = do
  pos <- getPosition
  tok <- parseTok <?> "token"
  spaces
  return (pos, tok)
  where
    parseTok = msum
      [ LBRACK <$ char '['
      , RBRACK <$ char ']'
      , LPAREN <$ char '('
      , RPAREN <$ char ')'
      , PTR <$ char '*'
      , COMMA <$ char ','
      , CONST <$ try (string "const")
      , SIGNED <$ try (string "signed")
      , UNSIGNED <$ try (string "signed")
      , CHAR <$ try (string "char")
      , SHORT <$ try (string "short")
      , INT <$ try (string "int")
      , LONG <$ try (string "long")
      , FLOAT <$ try (string "float")
      , DOUBLE <$ try (string "double")
      , VOID <$ try (string "void")
      , IDENTIFIER <$> try parseIdent
      , INTEGER <$> try parseInteger
      ]

    parseIdent = do
      ch <- identLetter
      chs <- many $ identLetter <|> digit_
      return $ ch : chs

    identLetter = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    digit_ = oneOf ['0'..'9']

    parseInteger = read <$> many1 digit_

prettyToken :: Token -> String
prettyToken tok = case tok of
  LBRACK -> "["
  RBRACK -> "]"
  LPAREN -> "("
  RPAREN -> ")"
  PTR -> "*"
  CONST -> "const"
  SIGNED -> "signed"
  UNSIGNED -> "signed"
  CHAR -> "char"
  SHORT -> "short"
  INT -> "int"
  LONG -> "long"
  FLOAT -> "float"
  DOUBLE -> "double"
  VOID -> "void"
  IDENTIFIER s -> s
  INTEGER i -> show i
