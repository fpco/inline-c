{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.C.Inline.HaskellIdentifier
  ( HaskellIdentifier
  , unHaskellIdentifier
  , haskellIdentifierFromString
  , haskellCParserContext
  , parseHaskellIdentifier
  , mangleHaskellIdentifier
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (when, msum, void)
import           Data.Char (ord)
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable)
import           Data.List (intercalate, partition, intersperse)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import           Data.Typeable (Typeable)
import           Numeric (showHex)
import qualified Test.QuickCheck as QC
import           Text.Parser.Char (upper, lower, digit, char)
import           Text.Parser.Combinators (many, eof, try, unexpected, (<?>))
import           Text.Parser.Token (IdentifierStyle(..), highlight, TokenParsing)
import qualified Text.Parser.Token.Highlight as Highlight
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.C.Types.Parse as C

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<*), (<$>), (<*>))
#endif

-- | A possibly qualified Haskell identifier.
newtype HaskellIdentifier = HaskellIdentifier {unHaskellIdentifier :: String}
  deriving (Typeable, Eq, Ord, Show, Hashable)

instance IsString HaskellIdentifier where
  fromString s =
    case haskellIdentifierFromString s of
      Left err -> error $ "HaskellIdentifier fromString: invalid string " ++ s ++ ":\n" ++ err
      Right x -> x

instance PP.Pretty HaskellIdentifier where
  pretty = PP.text . unHaskellIdentifier

haskellIdentifierFromString :: String -> Either String HaskellIdentifier
haskellIdentifierFromString s =
  case C.runCParser cpc "haskellIdentifierFromString" s (parseHaskellIdentifier <* eof) of
    Left err -> Left $ show err
    Right x -> Right x
  where
    cpc = haskellCParserContext HashSet.empty

haskellCParserContext :: C.TypeNames -> C.CParserContext HaskellIdentifier
haskellCParserContext typeNames = C.CParserContext
  { C.cpcTypeNames = typeNames
  , C.cpcParseIdent = parseHaskellIdentifier
  , C.cpcIdentName = "Haskell identifier"
  , C.cpcIdentToString = unHaskellIdentifier
  }

-- | See
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2>.
haskellIdentStyle :: C.CParser i m => IdentifierStyle m
haskellIdentStyle = IdentifierStyle
  { _styleName = "Haskell identifier"
  , _styleStart = small
  , _styleLetter = small <|> large <|> digit <|> char '\''
  , _styleReserved = haskellReservedWords
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }
  where
    small = lower <|> char '_'
    large = upper

-- We disallow both Haskell reserved words and C reserved words.
haskellReservedWords :: HashSet.HashSet String
haskellReservedWords = C.cReservedWords <> HashSet.fromList
  [ "case", "class", "data", "default", "deriving", "do", "else"
  , "foreign", "if", "import", "in", "infix", "infixl"
  , "infixr", "instance", "let", "module", "newtype", "of"
  , "then", "type", "where"
  ]

-- | See
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2>.
parseHaskellIdentifier :: forall i m. C.CParser i m => m HaskellIdentifier
parseHaskellIdentifier = do
  segments <- go
  return $ HaskellIdentifier $ intercalate "." segments
  where
    small = lower <|> char '_'
    large = upper

    conid :: m String
    conid = try $ highlight Highlight.Identifier $
      ((:) <$> large <*> many (small <|> large <|> digit <|> char '\'')) <?> "Haskell constructor"

    varid :: m String
    varid = identNoLex haskellIdentStyle

    go = msum
      [ do con <- conid
           msum
             [ do void $ char '.'
                  (con :) <$> go
             , return [con]
             ]
      , do var <- varid
           return [var]
      ]

-- | Mangles an 'HaskellIdentifier' to produce a valid 'C.CIdentifier'
-- which still sort of resembles the 'HaskellIdentifier'.
mangleHaskellIdentifier :: HaskellIdentifier -> C.CIdentifier
mangleHaskellIdentifier (HaskellIdentifier hs) =
  -- The leading underscore if we have no valid chars is because then
  -- we'd have an identifier starting with numbers.
  let cs = (if null valid then "_" else "") ++
           valid ++
           (if null mangled || null valid then "" else "_") ++
           mangled
  in case C.cIdentifierFromString cs of
    Left err -> error $ "mangleHaskellIdentifier: produced bad C identifier\n" ++ err
    Right x -> x
  where
    (valid, invalid) = partition (`elem` C.cIdentLetter) hs

    mangled = concat $ intersperse "_" $ map (`showHex` "") $ map ord invalid

-- Utils
------------------------------------------------------------------------

identNoLex :: (TokenParsing m, Monad m, IsString s) => IdentifierStyle m -> m s
identNoLex s = fmap fromString $ try $ do
  name <- highlight (_styleHighlight s)
          ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
  when (HashSet.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
  return name

-- Arbitrary instance
------------------------------------------------------------------------

instance QC.Arbitrary HaskellIdentifier where
  arbitrary = do
    modIds <- QC.listOf arbitraryModId
    id_ <- QC.oneof [arbitraryConId, arbitraryVarId]
    if null modIds && HashSet.member id_ haskellReservedWords
      then QC.arbitrary
      else return $ HaskellIdentifier $ intercalate "." $ modIds ++ [id_]
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
