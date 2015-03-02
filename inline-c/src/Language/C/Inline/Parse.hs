{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Language.C.Inline.Parse (runParserInQ, parseTypedC) where

import           Control.Applicative ((<*), (*>), (<|>))
import           Control.Monad (void, msum)
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Text.Parser.Char as Parser
import qualified Text.Parser.LookAhead as Parser
import qualified Text.Parser.Combinators as Parser
import qualified Text.Parser.Token as Parser
import           Text.Parsec.String (Parser)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import           Data.Monoid ((<>))
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Either (partitionEithers)

import qualified Language.C.Types as C

-- Parsing

runParserInQ :: String -> Parser a -> TH.Q a
runParserInQ s p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let parsecLoc = Parsec.newPos (TH.loc_filename loc) line col
  let p' = Parsec.setPosition parsecLoc *> p <* Parsec.eof
  let errOrRes = Parsec.runParser  p' () (TH.loc_filename loc) s
  case errOrRes of
    Left err -> do
      -- TODO consider prefixing with "error while parsing C" or similar
      error $ show err
    Right res -> do
      return res

parseTypedC
  :: Parser (C.Declaration (), [C.Declaration C.Id], String)
  -- ^ Returns the retun type, the captured variables, and the body.
parseTypedC = do
  -- Parse return type (consume spaces first)
  Parser.spaces
  cRetType <- C.parseAbstractDeclaration
  -- Parse the body
  void $ Parser.char '{'
  (cParams, cBody) <- parseBody
  -- Massage the params to make sure that we're good
  cParams' <- processParams cParams
  return (cRetType, cParams', cBody)
  where
    parseBody :: Parser ([Either C.Id (C.Declaration C.Id)], String)
    parseBody = do
      -- Note that this code does not work with "lexing" combinators
      -- (apart when appropriate) because we want to make sure to
      -- preserve whitespace after the things we substitute.
      s <- Parser.manyTill Parser.anyChar $
           Parser.lookAhead (Parser.char '}' <|> Parser.char '$')
      let parseEscapedDollar = do
            void $ Parser.char '$'
            return ([], "$")
      let parseTypedCapture = do
            void $ Parser.symbolic '('
            decl@(C.Declaration s' _ _) <- C.parseDeclaration
            void $ Parser.char ')'
            return ([Right decl], s')
      let parseCapture = do
            s' <- C.parseIdentifier
            return ([Left s'], s')
      (decls, s') <- msum
        [ do Parser.try $ do -- Try because we might fail to parse the 'eof'
               void $ Parser.symbolic '}'
               Parser.eof
             return ([], "")
        , do void $ Parser.char '}'
             (decls, s') <- parseBody
             return (decls, "}" ++ s')
        , do void $ Parser.char '$'
             (decls1, s1) <- parseEscapedDollar <|> parseTypedCapture <|> parseCapture
             (decls2, s2) <- parseBody
             return (decls1 ++ decls2, s1 ++ s2)
        ]
      return (decls, s ++ s')

    processParams
      :: [Either C.Id (C.Declaration C.Id)]
      -> Parser [C.Declaration C.Id]
    processParams cParams = do
      let (untyped, typed) = partitionEithers cParams
      params <- go Map.empty typed
      checkUntyped params untyped
      return $ map snd $ Map.toList params
      where
        go :: Map.Map C.Id (C.Declaration C.Id) -> [C.Declaration C.Id]
           -> Parser (Map.Map C.Id (C.Declaration C.Id))
        go visitedParams [] = do
          return visitedParams
        go visitedParams (decl@(C.Declaration s quals ty) : params) = do
          case Map.lookup s visitedParams of
            Nothing -> go (Map.insert s decl visitedParams) params
            Just (C.Declaration _ quals' ty') | (quals, ty) == (quals', ty') ->
              go visitedParams params
            Just decl' ->
              fail $ pretty80 $
                "Cannot recapture" <+> PP.pretty decl <> ", since previous" <>
                "definition conflicts with new type:" <+> PP.pretty decl'

        checkUntyped
          :: Map.Map C.Id (C.Declaration C.Id) -> [C.Id] -> Parser ()
        checkUntyped params = mapM_ $ \s -> do
          case Map.lookup s params of
            Just _ -> return ()
            Nothing -> do
              fail $ "Untyped variable `" ++ s ++ "'"

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""
