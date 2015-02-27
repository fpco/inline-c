{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Language.C.Inline.Parse (runParserInQ, parseTypedC) where

import           Control.Applicative ((<*), (<|>))
import           Control.Monad (void, msum)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Text.Trifecta as Trifecta
import qualified Text.Trifecta.Delta as Trifecta
import qualified Text.Parser.LookAhead as Trifecta
import           Data.Monoid ((<>))
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Either (partitionEithers)

import qualified Language.C.Types as C

-- Parsing

runParserInQ :: String -> Trifecta.Parser a -> TH.Q a
runParserInQ s p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let d = Trifecta.Directed
        (UTF8.fromString (TH.loc_filename loc)) (fromIntegral line)
        (fromIntegral col) 0 0
  case Trifecta.parseString (p <* Trifecta.eof) d s of
    Trifecta.Failure err -> do
      -- TODO consider prefixing with "error while parsing C" or similar
      error $ pretty80 err
    Trifecta.Success res -> do
      return res

parseTypedC
  :: Trifecta.Parser (C.Declaration (), [C.Declaration C.Id], String)
  -- ^ Returns the retun type, the captured variables, and the body.
parseTypedC = do
  -- Parse return type
  cRetType <- C.parseAbstractDeclaration
  -- Parse the body
  void $ Trifecta.char '{'
  (cParams, cBody) <- parseBody
  -- Massage the params to make sure that we're good
  cParams' <- processParams cParams
  return (cRetType, cParams', cBody)
  where
    parseBody :: Trifecta.Parser ([Either C.Id (C.Declaration C.Id)], String)
    parseBody = do
      -- Note that this code does not work with "lexing" combinators
      -- (apart when appropriate) because we want to make sure to
      -- preserve whitespace after the things we substitute.
      s <- Trifecta.manyTill Trifecta.anyChar $
           Trifecta.lookAhead (Trifecta.char '}' <|> Trifecta.char '$')
      let parseEscapedDollar = do
            void $ Trifecta.char '$'
            return ([], "$")
      let parseTypedCapture = do
            void $ Trifecta.symbolic '('
            decl@(C.Declaration s' _ _) <- C.parseDeclaration
            void $ Trifecta.char ')'
            return ([Right decl], s')
      let parseCapture = do
            s' <- C.parseIdentifier
            return ([Left s'], s')
      (decls, s') <- msum
        [ do Trifecta.try $ do -- Try because we might fail to parse the 'eof'
               void $ Trifecta.symbolic '}'
               Trifecta.eof
             return ([], "")
        , do void $ Trifecta.char '}'
             (decls, s') <- parseBody
             return (decls, "}" ++ s')
        , do void $ Trifecta.char '$'
             (decls1, s1) <- parseEscapedDollar <|> parseTypedCapture <|> parseCapture
             (decls2, s2) <- parseBody
             return (decls1 ++ decls2, s1 ++ s2)
        ]
      return (decls, s ++ s')

    processParams
      :: [Either C.Id (C.Declaration C.Id)]
      -> Trifecta.Parser [C.Declaration C.Id]
    processParams cParams = do
      let (untyped, typed) = partitionEithers cParams
      params <- go Map.empty typed
      checkUntyped params untyped
      return $ map snd $ Map.toList params
      where
        go :: Map.Map C.Id (C.Declaration C.Id) -> [C.Declaration C.Id]
           -> Trifecta.Parser (Map.Map C.Id (C.Declaration C.Id))
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
          :: Map.Map C.Id (C.Declaration C.Id) -> [C.Id] -> Trifecta.Parser ()
        checkUntyped params = mapM_ $ \s -> do
          case Map.lookup s params of
            Just _ -> return ()
            Nothing -> do
              fail $ "Untyped variable `" ++ s ++ "'"

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""
