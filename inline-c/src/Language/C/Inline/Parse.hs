{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.C.Inline.Parse
  ( runParserInQ
  , parseTypedC
  ) where

import           Control.Applicative ((<*), (*>), (<|>))
import           Control.Monad (void, msum, when, forM_)
import           Control.Monad.Reader (runReaderT, lift)
import           Data.Either (partitionEithers)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Language.Haskell.TH as TH
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Parser.Char as Parser
import qualified Text.Parser.Combinators as Parser
import qualified Text.Parser.LookAhead as Parser
import qualified Text.Parser.Token as Parser
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.C.Types as C

runParserInQ
  :: String -> (C.Id -> Bool) -> (forall m. C.CParser m => m a) -> TH.Q a
runParserInQ s isTypeName p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let parsecLoc = Parsec.newPos (TH.loc_filename loc) line col
  let p' = lift (Parsec.setPosition parsecLoc) *> p <* lift Parser.eof
  let errOrRes = Parsec.parse  (runReaderT p' isTypeName) (TH.loc_filename loc) s
  case errOrRes of
    Left err -> do
      -- TODO consider prefixing with "error while parsing C" or similar
      error $ show err
    Right res -> do
      return res

parseTypedC
  :: forall m. C.CParser m
  => m (C.ParameterDeclaration, [C.ParameterDeclaration], String)
  -- ^ Returns the return type, the captured variables, and the body.
parseTypedC = do
  -- Parse return type (consume spaces first)
  Parser.spaces
  cType <- C.parseParameterDeclaration
  (cRetType, cParams1) <- processReturnType cType
  -- Parse the body
  void $ Parser.char '{'
  (cParams2, cBody) <- parseBody
  -- Massage the params to make sure that we're good
  cParams <- processParams $ map Right cParams1 ++ cParams2
  return (cRetType, cParams, cBody)
  where
    parseBody :: m ([Either C.Id C.ParameterDeclaration], String)
    parseBody = do
      -- Note that this code does not use "lexing" combinators (apart
      -- when appropriate) because we want to make sure to preserve
      -- whitespace after we substitute things.
      s <- Parser.manyTill Parser.anyChar $
           Parser.lookAhead (Parser.char '}' <|> Parser.char '$')
      let parseEscapedDollar = do
            void $ Parser.char '$'
            return ([], "$")
      let parseTypedCapture = do
            void $ Parser.symbolic '('
            decl <- C.parseParameterDeclaration
            s' <- case C.parameterDeclarationId decl of
              Nothing -> fail $ pretty80 $
                "Un-named captured variable in decl" <+> PP.pretty decl
              Just s' -> return s'
            void $ Parser.char ')'
            return ([Right decl], C.unId s')
      let parseCapture = do
            s' <- C.parseIdentifier
            return ([Left s'], C.unId s')
      (decls, s') <- msum
        [ do Parser.try $ do -- Try because we might fail to parse the 'eof'
               -- 'symbolic' because we want to consume whitespace
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

    processReturnType
      :: C.ParameterDeclaration -> m (C.ParameterDeclaration, [C.ParameterDeclaration])
    processReturnType decl@(C.ParameterDeclaration _ cTy) = case cTy of
      C.Proto cRetType cParams -> do
        let ids = map C.parameterDeclarationId cParams
        forM_ ids $ \mbId ->
          case mbId of
            Nothing -> fail $ "Unnamed parameter"
            Just _ -> return ()
        let dups = Set.size (Set.fromList ids) /= length ids
        when dups $
          fail "Duplicates in parameter list"
        return (C.ParameterDeclaration (C.parameterDeclarationId decl) cRetType, cParams)
      _ -> do
        return (decl, [])

    processParams
      :: [Either C.Id C.ParameterDeclaration] -> m [C.ParameterDeclaration]
    processParams cParams = do
      let (untyped, typed) = partitionEithers cParams
      params <- go Map.empty typed
      checkUntyped params untyped
      return $ map snd $ Map.toList params
      where
        go :: Map.Map C.Id C.ParameterDeclaration -> [C.ParameterDeclaration]
           -> m (Map.Map C.Id C.ParameterDeclaration)
        go visitedParams [] = do
          return visitedParams
        go visitedParams (decl : params) = do
          let id' = declId decl
          case Map.lookup id' visitedParams of
            Nothing -> go (Map.insert id' decl visitedParams) params
            Just decl' | C.parameterDeclarationType decl == C.parameterDeclarationType decl' ->
              go visitedParams params
            Just decl' ->
              fail $ pretty80 $
                "Cannot recapture" <+> PP.pretty decl <> ", since previous" <>
                "definition conflicts with new type:" <+> PP.pretty decl'

        checkUntyped
          :: Map.Map C.Id C.ParameterDeclaration -> [C.Id] -> m ()
        checkUntyped params = mapM_ $ \s -> do
          case Map.lookup s params of
            Just _ -> return ()
            Nothing -> do
              fail $ "Untyped variable `" ++ pretty80 s ++ "'"

        declId decl = case C.parameterDeclarationId decl of
          Nothing -> error "processParams: no declaration id"
          Just id' -> id'

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""
