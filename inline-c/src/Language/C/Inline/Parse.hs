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
import           Control.Monad (void, msum, when, forM_, forM)
import           Control.Monad.Reader (runReaderT, lift)
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
import           Control.Lens (_1, _2, (%=), over)
import           Control.Monad.State (execState)

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
  => m (C.Type, [(C.Id, C.Type)], String)
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
  cParams <- processParams $ map (over _2 Just) cParams1 ++ cParams2
  return (cRetType, cParams, cBody)
  where
    parseBody :: m ([(C.Id, Maybe C.Type)], String)
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
            return ([(s', Just (C.parameterDeclarationType decl))], C.unId s')
      let parseCapture = do
            s' <- C.parseIdentifier
            return ([(s', Nothing)], C.unId s')
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
      :: C.ParameterDeclaration -> m (C.Type, [(C.Id, C.Type)])
    processReturnType (C.ParameterDeclaration _ cTy) = case cTy of
      C.Proto cRetType cParams -> do
        cParams' <- forM cParams $ \param -> do
          id' <- case C.parameterDeclarationId param of
            Nothing -> fail $ pretty80 $
              "Unnamed parameter" <+> PP.pretty param
            Just id' -> return id'
          return (id', C.parameterDeclarationType param)
        let dups = Set.size (Set.fromList (map fst cParams')) /= length cParams'
        when dups $
          fail "Duplicates in parameter list"
        return (cRetType, cParams')
      _ -> do
        return (cTy, [])

    processParams :: [(C.Id, Maybe C.Type)] -> m [(C.Id, C.Type)]
    processParams cParams = do
      let (untyped, typed) = flip execState ([], []) $ do
            forM_ (reverse cParams) $ \(id', mbTy) -> do
              case mbTy of
                Nothing -> _1 %= (id' :)
                Just ty -> _2 %= ((id', ty) :)
      params <- go Map.empty typed
      checkUntyped params untyped
      return $ Map.toList params
      where
        go :: Map.Map C.Id C.Type -> [(C.Id, C.Type)] -> m (Map.Map C.Id C.Type)
        go visitedParams [] = do
          return visitedParams
        go visitedParams ((id', ty) : params) = do
          case Map.lookup id' visitedParams of
            Nothing -> go (Map.insert id' ty visitedParams) params
            Just ty' | ty == ty' ->
              go visitedParams params
            Just ty' ->
              fail $ pretty80 $
                "Cannot recapture variable" <+> PP.pretty id' <+>
                "to have type" <+> PP.pretty ty' <> ", since previous" <>
                "type" <+> PP.pretty ty <+> "is different."

        checkUntyped
          :: Map.Map C.Id C.Type -> [C.Id] -> m ()
        checkUntyped params = mapM_ $ \id' -> do
          case Map.lookup id' params of
            Just _ -> return ()
            Nothing -> do
              fail $ "Untyped variable `" ++ pretty80 id' ++ "'"

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""
