{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Language.C.Inline.Parse
  ( runParserInQ
  , SomeEq
  , toSomeEq
  , fromSomeEq
  , ParameterType(..)
  , ParseTypedC(..)
  , parseTypedC
  ) where

import           Control.Applicative ((<*), (*>), (<|>))
import           Control.Monad (void, msum, when, forM)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
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
import           Control.Monad.State (evalStateT, StateT, get, put)
import           Data.Typeable (Typeable, cast)

import qualified Language.C.Types as C
import           Language.C.Inline.Context

runParserInQ
  :: String -> (C.Id -> Bool) -> (forall m. C.CParser m => m a) -> TH.Q a
runParserInQ s isTypeName' p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let parsecLoc = Parsec.newPos (TH.loc_filename loc) line col
  let p' = lift (Parsec.setPosition parsecLoc) *> p <* lift Parser.eof
  case C.runCParser isTypeName' (TH.loc_filename loc) s p' of
    Left err -> do
      -- TODO consider prefixing with "error while parsing C" or similar
      error $ show err
    Right res -> do
      return res

data SomeEq = forall a. (Typeable a, Eq a) => SomeEq a

instance Eq SomeEq where
  SomeEq x == SomeEq y = case cast x of
    Nothing -> False
    Just x' -> x' == y

instance Show SomeEq where
  show _ = "<<SomeEq>>"

toSomeEq :: (Eq a, Typeable a) => a -> SomeEq
toSomeEq x = SomeEq x

fromSomeEq :: (Eq a, Typeable a) => SomeEq -> Maybe a
fromSomeEq (SomeEq x) = cast x

data ParameterType
  = Plain String                -- The name of the captured variable
  | AntiQuote CAntiQuoterId SomeEq
  deriving (Show, Eq)

data ParseTypedC = ParseTypedC
  { ptcReturnType :: C.Type
  , ptcParameters :: [(C.Id, C.Type, ParameterType)]
  , ptcBody :: String
  }

parseTypedC
  :: forall m. C.CParser m
  => CAntiQuoters -> m ParseTypedC
  -- ^ Returns the return type, the captured variables, and the body.
parseTypedC antiQs = do
  -- Parse return type (consume spaces first)
  Parser.spaces
  cType <- C.parseParameterDeclaration
  (cRetType, cParams1) <- processReturnType cType
  -- Parse the body
  void $ Parser.char '{'
  (cParams2, cBody) <- evalStateT parseBody 0
  let cParams = map (\(cId, cTy) -> (cId, cTy, Plain (C.unId cId))) cParams1 ++ cParams2
  return $ ParseTypedC cRetType cParams cBody
  where
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

    parseBody :: StateT Int m ([(C.Id, C.Type, ParameterType)], String)
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
              Just id' -> return $ C.unId id'
            id' <- freshId s'
            void $ Parser.char ')'
            return ([(id', C.parameterDeclarationType decl, Plain s')], C.unId id')
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
             (decls1, s1) <- parseEscapedDollar <|> parseAntiQuote <|> parseTypedCapture
             (decls2, s2) <- parseBody
             return (decls1 ++ decls2, s1 ++ s2)
        ]
      return (decls, s ++ s')
      where

    parseAntiQuote :: StateT Int m ([(C.Id, C.Type, ParameterType)], String)
    parseAntiQuote = msum
      [ do void $ Parser.try (Parser.string $ antiQId ++ ":") Parser.<?> "anti quoter id"
           (s, cTy, x) <- caqParser antiQ
           id' <- freshId s
           return ([(id', cTy, AntiQuote antiQId (toSomeEq x))], C.unId id')
      | (antiQId, SomeCAntiQuoter antiQ) <- Map.toList antiQs
      ]

    freshId s = do
      c <- get
      put $ c + 1
      return $ C.Id $ s ++ "_inline_c_" ++ show c

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""
