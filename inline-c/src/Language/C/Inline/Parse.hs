{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Language.C.Inline.Parse (runParserInQ, parseTypedC) where

import           Control.Applicative ((*>), (<*))
import           Control.Monad (void)
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.UTF8
import           Data.Char (isSpace)
import           Data.Data (Data)
import           Data.Loc (Pos(..), locOf)
import qualified Data.Map as Map
import           Data.Typeable (Typeable, (:~:)(..), eqT)
import           Generics.SYB (everywhereM)
import qualified Language.C as C
import qualified Language.Haskell.TH as TH
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.PrettyPrint.Mainland as PrettyPrint

import           Language.C.Inline.Context

-- Parsing

runParserInQ :: String -> Parsec.Parser a -> TH.Q a
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

parseC
  :: Context -> Parsec.SourcePos -> String -> C.P a -> a
parseC context parsecPos str p =
  let pos = Pos
        (Parsec.sourceName parsecPos)
        (Parsec.sourceLine parsecPos)
        (Parsec.sourceColumn parsecPos)
        0
      bytes = Data.ByteString.UTF8.fromString str
      -- TODO support user-defined C types.
      pstate = C.emptyPState [C.Antiquotation] (ctxCTypes context) bytes pos
  in case C.evalP p pstate of
    Left err ->
      -- TODO consider prefixing with "error while parsing C" or similar
      error $ show err
    Right x ->
      x

-- Note that we split the input this way because we cannot compose Happy
-- parsers easily.
parseTypedC
  :: (Data a)
  => Context
  -> C.P a
  -- ^ Parser to parse the body of the typed expression
  -> Parsec.Parser (C.Type, [(C.Id, C.Type)], a)
parseTypedC context p = do
  -- Get stuff up to parens or brace, and parse it
  typePos <- Parsec.getPosition
  typeStr <- takeTillChar '{'
  let cType = parseC context typePos typeStr C.parseType
  let (cRetType, cParams) = processType cType
  -- Get the body, and feed it to the given parser
  bodyPos <- Parsec.getPosition
  bodyStr <- restOfInputNoBrace
  let cBody = parseC context bodyPos bodyStr p
  -- Collect the implicit parameters present in the body
  let paramsMap = mkParamsMap $ map cleanupParam $ cParams
  let (cBody', paramsMap') = State.runState (collectImplParams cBody) paramsMap
  -- Whew
  return (cRetType, Map.toList paramsMap', cBody')
  where
    lex_ :: Parsec.Parser b -> Parsec.Parser ()
    lex_ p' = do
      void p'
      Parsec.spaces

    takeTillChar :: Char -> Parsec.Parser String
    takeTillChar ch = do
      str <- Parsec.many $ Parsec.satisfy (/= ch)
      lex_ $ Parsec.char ch
      return str

    restOfInputNoBrace :: Parsec.Parser String
    restOfInputNoBrace = do
      inp <- Parsec.many1 $ Parsec.satisfy $ \_ -> True
      let revInp = dropWhile isSpace $ reverse inp
      case revInp of
        '}' : revInp' -> return $ reverse revInp'
        _ -> fail $ "No closing }!"

    processType :: C.Type -> (C.Type, [C.Param])
    processType cTy = case cTy of
      C.Type (C.DeclSpec storage quals cTySpec specSrc) decl0 typeSrc ->
        go decl0 $ \decl' ->
          C.Type (C.DeclSpec storage quals cTySpec specSrc) decl' typeSrc
      _ ->
        error "inline-c: got antiquotation (processType)"
      where
        -- We stop at the first proto not preceded by a Ptr.  TODO check
        -- if this is actually a good heuristic.
        go :: C.Decl -> (C.Decl -> C.Type) -> (C.Type, [C.Param])
        go decl0 cont = case decl0 of
          C.DeclRoot rootSrc ->
            (cont (C.DeclRoot rootSrc), [])
          C.Proto decl (C.Params params _ _) _ ->
            (cont decl, params)
          C.OldProto decl [] _ ->
            (cont decl, [])
          C.OldProto _ _ _ ->
            error "inline-c: Old prototypes not supported (processTye)"
          -- If we get a function pointers, we consider it part of the
          -- return type
          C.Ptr quals (C.Proto decl params protoSrc) ptrSrc ->
            go decl $ \decl' -> cont $ C.Ptr quals (C.Proto decl' params protoSrc) ptrSrc
          C.Ptr quals (C.OldProto decl pars protoSrc) ptrSrc ->
            go decl $ \decl' -> cont $ C.Ptr quals (C.OldProto decl' pars protoSrc) ptrSrc
          C.Ptr quals decl ptrSrc ->
            go decl $ \decl' -> cont $ C.Ptr quals decl' ptrSrc
          C.Array quals size decl arraySrc ->
            go decl $ \decl' -> cont $ C.Array quals size decl' arraySrc
          C.BlockPtr quals decl blockPtrSrc ->
            go decl $ \decl' -> cont $ C.BlockPtr quals decl' blockPtrSrc
          C.AntiTypeDecl{} ->
            error "inline-c: got antiquotation (processType)"

    cleanupParam :: C.Param -> (C.Id, C.Type)
    cleanupParam param = case param of
      C.Param Nothing _ _ _        -> error "inline-c: Cannot capture Haskell variable if you don't give a name."
      C.Param (Just name) ds d loc -> (name, C.Type ds d loc)
      _                            -> error "inline-c: got antiquotation (parseTypedC)"

    mkParamsMap :: [(C.Id, C.Type)] -> Map.Map C.Id C.Type
    mkParamsMap cParams =
      let m = Map.fromList cParams
      in if Map.size m == length cParams
           then m
           else error "inline-c: Duplicated variable in parameter list"

    collectImplParams = everywhereM (typeableAppM collectImplParam)

    collectImplParam :: C.Id -> State.State (Map.Map C.Id C.Type) C.Id
    collectImplParam name0@(C.Id str loc) = do
      case ctxGetSuffixType context str of
        Nothing -> return name0
        Just (str', type_) -> do
          let name = C.Id str' loc
          m <- State.get
          case Map.lookup name m of
            Nothing -> do
              State.put $ Map.insert name type_ m
              return name
            Just type' | type_ == type' -> do
              return name
            Just type' -> do
              let prevName = head $ dropWhile (/= name) $ Map.keys m
              error $
                "Cannot recapture variable " ++ pretty80 name ++
                " to have type " ++ pretty80 type_ ++ ".\n" ++
                "Previous redefinition of type " ++ pretty80 type' ++
                " at " ++ show (locOf prevName) ++ "."
    collectImplParam (C.AntiId _ _) = do
      error "inline-c: got antiquotation (collectImplParam)"

------------------------------------------------------------------------
-- Utils

pretty80 :: PrettyPrint.Pretty a => a -> String
pretty80 = PrettyPrint.pretty 80 . PrettyPrint.ppr

-- TODO doesn't something of this kind exist?
typeableAppM
  :: forall a b m. (Typeable a, Typeable b, Monad m) => (b -> m b) -> a -> m a
typeableAppM = help eqT
  where
    help :: Maybe (a :~: b) -> (b -> m b) -> a -> m a
    help (Just Refl) f x = f x
    help Nothing     _ x = return x
