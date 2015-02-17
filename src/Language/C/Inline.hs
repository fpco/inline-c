{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Language.C.Inline
    ( -- * Build process
      -- $building
      module Language.C.Context
    , setContext
      -- * Manual handling
    , Code(..)
      -- ** Emitting
      -- $emitting
    , emitLiteral
    , emitInclude
    , emitCode
      -- ** Embedding
      -- $embedding
    , embedCode
    , embedStm
    , embedExp
    , embedItems
      -- * Direct quoting
      -- $quoting
    , cstm
    , cstm_unsafe
    , cstm_pure
    , cstm_pure_unsafe
    , cexp
    , cexp_unsafe
    , cexp_pure
    , cexp_pure_unsafe
    , citems
    , citems_unsafe
    , citems_pure
    , citems_pure_unsafe
    ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Text.PrettyPrint.Mainland as PrettyPrint
import qualified Language.C as C
import qualified Language.C.Quote.C as C
import           Control.Exception (catch, throwIO)
import           System.FilePath (addExtension, dropExtension)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Control.Monad (void, unless)
import           System.IO.Error (isDoesNotExistError)
import           System.Directory (removeFile)
import           Data.Functor ((<$>))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Parsec.String as Parsec
import           Data.Loc (Pos(..), locOf)
import qualified Data.ByteString.UTF8
import           Control.Applicative ((*>), (<*), (<|>))
import           Data.Data (Data)
import           Generics.SYB (everywhereM)
import qualified Data.Map as Map
import qualified Control.Monad.Trans.State as State
import           Data.Typeable (Typeable, (:~:)(..), eqT)
import           Data.Foldable (forM_)
import           Data.Maybe (fromMaybe)

import           Language.C.Context

------------------------------------------------------------------------
-- Module compile-time state

-- $building
--
-- Each module that uses at least one of the TH functions below gets
-- a C file associated to it.  This C file must be built after the
-- Haskell code and linked appropriately.  If you use cabal, all you
-- have to do is declare each associated C file in the @.cabal@ file and
-- you are good.
--
-- For example we might have
--
-- @
-- executable foo
--   main-is:             Main.hs, Foo.hs, Bar.hs
--   hs-source-dirs:      src
--   -- Here the corresponding C sources must be listed for every module
--   -- that uses C code.  In this example, Main.hs and Bar.hs do, but
--   -- Foo.hs does not.
--   c-sources:           src\/Main.c, src\/Bar.c
--   -- These flags will be passed to the C compiler
--   cc-options:          -Wall -O2
--   -- Libraries to link the code with.
--   extra-libraries:     -lm
--   ...
-- @
--
-- Note that currently @cabal repl@ is not supported, because the C code
-- is not compiled and linked appropriately.

data ModuleState = ModuleState
  { msModuleName :: String
  , msContext :: Context
  }

{-# NOINLINE moduleStateRef #-}
moduleStateRef :: IORef (Maybe ModuleState)
moduleStateRef = unsafePerformIO $ newIORef Nothing

-- | Make sure that 'moduleStateRef' and the respective C file are up
-- to date.
initialiseModuleState
  :: Maybe Context
  -- ^ The 'Context' to use if we initialise the module.  If 'Nothing',
  -- 'baseCtx' will be used.
  -> TH.Q ()
initialiseModuleState mbContext = do
  cFile <- cSourceLoc
  mbModuleState <- TH.runIO $ readIORef moduleStateRef
  thisModule <- TH.loc_module <$> TH.location
  let recordThisModule = TH.runIO $ do
        -- If the file exists and this is the first time we write
        -- something from this module (in other words, if we are
        -- recompiling the module), kill the file first.
        removeIfExists cFile
        writeIORef moduleStateRef $ Just ModuleState
          { msModuleName = thisModule
          , msContext = context
          }
  case mbModuleState of
    Nothing -> recordThisModule
    Just ms | msModuleName ms == thisModule -> return ()
    Just _ms -> recordThisModule
  where
    context = fromMaybe baseCtx mbContext

initialiseModuleState_ :: TH.Q ()
initialiseModuleState_ = initialiseModuleState Nothing

getModuleState :: TH.Q ModuleState
getModuleState = do
  mbModuleState <- TH.runIO $ readIORef moduleStateRef
  thisModule <- TH.loc_module <$> TH.location
  case mbModuleState of
    Nothing -> error "inline-c: ModuleState not present"
    Just ms | msModuleName ms == thisModule -> return ms
    Just _ms -> error "inline-c: stale ModuleState"

getContext :: TH.Q Context
getContext = msContext <$> getModuleState

setContext :: Context -> TH.DecsQ
setContext ctx = do
  initialiseModuleState $ Just ctx
  return []

------------------------------------------------------------------------
-- Emitting

-- $emitting
--
-- TODO document

-- | Data type representing some C code with a typed and named entry
-- function.
data Code = Code
  { codeCallSafety :: TH.Safety
    -- ^ Safety of the foreign call
  , codeType :: TH.TypeQ
    -- ^ Type of the foreign call
  , codeFunName :: String
    -- ^ Name of the function to call in the code above.
  , codeDefs :: [C.Definition]
    -- ^ The C code.
  }

cSourceLoc :: TH.Q FilePath
cSourceLoc = do
  thisFile <- TH.loc_filename <$> TH.location
  return $ dropExtension thisFile `addExtension` "c"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e = unless (isDoesNotExistError e) $ throwIO e

-- | Simply appends some string to the module's C file.  Use with care.
emitLiteral :: String -> TH.DecsQ
emitLiteral s = do
  initialiseModuleState_         -- Make sure that things are up-to-date
  cFile <- cSourceLoc
  TH.runIO $ appendFile cFile $ "\n" ++ s ++ "\n"
  return []

-- | Emits some definitions to the module C file.
emitCode :: [C.Definition] -> TH.DecsQ
emitCode defs = do
  forM_ defs $ \def -> void $ emitLiteral $ pretty80 def
  return []

-- | Emits an include CPP statement for the given file.
-- To avoid having to escape quotes, the function itself adds them when
-- appropriate, so that
--
-- @
-- emitInclude "foo.h" ==> #include "foo.h"
-- @
--
-- but
--
-- @
-- emitInclude \<foo\> ==> #include \<foo\>
-- @
emitInclude :: String -> TH.DecsQ
emitInclude s
  | null s = error "emitImport: empty string"
  | head s == '<' = emitLiteral $ "#include " ++ s
  | otherwise = emitLiteral $ "#include \"" ++ s ++ "\""

------------------------------------------------------------------------
-- Embedding

-- $embedding
--
-- TODO document

-- TODO use the #line CPP macro to have the functions in the C file
-- refer to the source location in the Haskell file they come from.
--
-- See <https://gcc.gnu.org/onlinedocs/cpp/Line-Control.html>.

-- | Embeds a piece of code inline.  The resulting 'TH.Exp' will have
-- the type specified in the 'codeType'.
--
-- @
-- c_add :: Int -> Int -> Int
-- c_add = $(embedCode $ Code
--   TH.Unsafe                   -- Call safety
--   [t| Int -> Int -> Int |]    -- Call type
--   "francescos_add"            -- Call name
--   -- C Code
--   [C.cunit| int francescos_add(int x, int y) { int z = x + y; return z; } |])
-- @
embedCode :: Code -> TH.ExpQ
embedCode Code{..} = do
  initialiseModuleState_         -- Make sure that things are up-to-date
  -- Write out definitions
  void $ emitCode codeDefs
  -- Create and add the FFI declaration.
  -- TODO absurdly, I need to
  -- 'newName' twice for things to work.  I found this hack in
  -- language-c-inline.  Why is this?
  ffiImportName <- TH.newName . show =<< TH.newName "inline_c_ffi"
  dec <- TH.forImpD TH.CCall codeCallSafety codeFunName ffiImportName codeType
  TH.addTopDecls [dec]
  [| $(TH.varE ffiImportName) |]

uniqueCName :: IO String
uniqueCName = do
  -- UUID with the dashes removed
  unique <- filter (/= '-') . UUID.toString <$> UUID.nextRandom
  return $ "inline_c_" ++ unique

-- |
-- @
-- c_cos :: Double -> Double
-- c_cos = $(embedStm
--   TH.Unsafe
--   [t| Double -> Double |]
--   [cty| double |] [cparams| double x |]
--   [cstm| return cos(x); |])
-- @
embedStm
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type
  -- ^ Return type of the C expr
  -> [C.Param]
  -- ^ Parameters of the C expr
  -> C.Stm
  -- ^ The C statement
  -> TH.ExpQ
embedStm callSafety type_ cRetType cParams cStm = do
  funName <- TH.runIO uniqueCName
  let defs = [C.cunit| $ty:cRetType $id:funName($params:cParams) { $stm:cStm } |]
  embedCode $ Code
    { codeCallSafety = callSafety
    , codeType = type_
    , codeFunName = funName
    , codeDefs = defs
    }

-- |
-- @
-- c_cos :: Double -> Double
-- c_cos = $(embedStm
--   TH.Unsafe
--   [t| Double -> Double |]
--   [cty| double |] [cparams| double x |]
--   [cstm| return cos(x); |])
-- @
embedExp
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type
  -- ^ Return type of the C expr
  -> [C.Param]
  -- ^ Parameters of the C expr
  -> C.Exp
  -- ^ The C expression
  -> TH.ExpQ
embedExp callSafety type_ cRetType cParams cExp =
  embedStm callSafety type_ cRetType cParams [C.cstm| $exp:cExp; |]

embedItems
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type
  -- ^ Return type of the C expr
  -> [C.Param]
  -- ^ Parameters of the C expr
  -> [C.BlockItem]
  -> TH.ExpQ
embedItems callSafety type_ cRetType cParams cItems = do
  funName <- TH.runIO uniqueCName
  let defs = [C.cunit| $ty:cRetType $id:funName($params:cParams) { $items:cItems } |]
  embedCode $ Code
    { codeCallSafety = callSafety
    , codeType = type_
    , codeFunName = funName
    , codeDefs = defs
    }

------------------------------------------------------------------------
-- Quoting sugar

-- $quoting
--
-- TODO document

cexp :: TH.QuasiQuoter
cexp = genericQuote False C.parseExp $ embedExp TH.Safe

cexp_unsafe :: TH.QuasiQuoter
cexp_unsafe = genericQuote False C.parseExp $ embedExp TH.Unsafe

cexp_pure :: TH.QuasiQuoter
cexp_pure = genericQuote True C.parseExp $ embedExp TH.Safe

cexp_pure_unsafe :: TH.QuasiQuoter
cexp_pure_unsafe = genericQuote True C.parseExp $ embedExp TH.Unsafe

cstm :: TH.QuasiQuoter
cstm = genericQuote False C.parseStm $ embedStm TH.Safe

cstm_unsafe :: TH.QuasiQuoter
cstm_unsafe = genericQuote False C.parseStm $ embedStm TH.Unsafe

cstm_pure :: TH.QuasiQuoter
cstm_pure = genericQuote True C.parseStm $ embedStm TH.Safe

cstm_pure_unsafe :: TH.QuasiQuoter
cstm_pure_unsafe = genericQuote True C.parseStm $ embedStm TH.Unsafe

citems :: TH.QuasiQuoter
citems = genericQuote False C.parseBlockItems $ embedItems TH.Safe

citems_unsafe :: TH.QuasiQuoter
citems_unsafe = genericQuote False C.parseBlockItems $ embedItems TH.Unsafe

citems_pure :: TH.QuasiQuoter
citems_pure = genericQuote True C.parseBlockItems $ embedItems TH.Safe

citems_pure_unsafe :: TH.QuasiQuoter
citems_pure_unsafe = genericQuote True C.parseBlockItems $ embedItems TH.Unsafe

quoteCode
  :: (String -> TH.ExpQ)
  -- ^ The parser
  -> TH.QuasiQuoter
quoteCode p = TH.QuasiQuoter
  { TH.quoteExp = p
  , TH.quotePat = error "quoteCode: quotePat not implemented"
  , TH.quoteType = error "quoteCode: quoteType not implemented"
  , TH.quoteDec = error "quoteCode: quoteDec not implemeted"
  }

genericQuote
  :: (Data a)
  => Bool
  -- ^ Whether the call should be pure or not
  -> C.P a
  -- ^ Parser producing something
  -> (TH.TypeQ -> C.Type -> [C.Param] -> a -> TH.ExpQ)
  -- ^ Function taking that something and building an expression, see
  -- 'embedExp' for other args.
  -> TH.QuasiQuoter
genericQuote pure p build = quoteCode $ \s -> do
  context <- getContext
  (cType, cParams, cExp) <- runCParser s $ parseTypedC context p
  let hsType = convertCFunSig pure cType $ map snd cParams
  buildFunCall (build hsType cType (map rebuildParam cParams) cExp) $ map fst cParams
  where
    buildFunCall :: TH.ExpQ -> [C.Id] -> TH.ExpQ
    buildFunCall f [] =
      f
    buildFunCall f (name : params) = do
      mbHsName <- TH.lookupValueName $ case name of
        C.Id s _ -> s
        C.AntiId _ _ -> error "inline-c: got antiquotation (buildFunCall)"
      case mbHsName of
        Nothing -> do
          error $ "Cannot capture Haskell variable " ++ show name ++
                  ", because it's not in scope."
        Just hsName -> do
          buildFunCall [| $f $(TH.varE hsName) |] params

    rebuildParam :: (C.Id, C.Type) -> C.Param
    rebuildParam (name, C.Type ds d loc) = C.Param (Just name) ds d loc
    rebuildParam (_, _) = error "inline-c: got antiquotation (rebuildParam)"

-- Type conversion

convertCFunSig :: Bool -> C.Type -> [C.Type] -> TH.TypeQ
convertCFunSig pure retType params0 = do
  ctx <- getContext
  go params0 $ \cTy -> do
    mbHsTy <- convertCType ctx cTy
    case mbHsTy of
      Nothing -> error $ "Could not resolve Haskell type for C type " ++ pretty80 cTy
      Just hsTy -> return hsTy
  where
    go [] cToHs = do
      let hsType = cToHs retType
      if pure then hsType else [t| IO $hsType |]
    go (paramType : params) cToHs = do
      [t| $(cToHs paramType) -> $(go params cToHs) |]

-- Parsing

runCParser :: String -> Parsec.Parser a -> TH.Q a
runCParser s p = do
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
  typeStr <- takeTillAnyChar ['(', '{']
  let cType = parseC context typePos typeStr C.parseType
  -- Get stuff for params, and parse them
  let emptyParams = do
        Parsec.try $ do
          lex_ $ Parsec.char '('
          lex_ $ Parsec.char ')'
        lex_ $ Parsec.char '{'
        return []
  let someParams = do
        lex_ $ Parsec.char '('
        paramsPos <- Parsec.getPosition
        paramsStr <- takeTillChar ')'
        lex_ $ Parsec.char '{'
        return $ map cleanupParam $ parseC context paramsPos paramsStr C.parseParams
  let noParams = do
        lex_ $ Parsec.char '{'
        return []
  cParams <- emptyParams <|> someParams <|> noParams
  -- Get the body, and feed it to the given parser
  bodyPos <- Parsec.getPosition
  bodyStr <- takeTillChar '}'
  let cBody = parseC context bodyPos bodyStr p
  -- Collect the implicit parameters present in the body
  let paramsMap = mkParamsMap cParams
  let (cBody', paramsMap') = State.runState (collectImplParams cBody) paramsMap
  -- Whew
  return (cType, Map.toList paramsMap', cBody')
  where
    lex_ :: Parsec.Parser b -> Parsec.Parser ()
    lex_ p' = do
      void p'
      Parsec.spaces

    takeTillAnyChar :: [Char] -> Parsec.Parser String
    takeTillAnyChar chs = Parsec.many $ Parsec.satisfy $ \ch -> all (/= ch) chs

    takeTillChar :: Char -> Parsec.Parser String
    takeTillChar ch = do
      str <- takeTillAnyChar [ch]
      lex_ $ Parsec.char ch
      return str

    cleanupParam :: C.Param -> (C.Id, C.Type)
    cleanupParam param = case param of
      C.Param Nothing _ _ _        -> error "Cannot capture Haskell variable if you don't give a name."
      C.Param (Just name) ds d loc -> (name, C.Type ds d loc)
      _                            -> error "inline-c: got antiquotation (parseTypedC)"

    mkParamsMap :: [(C.Id, C.Type)] -> Map.Map C.Id C.Type
    mkParamsMap cParams =
      let m = Map.fromList cParams
      in if Map.size m == length cParams
           then m
           else error "Duplicated variable in parameter list"

    collectImplParams = everywhereM (typeableAppM collectImplParam)

    collectImplParam :: C.Id -> State.State (Map.Map C.Id C.Type) C.Id
    collectImplParam name0@(C.Id str loc) = do
      case hasSuffixType str of
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

    hasSuffixType :: String -> Maybe (String, C.Type)
    hasSuffixType s =
      let (afterUnd0, beforeUnd0) = break (== '_') $ reverse s
          (beforeUnd, afterUnd) = (reverse (tail beforeUnd0), reverse afterUnd0)
      in if beforeUnd0 == ""
           then Nothing
           else (,) beforeUnd <$> ctxSuffixTypes context afterUnd

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
