{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Language.C.Inline.Internal
    ( -- * Context handling
      setContext
    , getContext

      -- * Emitting and invoking C code
      --
      -- | The functions in this section let us access more the C file
      -- associated with the current module.  They can be used to build
      -- additional features on top of the basic machinery.  All of
      -- @inline-c@ is based upon the functions defined here.

      -- ** Emitting C code
    , emitVerbatim

      -- ** Inlining C code
      -- $embedding
    , Code(..)
    , inlineCode
    , inlineExp
    , inlineItems

      -- * Parsing
      --
      -- | These functions are used to parse the anti-quotations.  They're
      -- exposed for testing purposes, you really should not use them.
    , SomeEq
    , toSomeEq
    , fromSomeEq
    , ParameterType(..)
    , ParseTypedC(..)
    , parseTypedC
    , runParserInQ

      -- * Utility functions for writing quasiquoters
    , genericQuote
    ) where

import           Control.Applicative
import           Control.Exception (catch, throwIO)
import           Control.Monad (forM, void, msum, when, unless)
import           Control.Monad.State (evalStateT, StateT, get, put)
import           Control.Monad.Trans.Class (lift)
import qualified Crypto.Hash as CryptoHash
import qualified Data.Binary as Binary
import           Data.Foldable (forM_)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable, cast)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import           System.Directory (removeFile)
import           System.FilePath (addExtension, dropExtension)
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Parser.Char as Parser
import qualified Text.Parser.Combinators as Parser
import qualified Text.Parser.LookAhead as Parser
import qualified Text.Parser.Token as Parser
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.C.Types as C
import           Language.C.Inline.Context
import           Language.C.Inline.FunPtr

data ModuleState = ModuleState
  { msModuleName :: String
  , msContext :: Context
  , msGeneratedNames :: Int
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
  -> TH.Q Context
initialiseModuleState mbContext = do
  cFile <- cSourceLoc context
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
          , msGeneratedNames = 0
          }
        return context
  case mbModuleState of
    Nothing -> recordThisModule
    Just ms | msModuleName ms == thisModule -> return $ msContext ms
    Just _ms -> recordThisModule
  where
    context = fromMaybe baseCtx mbContext

-- | Gets the current 'Context'.  Also makes sure that the current
-- module is initialised.
getContext :: TH.Q Context
getContext = initialiseModuleState Nothing

getModuleState :: TH.Q ModuleState
getModuleState = do
  mbModuleState <- TH.runIO $ readIORef moduleStateRef
  thisModule <- TH.loc_module <$> TH.location
  case mbModuleState of
    Nothing -> error "inline-c: ModuleState not present"
    Just ms | msModuleName ms == thisModule -> return ms
    Just _ms -> error "inline-c: stale ModuleState"

-- $context
--
-- The inline C functions ('cexp', 'c', etc.) need a 'Context' to
-- operate.  Said context can be explicitely set with 'setContext'.
-- Otherwise, at the first usage of one of the TH functions in this
-- module the 'Context' is implicitely set to 'baseCtx'.

-- | Sets the 'Context' for the current module.  This function, if
-- called, must be called before any of the other TH functions in this
-- module.  Fails if that's not the case.
setContext :: Context -> TH.Q ()
setContext ctx = do
  mbModuleState <- TH.runIO $ readIORef moduleStateRef
  forM_ mbModuleState $ \ms -> do
    thisModule <- TH.loc_module <$> TH.location
    when (msModuleName ms == thisModule) $
      error "inline-c: The module has already been initialised (setContext)."
  void $ initialiseModuleState $ Just ctx

bumpGeneratedNames :: TH.Q Int
bumpGeneratedNames = do
  ms <- getModuleState
  TH.runIO $ do
    let c' = msGeneratedNames ms
    writeIORef moduleStateRef $ Just ms{msGeneratedNames = c' + 1}
    return c'

------------------------------------------------------------------------
-- Emitting

cSourceLoc :: Context -> TH.Q FilePath
cSourceLoc ctx = do
  thisFile <- TH.loc_filename <$> TH.location
  let ext = fromMaybe "c" $ ctxFileExtension ctx
  return $ dropExtension thisFile `addExtension` ext

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e = unless (isDoesNotExistError e) $ throwIO e

-- | Simply appends some string to the module's C file.  Use with care.
emitVerbatim :: String -> TH.DecsQ
emitVerbatim s = do
  ctx <- getContext
  cFile <- cSourceLoc ctx
  TH.runIO $ appendFile cFile $ "\n" ++ s ++ "\n"
  return []

------------------------------------------------------------------------
-- Inlining

-- $embedding
--
-- We use the 'Code' data structure to represent some C code that we
-- want to emit to the module's C file and immediately generate a
-- foreign call to.  For this reason, 'Code' includes both some C
-- definition, and enough information to be able to generate a foreign
-- call -- specifically the name of the function to call and the Haskell
-- type.
--
-- All the quasi-quoters work by constructing a 'Code' and calling
-- 'inlineCode'.

-- | Data type representing a list of C definitions with a typed and named entry
-- function.
--
-- We use it as a basis to inline and call C code.
data Code = Code
  { codeCallSafety :: TH.Safety
    -- ^ Safety of the foreign call.
  , codeType :: TH.TypeQ
    -- ^ Type of the foreign call.
  , codeFunName :: String
    -- ^ Name of the function to call in the code below.
  , codeDefs :: String
    -- ^ The C code.
  }

-- TODO use the #line CPP macro to have the functions in the C file
-- refer to the source location in the Haskell file they come from.
--
-- See <https://gcc.gnu.org/onlinedocs/cpp/Line-Control.html>.

-- | Inlines a piece of code inline.  The resulting 'TH.Exp' will have
-- the type specified in the 'codeType'.
--
-- In practice, this function outputs the C code to the module's C file,
-- and then inserts a foreign call of type 'codeType' calling the
-- provided 'codeFunName'.
--
-- Example:
--
-- @
-- c_add :: Int -> Int -> Int
-- c_add = $(inlineCode $ Code
--   TH.Unsafe                   -- Call safety
--   [t| Int -> Int -> Int |]    -- Call type
--   "francescos_add"            -- Call name
--   -- C Code
--   \"int francescos_add(int x, int y) { int z = x + y; return z; }\")
-- @
inlineCode :: Code -> TH.ExpQ
inlineCode Code{..} = do
  -- Write out definitions
  ctx <- getContext
  let out = fromMaybe id $ ctxOutput ctx
  void $ emitVerbatim $ out codeDefs
  -- Create and add the FFI declaration.
  ffiImportName <- uniqueFfiImportName
  dec <- TH.forImpD TH.CCall codeCallSafety codeFunName ffiImportName codeType
  TH.addTopDecls [dec]
  TH.varE ffiImportName

uniqueCName :: String -> TH.Q String
uniqueCName x = do
  c' <- bumpGeneratedNames
  let unique :: CryptoHash.Digest CryptoHash.SHA1 = CryptoHash.hashlazy $ Binary.encode x
  return $ "inline_c_" ++ show c' ++ "_" ++ show unique

-- | Same as 'inlineCItems', but with a single expression.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(inlineExp
--   TH.Unsafe
--   [t| Double -> Double |]
--   (quickCParser_ \"double\" parseType)
--   [("x", quickCParser_ \"double\") parseType]
--   "cos(x)")
-- @
inlineExp
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type
  -- ^ Return type of the C expr
  -> [(C.Identifier, C.Type)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C expression
  -> TH.ExpQ
inlineExp callSafety type_ cRetType cParams cExp =
  inlineItems callSafety type_ cRetType cParams cItems
  where
    cItems = case cRetType of
      C.TypeSpecifier _quals C.Void -> cExp ++ ";"
      _ -> "return (" ++ cExp ++ ");"

-- | Same as 'inlineCode', but accepts a string containing a list of C
-- statements instead instead than a full-blown 'Code'.  A function
-- containing the provided statement will be automatically generated.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(inlineItems
--   TH.Unsafe
--   [t| Double -> Double |]
--   (quickCParser_ \"double\" parseType)
--   [("x", quickCParser_ \"double\" parseType)]
--   "return cos(x);")
-- @
inlineItems
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type
  -- ^ Return type of the C expr
  -> [(C.Identifier, C.Type)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C items
  -> TH.ExpQ
inlineItems callSafety type_ cRetType cParams cItems = do
  let mkParam (id', paramTy) = C.ParameterDeclaration (Just id') paramTy
  let proto = C.Proto cRetType (map mkParam cParams)
  funName <- uniqueCName $ show proto ++ cItems
  let decl = C.ParameterDeclaration (Just (C.Identifier funName)) proto
  let defs =
        prettyOneLine decl ++ " {\n" ++
        cItems ++ "\n}\n"
  inlineCode $ Code
    { codeCallSafety = callSafety
    , codeType = type_
    , codeFunName = funName
    , codeDefs = defs
    }

------------------------------------------------------------------------
-- Parsing

runParserInQ
  :: String -> C.IsTypeName -> (forall m. C.CParser m => m a) -> TH.Q a
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
  | AntiQuote AntiQuoterId SomeEq
  deriving (Show, Eq)

data ParseTypedC = ParseTypedC
  { ptcReturnType :: C.Type
  , ptcParameters :: [(C.Identifier, C.Type, ParameterType)]
  , ptcBody :: String
  }

parseTypedC
  :: forall m. C.CParser m
  => AntiQuoters -> m ParseTypedC
  -- ^ Returns the return type, the captured variables, and the body.
parseTypedC antiQs = do
  -- Parse return type (consume spaces first)
  Parser.spaces
  cRetType <- C.parseType
  -- Parse the body
  void $ Parser.char '{'
  (cParams, cBody) <- evalStateT parseBody 0
  return $ ParseTypedC cRetType cParams cBody
  where
    parseBody :: StateT Int m ([(C.Identifier, C.Type, ParameterType)], String)
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
              Just id' -> return $ C.unIdentifier id'
            id' <- freshId s'
            void $ Parser.char ')'
            return ([(id', C.parameterDeclarationType decl, Plain s')], C.unIdentifier id')
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

    parseAntiQuote :: StateT Int m ([(C.Identifier, C.Type, ParameterType)], String)
    parseAntiQuote = msum
      [ do void $ Parser.try (Parser.string $ antiQId ++ ":") Parser.<?> "anti quoter id"
           (s, cTy, x) <- aqParser antiQ
           id' <- freshId s
           return ([(id', cTy, AntiQuote antiQId (toSomeEq x))], C.unIdentifier id')
      | (antiQId, SomeAntiQuoter antiQ) <- Map.toList antiQs
      ]

    freshId s = do
      c <- get
      put $ c + 1
      return $ C.Identifier $ s ++ "_inline_c_" ++ show c

quoteCode
  :: (String -> TH.ExpQ)
  -- ^ The parser
  -> TH.QuasiQuoter
quoteCode p = TH.QuasiQuoter
  { TH.quoteExp = p
  , TH.quotePat = error "inline-c: quotePat not implemented (quoteCode)"
  , TH.quoteType = error "inline-c: quoteType not implemented (quoteCode)"
  , TH.quoteDec = error "inline-c: quoteDec not implemented (quoteCode)"
  }

genericQuote
  :: Purity
  -> (TH.TypeQ -> C.Type -> [(C.Identifier, C.Type)] -> String -> TH.ExpQ)
  -- ^ Function taking that something and building an expression, see
  -- 'inlineExp' for other args.
  -> TH.QuasiQuoter
genericQuote purity build = quoteCode $ \s -> do
    ctx <- getContext
    ParseTypedC cType cParams cExp <-
      runParserInQ s (isTypeName (ctxTypesTable ctx)) $ parseTypedC $ ctxAntiQuoters ctx
    hsType <- cToHs ctx cType
    hsParams <- forM cParams $ \(_cId, cTy, parTy) -> do
      case parTy of
        Plain s' -> do
          hsTy <- cToHs ctx cTy
          mbHsName <- TH.lookupValueName s'
          hsExp <- case mbHsName of
            Nothing -> do
              error $ "Cannot capture Haskell variable " ++ s' ++
                      ", because it's not in scope. (genericQuote)"
            Just hsName -> do
              hsExp <- TH.varE hsName
              [| \cont -> cont ($(return hsExp) :: $(return hsTy)) |]
          return (hsTy, hsExp)
        AntiQuote antiId dyn -> do
          case Map.lookup antiId (ctxAntiQuoters ctx) of
            Nothing ->
              error $ "IMPOSSIBLE: could not find anti-quoter " ++ show antiId ++
                      ". (genericQuote)"
            Just (SomeAntiQuoter antiQ) -> case fromSomeEq dyn of
              Nothing ->
                error  $ "IMPOSSIBLE: could not cast value for anti-quoter " ++
                         show antiId ++ ". (genericQuote)"
              Just x ->
                aqMarshaller antiQ purity (ctxTypesTable ctx) cTy x
    let hsFunType = convertCFunSig hsType $ map fst hsParams
    let cParams' = [(cId, cTy) | (cId, cTy, _) <- cParams]
    ioCall <- buildFunCall ctx (build hsFunType cType cParams' cExp) (map snd hsParams) []
    -- If the user requested a pure function, make it so.
    case purity of
      Pure -> [| unsafePerformIO $(return ioCall) |]
      IO -> return ioCall
  where
    cToHs :: Context -> C.Type -> TH.TypeQ
    cToHs ctx cTy = do
      mbHsTy <- convertType purity (ctxTypesTable ctx) cTy
      case mbHsTy of
        Nothing -> error $ "Could not resolve Haskell type for C type " ++ pretty80 cTy
        Just hsTy -> return hsTy

    buildFunCall :: Context -> TH.ExpQ -> [TH.Exp] -> [TH.Name] -> TH.ExpQ
    buildFunCall _ctx f [] args =
      foldl (\f' arg -> [| $f' $(TH.varE arg) |]) f args
    buildFunCall ctx f (hsExp : params) args =
       [| $(return hsExp) $ \arg ->
            $(buildFunCall ctx f params (args ++ ['arg]))
       |]

    convertCFunSig :: TH.Type -> [TH.Type] -> TH.TypeQ
    convertCFunSig retType params0 = do
      go params0
      where
        go [] =
          [t| IO $(return retType) |]
        go (paramType : params) = do
          [t| $(return paramType) -> $(go params) |]

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""

prettyOneLine :: PP.Pretty a => a -> String
prettyOneLine x = PP.displayS (PP.renderCompact (PP.pretty x)) ""
