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
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import           Control.Exception (catch, throwIO)
import           Control.Monad (forM, void, msum, unless)
import           Control.Monad.State (evalStateT, StateT, get, put)
import           Control.Monad.Trans.Class (lift)
import qualified Crypto.Hash as CryptoHash
import qualified Data.Binary as Binary
import           Data.Foldable (forM_)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Traversable (for)
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
import           System.Environment (getProgName)

import           Language.C.Inline.Context
import           Language.C.Inline.FunPtr
import           Language.C.Inline.HaskellIdentifier
import qualified Language.C.Types as C

data ModuleState = ModuleState
  { msContext :: Context
  , msGeneratedNames :: Int
  }

-- | Identifier for the current module.  Currently we use the file name.
-- Since we're pairing Haskell files with C files, it makes more sense
-- to use the file name.  I'm not sure if it's possible to compile two
-- modules with the same name in one run of GHC, but in this way we make
-- sure that we don't run into trouble even it is.
type ModuleId = String

getModuleId :: TH.Q ModuleId
getModuleId = TH.loc_filename <$> TH.location

-- | 'MVar' storing the state for all the modules we visited.  Note that
-- currently we do not bother with cleaning up the state after we're
-- done compiling a module.  TODO if there is an easy way, clean up the
-- state.
{-# NOINLINE moduleStatesVar #-}
moduleStatesVar :: MVar (Map.Map ModuleId ModuleState)
moduleStatesVar = unsafePerformIO $ newMVar Map.empty

-- | Make sure that 'moduleStatesVar' and the respective C file are up
--   to date.
initialiseModuleState
  :: Maybe Context
  -- ^ The 'Context' to use if we initialise the module.  If 'Nothing',
  -- 'baseCtx' will be used.
  -> TH.Q Context
initialiseModuleState mbContext = do
  mbcFile <- cSourceLoc context
  thisModule <- getModuleId
  TH.runIO $ modifyMVar moduleStatesVar $ \moduleStates -> do
    case Map.lookup thisModule moduleStates of
      Just moduleState -> return (moduleStates, msContext moduleState)
      Nothing -> do
        -- If the file exists and this is the first time we write
        -- something from this module (in other words, if we are
        -- recompiling the module), kill the file first.
        forM_ mbcFile $ \cFile -> removeIfExists cFile
        let moduleState = ModuleState
              { msContext = context
              , msGeneratedNames = 0
              }
        return (Map.insert thisModule moduleState moduleStates, context)
  where
    context = fromMaybe baseCtx mbContext

-- | Gets the current 'Context'.  Also makes sure that the current
-- module is initialised.
getContext :: TH.Q Context
getContext = initialiseModuleState Nothing

modifyModuleState :: (ModuleState -> (ModuleState, a)) -> TH.Q a
modifyModuleState f = do
  thisModule <- getModuleId
  TH.runIO $ modifyMVar moduleStatesVar $ \moduleStates ->
    case Map.lookup thisModule moduleStates of
      Nothing -> fail "inline-c: ModuleState not present"
      Just ms -> do
        let (ms', x) = f ms
        return (Map.insert thisModule ms' moduleStates, x)

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
  thisModule <- getModuleId
  moduleStates <- TH.runIO $ readMVar moduleStatesVar
  forM_ (Map.lookup thisModule moduleStates) $ \_ms ->
    fail "inline-c: The module has already been initialised (setContext)."
  void $ initialiseModuleState $ Just ctx

bumpGeneratedNames :: TH.Q Int
bumpGeneratedNames = do
  modifyModuleState $ \ms ->
    let c' = msGeneratedNames ms
    in (ms{msGeneratedNames = c' + 1}, c')

------------------------------------------------------------------------
-- Emitting

-- | Return the path in which to emit C code. Or 'Nothing' if emitting should be
-- inhibited, say because we're only type checking the module, not emitting code
-- (e.g. with @-fno-code@ or in @haddock@)
cSourceLoc :: Context -> TH.Q (Maybe FilePath)
cSourceLoc ctx = do
    prog <- TH.runIO getProgName
    -- Hard-code a common case for not generating code.  haddock just
    -- type-checks, so we do not need to generate the C file again.
    -- See issue #24.
    let emitCode = prog /= "haddock"
    if not emitCode
      then return Nothing
      else do
        thisFile <- TH.loc_filename <$> TH.location
        let ext = fromMaybe "c" $ ctxFileExtension ctx
        return $ Just $ dropExtension thisFile `addExtension` ext

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e = unless (isDoesNotExistError e) $ throwIO e

-- | Simply appends some string to the module's C file.  Use with care.
emitVerbatim :: String -> TH.DecsQ
emitVerbatim s = do
  ctx <- getContext
  mbCFile <- cSourceLoc ctx
  case mbCFile of
    Nothing -> return ()
    Just cFile -> TH.runIO $ appendFile cFile $ "\n" ++ s ++ "\n"
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

uniqueCName
  :: String
  -- ^ Some string identifying the body of the symbol the name will
  -- refer too -- e.g. the function arguments + body.  This is used to
  -- generate persistent names: we do not want completely random names
  -- since this causes issues when cabal builds things repeatedly, for
  -- example when building with profiling.
  -> TH.Q String
uniqueCName x = do
  -- The name looks like this:
  -- inline_c_MODULE_INDEX_HASH
  --
  -- Where:
  --  * MODULE is the module name but with _s instead of .s;
  --  * INDEX is a counter that keeps track of how many names we're generating
  --    for each module;
  --  * HASH is the SHA1 hash of the contents.
  c' <- bumpGeneratedNames
  let unique :: CryptoHash.Digest CryptoHash.SHA1 = CryptoHash.hashlazy $ Binary.encode x
  module_ <- TH.loc_module <$> TH.location
  let replaceDot '.' = '_'
      replaceDot c = c
  return $ "inline_c_" ++ map replaceDot module_ ++ "_" ++ show c' ++ "_" ++ show unique

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
  -> C.Type C.CIdentifier
  -- ^ Return type of the C expr
  -> [(C.CIdentifier, C.Type C.CIdentifier)]
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
  -> C.Type C.CIdentifier
  -- ^ Return type of the C expr
  -> [(C.CIdentifier, C.Type C.CIdentifier)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C items
  -> TH.ExpQ
inlineItems callSafety type_ cRetType cParams cItems = do
  let mkParam (id', paramTy) = C.ParameterDeclaration (Just id') paramTy
  let proto = C.Proto cRetType (map mkParam cParams)
  funName <- uniqueCName $ show proto ++ cItems
  cFunName <- case C.cIdentifierFromString funName of
    Left err -> fail $ "inlineItems: impossible, generated bad C identifier " ++
                       "funName:\n" ++ err
    Right x -> return x
  let decl = C.ParameterDeclaration (Just cFunName) proto
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
  :: String -> C.TypeNames -> (forall m. C.CParser HaskellIdentifier m => m a) -> TH.Q a
runParserInQ s typeNames' p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let parsecLoc = Parsec.newPos (TH.loc_filename loc) line col
  let p' = lift (Parsec.setPosition parsecLoc) *> p <* lift Parser.eof
  case C.runCParser (haskellCParserContext typeNames') (TH.loc_filename loc) s p' of
    Left err -> do
      -- TODO consider prefixing with "error while parsing C" or similar
      fail $ show err
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
  = Plain HaskellIdentifier                -- The name of the captured variable
  | AntiQuote AntiQuoterId SomeEq
  deriving (Show, Eq)

data ParseTypedC = ParseTypedC
  { ptcReturnType :: C.Type C.CIdentifier
  , ptcParameters :: [(C.CIdentifier, C.Type C.CIdentifier, ParameterType)]
  , ptcBody :: String
  }

-- To parse C declarations, we're faced with a bit of a problem: we want
-- to parse the anti-quotations so that Haskell identifiers are
-- accepted, but we want them to appear only as the root of
-- declarations.  For this reason, we parse allowing Haskell identifiers
-- everywhere, and then we "purge" Haskell identifiers everywhere but at
-- the root.
parseTypedC
  :: forall m. C.CParser HaskellIdentifier m
  => AntiQuoters -> m ParseTypedC
  -- ^ Returns the return type, the captured variables, and the body.
parseTypedC antiQs = do
  -- Parse return type (consume spaces first)
  Parser.spaces
  cRetType <- purgeHaskellIdentifiers =<< C.parseType
  -- Parse the body
  void $ Parser.char '{'
  (cParams, cBody) <- evalStateT parseBody 0
  return $ ParseTypedC cRetType cParams cBody
  where
    parseBody
      :: StateT Int m ([(C.CIdentifier, C.Type C.CIdentifier, ParameterType)], String)
    parseBody = do
      -- Note that this code does not use "lexing" combinators (apart
      -- when appropriate) because we want to make sure to preserve
      -- whitespace after we substitute things.
      s <- Parser.manyTill Parser.anyChar $
           Parser.lookAhead (Parser.char '}' <|> Parser.char '$')
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

    parseAntiQuote
      :: StateT Int m ([(C.CIdentifier, C.Type C.CIdentifier, ParameterType)], String)
    parseAntiQuote = msum
      [ do void $ Parser.try (Parser.string $ antiQId ++ ":") Parser.<?> "anti quoter id"
           (s, cTy, x) <- aqParser antiQ
           id' <- freshId s
           return ([(id', cTy, AntiQuote antiQId (toSomeEq x))], C.unCIdentifier id')
      | (antiQId, SomeAntiQuoter antiQ) <- Map.toList antiQs
      ]

    parseEscapedDollar :: StateT Int m ([a], String)
    parseEscapedDollar = do
      void $ Parser.char '$'
      return ([], "$")

    parseTypedCapture
      :: StateT Int m ([(C.CIdentifier, C.Type C.CIdentifier, ParameterType)], String)
    parseTypedCapture = do
      void $ Parser.symbolic '('
      decl <- C.parseParameterDeclaration
      declType <- purgeHaskellIdentifiers $ C.parameterDeclarationType decl
      -- Purge the declaration type of all the Haskell identifiers.
      hId <- case C.parameterDeclarationId decl of
        Nothing -> fail $ pretty80 $
          "Un-named captured variable in decl" <+> PP.pretty decl
        Just hId -> return hId
      id' <- freshId $ mangleHaskellIdentifier hId
      void $ Parser.char ')'
      return ([(id', declType, Plain hId)], C.unCIdentifier id')

    freshId s = do
      c <- get
      put $ c + 1
      case C.cIdentifierFromString (C.unCIdentifier s ++ "_inline_c_" ++ show c) of
        Left _err -> error "freshId: The impossible happened"
        Right x -> return x

    -- The @m@ is polymorphic because we use this both for the plain
    -- parser and the StateT parser we use above.  We only need 'fail'.
    purgeHaskellIdentifiers
      :: forall n. (Applicative n, Monad n)
      => C.Type HaskellIdentifier -> n (C.Type C.CIdentifier)
    purgeHaskellIdentifiers cTy = for cTy $ \hsIdent -> do
      let hsIdentS = unHaskellIdentifier hsIdent
      case C.cIdentifierFromString hsIdentS of
        Left err -> fail $ "Haskell identifier " ++ hsIdentS ++ " in illegal position" ++
                           "in C type\n" ++ pretty80 cTy ++ "\n" ++
                           "A C identifier was expected, but:\n" ++ err
        Right cIdent -> return cIdent

quoteCode
  :: (String -> TH.ExpQ)
  -- ^ The parser
  -> TH.QuasiQuoter
quoteCode p = TH.QuasiQuoter
  { TH.quoteExp = p
  , TH.quotePat = fail "inline-c: quotePat not implemented (quoteCode)"
  , TH.quoteType = fail "inline-c: quoteType not implemented (quoteCode)"
  , TH.quoteDec = fail "inline-c: quoteDec not implemented (quoteCode)"
  }

genericQuote
  :: Purity
  -> (TH.TypeQ -> C.Type C.CIdentifier -> [(C.CIdentifier, C.Type C.CIdentifier)] -> String -> TH.ExpQ)
  -- ^ Function building an Haskell expression, see 'inlineExp' for
  -- guidance on the other args.
  -> TH.QuasiQuoter
genericQuote purity build = quoteCode $ \s -> do
    ctx <- getContext
    ParseTypedC cType cParams cExp <-
      runParserInQ s (typeNamesFromTypesTable (ctxTypesTable ctx)) $ parseTypedC $ ctxAntiQuoters ctx
    hsType <- cToHs ctx cType
    hsParams <- forM cParams $ \(_cId, cTy, parTy) -> do
      case parTy of
        Plain s' -> do
          hsTy <- cToHs ctx cTy
          mbHsName <- TH.lookupValueName $ unHaskellIdentifier s'
          hsExp <- case mbHsName of
            Nothing -> do
              fail $ "Cannot capture Haskell variable " ++ unHaskellIdentifier s' ++
                     ", because it's not in scope. (genericQuote)"
            Just hsName -> do
              hsExp <- TH.varE hsName
              [| \cont -> cont ($(return hsExp) :: $(return hsTy)) |]
          return (hsTy, hsExp)
        AntiQuote antiId dyn -> do
          case Map.lookup antiId (ctxAntiQuoters ctx) of
            Nothing ->
              fail $ "IMPOSSIBLE: could not find anti-quoter " ++ show antiId ++
                     ". (genericQuote)"
            Just (SomeAntiQuoter antiQ) -> case fromSomeEq dyn of
              Nothing ->
                fail  $ "IMPOSSIBLE: could not cast value for anti-quoter " ++
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
    cToHs :: Context -> C.Type C.CIdentifier -> TH.TypeQ
    cToHs ctx cTy = do
      mbHsTy <- convertType purity (ctxTypesTable ctx) cTy
      case mbHsTy of
        Nothing -> fail $ "Could not resolve Haskell type for C type " ++ pretty80 cTy
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
