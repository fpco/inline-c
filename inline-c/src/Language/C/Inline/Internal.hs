{-# LANGUAGE CPP #-}
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
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MonoLocalBinds #-}

module Language.C.Inline.Internal
    ( -- * Context handling
      setContext
    , getContext

      -- * Substitution
    , Substitutions(..)
    , substitute
    , getHaskellType

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
    , splitTypedC

      -- * Utility functions for writing quasiquoters
    , genericQuote
    , funPtrQuote
    ) where

import           Control.Applicative
import           Control.Monad (forM, void, msum)
import           Control.Monad.State (evalStateT, StateT, get, put)
import           Control.Monad.Trans.Class (lift)
import           Data.Foldable (forM_)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Traversable (for)
import           Data.Typeable (Typeable, cast)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import           System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Parser.Char as Parser
import qualified Text.Parser.Combinators as Parser
import qualified Text.Parser.LookAhead as Parser
import qualified Text.Parser.Token as Parser
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.List as L
import qualified Data.Char as C
import           Data.Hashable (Hashable)
import           Foreign.Ptr (FunPtr)
import qualified Data.Map as M

-- We cannot use getQ/putQ before 7.10.3 because of <https://ghc.haskell.org/trac/ghc/ticket/10596>
#define USE_GETQ (__GLASGOW_HASKELL__ > 710 || (__GLASGOW_HASKELL__ == 710 && __GLASGOW_HASKELL_PATCHLEVEL1__ >= 3))

#if !USE_GETQ
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
#endif

import           Language.C.Inline.Context
import           Language.C.Inline.FunPtr
import           Language.C.Inline.HaskellIdentifier
import qualified Language.C.Types as C

data ModuleState = ModuleState
  { msContext :: Context
  , msGeneratedNames :: Int
  , msFileChunks :: [String]
  } deriving (Typeable)

getModuleState :: TH.Q (Maybe ModuleState)
putModuleState :: ModuleState -> TH.Q ()

#if USE_GETQ

getModuleState = TH.getQ
putModuleState = TH.putQ

#else

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

getModuleState = do
  moduleStates <- TH.runIO (readMVar moduleStatesVar)
  moduleId <- getModuleId
  return (Map.lookup moduleId moduleStates)

putModuleState ms = do
  moduleId <- getModuleId
  TH.runIO (modifyMVar_ moduleStatesVar (return . Map.insert moduleId ms))

#endif


-- | Make sure that 'moduleStatesVar' and the respective C file are up
--   to date.
initialiseModuleState
  :: Maybe Context
  -- ^ The 'Context' to use if we initialise the module.  If 'Nothing',
  -- 'baseCtx' will be used.
  -> TH.Q Context
initialiseModuleState mbContext = do
  mbModuleState <- getModuleState
  case mbModuleState of
    Just moduleState -> return (msContext moduleState)
    Nothing -> do
      -- Add hook to add the file
      TH.addModFinalizer $ do
        mbMs <- getModuleState
        ms <- case mbMs of
          Nothing -> fail "inline-c: ModuleState not present (initialiseModuleState)"
          Just ms -> return ms
        let lang = fromMaybe TH.LangC (ctxForeignSrcLang context)
#if MIN_VERSION_base(4,12,0)
        TH.addForeignSource lang (concat (reverse (msFileChunks ms)))
#else
        TH.addForeignFile lang (concat (reverse (msFileChunks ms)))
#endif
      let moduleState = ModuleState
            { msContext = context
            , msGeneratedNames = 0
            , msFileChunks = mempty
            }
      putModuleState moduleState
      return context
  where
    context = fromMaybe baseCtx mbContext

-- | Gets the current 'Context'.  Also makes sure that the current
-- module is initialised.
getContext :: TH.Q Context
getContext = initialiseModuleState Nothing

modifyModuleState :: (ModuleState -> (ModuleState, a)) -> TH.Q a
modifyModuleState f = do
  mbModuleState <- getModuleState
  case mbModuleState of
    Nothing -> fail "inline-c: ModuleState not present (modifyModuleState)"
    Just ms -> do
      let (ms', x) = f ms
      putModuleState ms'
      return x

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
  mbModuleState <- getModuleState
  forM_ mbModuleState $ \_ms ->
    fail "inline-c: The module has already been initialised (setContext)."
  void $ initialiseModuleState $ Just ctx

bumpGeneratedNames :: TH.Q Int
bumpGeneratedNames = do
  modifyModuleState $ \ms ->
    let c' = msGeneratedNames ms
    in (ms{msGeneratedNames = c' + 1}, c')

------------------------------------------------------------------------
-- Emitting

-- | Simply appends some string to the module's C file.  Use with care.
emitVerbatim :: String -> TH.DecsQ
emitVerbatim s = do
  -- Make sure that the 'ModuleState' is initialized
  void (initialiseModuleState Nothing)
  let chunk = "\n" ++ s ++ "\n"
  modifyModuleState $ \ms ->
    (ms{msFileChunks = chunk : msFileChunks ms}, ())
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
  , codeLoc :: Maybe TH.Loc
    -- ^ The haskell source location used for the #line directive
  , codeType :: TH.TypeQ
    -- ^ Type of the foreign call.
  , codeFunName :: String
    -- ^ Name of the function to call in the code below.
  , codeDefs :: String
    -- ^ The C code.
  , codeFunPtr :: Bool
    -- ^ If 'True', the type will be wrapped in 'FunPtr', and
    -- the call will be static (e.g. prefixed by &).
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
-- c_add = $(do
--   here <- TH.location
--   inlineCode $ Code
--     TH.Unsafe                   -- Call safety
--     (Just here)
--     [t| Int -> Int -> Int |]    -- Call type
--     "francescos_add"            -- Call name
--     -- C Code
--     \"int francescos_add(int x, int y) { int z = x + y; return z; }\")
-- @
inlineCode :: Code -> TH.ExpQ
inlineCode Code{..} = do
  -- Write out definitions
  ctx <- getContext
  let out = fromMaybe id $ ctxOutput ctx
  let directive = maybe "" (\l -> "#line " ++ show (fst $ TH.loc_start l) ++ " " ++ show (TH.loc_filename l ) ++ "\n") codeLoc
  void $ emitVerbatim $ out $ directive ++ codeDefs
  -- Create and add the FFI declaration.
  ffiImportName <- uniqueFfiImportName
  dec <- if codeFunPtr
    then
      TH.forImpD TH.CCall codeCallSafety ("&" ++ codeFunName) ffiImportName [t| FunPtr $(codeType) |]
    else TH.forImpD TH.CCall codeCallSafety codeFunName ffiImportName codeType
  TH.addTopDecls [dec]
  TH.varE ffiImportName

uniqueCName :: Maybe String -> TH.Q String
uniqueCName mbPostfix = do
  -- The name looks like this:
  -- inline_c_MODULE_INDEX_POSTFIX
  --
  -- Where:
  --  * MODULE is the module name but with _s instead of .s;
  --  * INDEX is a counter that keeps track of how many names we're generating
  --    for each module.
  --  * POSTFIX is an optional postfix to ease debuggability
  --
  -- we previously also generated a hash from the contents of the
  -- C code because of problems when cabal recompiled but now this
  -- is not needed anymore since we use 'addDependentFile' to compile
  -- the C code.
  c' <- bumpGeneratedNames
  module_ <- TH.loc_module <$> TH.location
  let replaceDot '.' = '_'
      replaceDot c = c
  let postfix = case mbPostfix of
        Nothing -> ""
        Just s -> "_" ++ s ++ "_"
  return $ "inline_c_" ++ map replaceDot module_ ++ "_" ++ show c' ++ postfix

-- | Same as 'inlineCItems', but with a single expression.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(do
--   here <- TH.location
--   inlineExp
--     TH.Unsafe
--     here
--     [t| Double -> Double |]
--     (quickCParser_ \"double\" parseType)
--     [("x", quickCParser_ \"double\" parseType)]
--     "cos(x)")
-- @
inlineExp
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.Loc
  -- ^ The location to report
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type C.CIdentifier
  -- ^ Return type of the C expr
  -> [(C.CIdentifier, C.Type C.CIdentifier)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C expression
  -> TH.ExpQ
inlineExp callSafety loc type_ cRetType cParams cExp =
  inlineItems callSafety False Nothing loc type_ cRetType cParams cItems
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
-- c_cos = $(do
--  here <- TH.location
--  inlineItems
--   TH.Unsafe
--   False
--   Nothing
--   here
--   [t| Double -> Double |]
--   (quickCParser_ \"double\" parseType)
--   [("x", quickCParser_ \"double\" parseType)]
--   "return cos(x);")
-- @
inlineItems
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> Bool
  -- ^ Whether to return as a FunPtr or not
  -> Maybe String
  -- ^ Optional postfix for the generated name
  -> TH.Loc
  -- ^ The location to report
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type C.CIdentifier
  -- ^ Return type of the C expr
  -> [(C.CIdentifier, C.Type C.CIdentifier)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C items
  -> TH.ExpQ
inlineItems callSafety funPtr mbPostfix loc type_ cRetType cParams cItems = do
  let mkParam (id', paramTy) = C.ParameterDeclaration (Just id') paramTy
  let proto = C.Proto cRetType (map mkParam cParams)
  ctx <- getContext
  funName <- uniqueCName mbPostfix
  cFunName <- case C.cIdentifierFromString (ctxEnableCpp ctx) funName of
    Left err -> fail $ "inlineItems: impossible, generated bad C identifier " ++
                       "funName:\n" ++ err
    Right x -> return x
  let decl = C.ParameterDeclaration (Just cFunName) proto
  let defs = prettyOneLine decl ++ " { " ++ cItems ++ " }\n"
  inlineCode $ Code
    { codeCallSafety = callSafety
    , codeLoc = Just loc
    , codeType = type_
    , codeFunName = funName
    , codeDefs = defs
    , codeFunPtr = funPtr
    }

------------------------------------------------------------------------
-- Parsing

runParserInQ
  :: (Hashable ident)
  => String
  -> C.CParserContext ident
  -> (forall m. C.CParser ident m => m a) -> TH.Q a
runParserInQ s ctx p = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let parsecLoc = Parsec.newPos (TH.loc_filename loc) line col
  let p' = lift (Parsec.setPosition parsecLoc) *> p <* lift Parser.eof
  case C.runCParser ctx (TH.loc_filename loc) s p' of
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

newtype Substitutions = Substitutions { unSubstitutions :: M.Map String (String -> String) }

applySubstitutions :: String -> TH.Q String
applySubstitutions str = do
  subs <- maybe mempty unSubstitutions <$> TH.getQ
  let substitution = msum $ flip map (M.toList subs) $ \( subName, subFunc ) ->
        Parsec.try $ do
          _ <- Parsec.string ('@' : subName ++ "(")
          subArg <- Parsec.manyTill Parsec.anyChar (Parsec.char ')')
          return (subFunc subArg)
  let someChar = (:[]) <$> Parsec.anyChar
  case Parsec.parse (many (substitution <|> someChar)) "" str of
    Left _ -> fail "Substitution failed (should be impossible)"
    Right chunks -> return (concat chunks)

-- | Define macros that can be used in the nested Template Haskell expression.
-- Macros can be used as @\@MACRO_NAME(input)@ in inline-c quotes, and will transform their input with the given function.
-- They can be useful for passing in types when defining Haskell instances for C++ template types.
substitute :: [ ( String, String -> String ) ] -> TH.Q a -> TH.Q a
substitute subsList cont = do
  oldSubs <- maybe mempty unSubstitutions <$> TH.getQ
  let subs = M.fromList subsList
  let conflicting = M.intersection subs oldSubs
  newSubs <-
    if M.null conflicting
      then return (Substitutions (M.union oldSubs subs))
      else fail ("Conflicting substitutions `" ++ show (M.keys conflicting) ++ "`")
  TH.putQ newSubs *> cont <* TH.putQ (Substitutions oldSubs)

-- | Given a C type name, return the Haskell type in Template Haskell. The first parameter controls whether function pointers
-- should be mapped as pure or IO functions.
getHaskellType :: Bool -> String -> TH.TypeQ
getHaskellType pureFunctions cTypeStr = do
  ctx <- getContext
  let cParseCtx = C.cCParserContext (ctxEnableCpp ctx) (typeNamesFromTypesTable (ctxTypesTable ctx))
  cType <- runParserInQ cTypeStr cParseCtx C.parseType
  cToHs ctx (if pureFunctions then Pure else IO) cType

-- To parse C declarations, we're faced with a bit of a problem: we want
-- to parse the anti-quotations so that Haskell identifiers are
-- accepted, but we want them to appear only as the root of
-- declarations.  For this reason, we parse allowing Haskell identifiers
-- everywhere, and then we "purge" Haskell identifiers everywhere but at
-- the root.
parseTypedC
  :: forall m. C.CParser HaskellIdentifier m
  => Bool -> AntiQuoters -> m ParseTypedC
  -- ^ Returns the return type, the captured variables, and the body.
parseTypedC useCpp antiQs = do
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
      id' <- freshId $ mangleHaskellIdentifier useCpp hId
      void $ Parser.char ')'
      return ([(id', declType, Plain hId)], C.unCIdentifier id')

    freshId s = do
      c <- get
      put $ c + 1
      case C.cIdentifierFromString useCpp (C.unCIdentifier s ++ "_inline_c_" ++ show c) of
        Left _err -> error "freshId: The impossible happened"
        Right x -> return x

    -- The @m@ is polymorphic because we use this both for the plain
    -- parser and the StateT parser we use above.  We only need 'fail'.
    purgeHaskellIdentifiers
#if MIN_VERSION_base(4,13,0)
      :: forall n. MonadFail n
#else
      :: forall n. (Applicative n, Monad n)
#endif
      => C.Type HaskellIdentifier -> n (C.Type C.CIdentifier)
    purgeHaskellIdentifiers cTy = for cTy $ \hsIdent -> do
      let hsIdentS = unHaskellIdentifier hsIdent
      case C.cIdentifierFromString useCpp hsIdentS of
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
  , TH.quotePat = const $ fail "inline-c: quotePat not implemented (quoteCode)"
  , TH.quoteType = const $ fail "inline-c: quoteType not implemented (quoteCode)"
  , TH.quoteDec = const $ fail "inline-c: quoteDec not implemented (quoteCode)"
  }

cToHs :: Context -> Purity -> C.Type C.CIdentifier -> TH.TypeQ
cToHs ctx purity cTy = do
  mbHsTy <- convertType purity (ctxTypesTable ctx) cTy
  case mbHsTy of
    Nothing -> fail $ "Could not resolve Haskell type for C type " ++ pretty80 cTy
    Just hsTy -> return hsTy

genericQuote
  :: Purity
  -> (TH.Loc -> TH.TypeQ -> C.Type C.CIdentifier -> [(C.CIdentifier, C.Type C.CIdentifier)] -> String -> TH.ExpQ)
  -- ^ Function building an Haskell expression, see 'inlineExp' for
  -- guidance on the other args.
  -> TH.QuasiQuoter
genericQuote purity build = quoteCode $ \rawStr -> do
    ctx <- getContext
    here <- TH.location
    s <- applySubstitutions rawStr
    ParseTypedC cType cParams cExp <-
      runParserInQ s
        (haskellCParserContext (ctxEnableCpp ctx) (typeNamesFromTypesTable (ctxTypesTable ctx)))
        (parseTypedC (ctxEnableCpp ctx) (ctxAntiQuoters ctx))
    hsType <- cToHs ctx purity cType
    hsParams <- forM cParams $ \(_cId, cTy, parTy) -> do
      case parTy of
        Plain s' -> do
          hsTy <- cToHs ctx purity cTy
          let hsName = TH.mkName (unHaskellIdentifier s')
          hsExp <- [| \cont -> cont ($(TH.varE hsName) :: $(return hsTy)) |]
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
    ioCall <- buildFunCall ctx (build here hsFunType cType cParams' cExp) (map snd hsParams) []
    -- If the user requested a pure function, make it so.
    case purity of
      Pure -> [| unsafeDupablePerformIO $(return ioCall) |]
      IO -> return ioCall
  where
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

splitTypedC :: String -> (String, String)
  -- ^ Returns the type and the body separately
splitTypedC s = (trim ty, case body of
                            [] -> []
                            r  -> r)
  where (ty, body) = span (/= '{') s
        trim x = L.dropWhileEnd C.isSpace (dropWhile C.isSpace x)

-- | Data to parse for the 'funPtr' quasi-quoter.
data FunPtrDecl = FunPtrDecl
  { funPtrReturnType :: C.Type C.CIdentifier
  , funPtrParameters :: [(C.CIdentifier, C.Type C.CIdentifier)]
  , funPtrBody :: String
  , funPtrName :: Maybe String
  } deriving (Eq, Show)

funPtrQuote :: TH.Safety -> TH.QuasiQuoter
funPtrQuote callSafety = quoteCode $ \rawCode -> do
  loc <- TH.location
  ctx <- getContext
  code <- applySubstitutions rawCode
  FunPtrDecl{..} <- runParserInQ code (C.cCParserContext (ctxEnableCpp ctx) (typeNamesFromTypesTable (ctxTypesTable ctx))) parse
  hsRetType <- cToHs ctx IO funPtrReturnType
  hsParams <- forM funPtrParameters (\(_ident, typ_) -> cToHs ctx IO typ_)
  let hsFunType = convertCFunSig hsRetType hsParams
  inlineItems callSafety True funPtrName loc hsFunType funPtrReturnType funPtrParameters funPtrBody
  where
    convertCFunSig :: TH.Type -> [TH.Type] -> TH.TypeQ
    convertCFunSig retType params0 = do
      go params0
      where
        go [] =
          [t| IO $(return retType) |]
        go (paramType : params) = do
          [t| $(return paramType) -> $(go params) |]

    parse :: C.CParser C.CIdentifier m => m FunPtrDecl
    parse = do
      -- skip spaces
      Parser.spaces
      -- parse a proto
      C.ParameterDeclaration mbName protoTyp <- C.parseParameterDeclaration
      case protoTyp of
        C.Proto retType paramList -> do
          args <- forM paramList $ \decl -> case C.parameterDeclarationId decl of
            Nothing -> fail $ pretty80 $
              "Un-named captured variable in decl" <+> PP.pretty decl
            Just declId -> return (declId, C.parameterDeclarationType decl)
          -- get the rest of the body
          void (Parser.symbolic '{')
          body <- parseBody
          return FunPtrDecl
            { funPtrReturnType = retType
            , funPtrParameters = args
            , funPtrBody = body
            , funPtrName = fmap C.unCIdentifier mbName
            }
        _ -> fail $ "Expecting function declaration"

    parseBody :: C.CParser C.CIdentifier m => m String
    parseBody = do
      s <- Parser.manyTill Parser.anyChar $
           Parser.lookAhead (Parser.char '}')
      s' <- msum
        [ do Parser.try $ do -- Try because we might fail to parse the 'eof'
                -- 'symbolic' because we want to consume whitespace
               void $ Parser.symbolic '}'
               Parser.eof
             return ""
        , do void $ Parser.char '}'
             s' <- parseBody
             return ("}" ++ s')
        ]
      return (s ++ s')

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""

prettyOneLine :: PP.Pretty a => a -> String
prettyOneLine x = PP.displayS (PP.renderCompact (PP.pretty x)) ""
