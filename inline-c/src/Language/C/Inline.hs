{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The main goal of this module is to allow painless embedding of C
-- code in Haskell code.  If you're interested in how to use the
-- library, skip to the "Inline C" section.  To build, read the first
-- two sections.
module Language.C.Inline
    ( -- * Build process
      -- $building

      -- * Context
      -- $context
      module Language.C.Inline.Context
    , setContext

      -- * Inline C
      -- $quoting
    , cexp
    , cexp_unsafe
    , cexp_pure
    , cexp_pure_unsafe
    , citems
    , citems_unsafe
    , citems_pure
    , citems_pure_unsafe

      -- * 'FunPtr' utils
      --
      -- | The functions in this section provide facilities to quickly
      -- get 'FunPtr's from Haskell functions, and vice-versa.
    , mkFunPtr
    , peekFunPtr

      -- * Low-level API
      --
      -- | The functions in this section let us access more the C file
      -- associated with the current module.  They can be used to build
      -- additional features on top of the basic machinery.

      -- ** Emitting C code
    , emitLiteral
    , include

      -- ** Inlining C code
      -- $embedding
    , Code(..)
    , inlineCode
    , inlineExp
    , inlineItems
    ) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (void, unless, forM)
import           Data.Foldable (forM_)
import           Data.Functor ((<$>))
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import           System.Directory (removeFile)
import           System.FilePath (addExtension, dropExtension)
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Map as Map
import qualified Crypto.Hash as CryptoHash
import qualified Data.Binary as Binary

import qualified Language.C.Types as C
import           Language.C.Inline.Context
import           Language.C.Inline.Parse
import           Language.C.Inline.FunPtr

------------------------------------------------------------------------
-- Module compile-time state

-- $building
--
-- Each module that uses at least one of the TH functions in this module
-- gets a C file associated to it, where the filename of said file will
-- be the same as the module but with a C extension.  This C file must
-- be built after the Haskell code and linked appropriately.  If you use
-- cabal, all you have to do is declare each associated C file in the
-- @.cabal@ file and you are good.
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
--
-- If we were to compile the above manaully we could do
--
-- @
-- $ ghc -c Main.hs
-- $ cc -c Main.c -o Main_c.o
-- $ ghc Foo.hs
-- $ ghc Bar.hs
-- $ cc -c Bar.c -o Bar_c.o
-- $ ghc Main.o Foo.o Bar.o Main_c.o Bar_c.o -lm -o Main
-- @
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
          , msGeneratedNames = 0
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

-- $context
--
-- The inline C functions ('cexp', 'citems', etc.) need a 'Context' to
-- operate.  Said context can be explicitely set with 'setContext'.
-- Otherwise, at the first usage of one of the TH functions in this
-- module the 'Context' is implicitely set to 'baseCtx'.

-- | Sets the 'Context' for the current module.  This function, if
-- called, must be called before any of the other TH functions in this
-- module.  Fails if that's not the case.
setContext :: Context -> TH.DecsQ
setContext ctx = do
  mbModuleState <- TH.runIO $ readIORef moduleStateRef
  forM_ mbModuleState $ \_moduleState -> do
    error "inline-c: The module has already been initialised (setContext)."
  initialiseModuleState $ Just ctx
  return []

bumpGeneratedNames :: TH.Q Int
bumpGeneratedNames = do
  ms <- getModuleState
  TH.runIO $ do
    let c = msGeneratedNames ms
    writeIORef moduleStateRef $ Just ms{msGeneratedNames = c + 1}
    return c

------------------------------------------------------------------------
-- Emitting

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

-- | Emits an include CPP statement for the given file.
-- To avoid having to escape quotes, the function itself adds them when
-- appropriate, so that
--
-- @
-- include "foo.h" ==> #include "foo.h"
-- @
--
-- but
--
-- @
-- include \<foo\> ==> #include \<foo\>
-- @
include :: String -> TH.DecsQ
include s
  | null s = error "inline-c: empty string (include)"
  | head s == '<' = emitLiteral $ "#include " ++ s
  | otherwise = emitLiteral $ "#include \"" ++ s ++ "\""

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
    -- ^ Safety of the foreign call
  , codeType :: TH.TypeQ
    -- ^ Type of the foreign call
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
--   [C.cunit| int francescos_add(int x, int y) { int z = x + y; return z; } |])
-- @
inlineCode :: Code -> TH.ExpQ
inlineCode Code{..} = do
  initialiseModuleState_         -- Make sure that things are up-to-date
  -- Write out definitions
  void $ emitLiteral codeDefs
  -- Create and add the FFI declaration.
  ffiImportName <- uniqueFfiImportName
  dec <- TH.forImpD TH.CCall codeCallSafety codeFunName ffiImportName codeType
  TH.addTopDecls [dec]
  TH.varE ffiImportName

uniqueCName :: String -> TH.Q String
uniqueCName x = do
  c <- bumpGeneratedNames
  let unique :: CryptoHash.Digest CryptoHash.SHA1 = CryptoHash.hashlazy $ Binary.encode x
  return $ "inline_c_" ++ show c ++ "_" ++ show unique

-- | Same as 'inlineItems', but with a single expression.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(inlineExp
--   TH.Unsafe
--   [t| Double -> Double |]
--   [cty| double |] [cparams| double x |]
--   [cexp| cos(x) |])
-- @
inlineExp
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type
  -- ^ Return type of the C expr
  -> [(C.Id, C.Type)]
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

-- | Same as 'inlineCode', but accepts a list of 'C.BlockItem's instead than a
-- full-blown 'Code'.  A function containing the provided statement will be
-- automatically generated.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(inlineItems
--   TH.Unsafe
--   [t| Double -> Double |]
--   [cty| double |] [cparams| double x |]
--   [citems| return cos(x); |])
-- @
inlineItems
  :: TH.Safety
  -- ^ Safety of the foreign call
  -> TH.TypeQ
  -- ^ Type of the foreign call
  -> C.Type
  -- ^ Return type of the C expr
  -> [(C.Id, C.Type)]
  -- ^ Parameters of the C expr
  -> String
  -- ^ The C items
  -> TH.ExpQ
inlineItems callSafety type_ cRetType cParams cItems = do
  let mkParam (id', paramTy) = C.ParameterDeclaration (Just id') paramTy
  let proto = C.Proto cRetType (map mkParam cParams)
  funName <- uniqueCName $ show proto ++ cItems
  let decl = C.ParameterDeclaration (Just (C.Id funName)) proto
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
-- Quoting sugar

-- $quoting
--
-- The functions below are the main interface to this library, and let
-- you easily inline C code in Haskell.
--
-- In general, they are used like so:
--
-- @
-- [cXXX| int(double x, float y) { \<C code\> } |]
-- @
--
-- Where @cXXX@ is one of the quasi-quoters defined in this section.
--
-- The syntax is essentially representing an anonymous C function:
--
-- * The first type to appear (@int@ in the example) is the return type
--   of said function.
--
-- * The arguments list (@(double x, float y)@ in the example) captures
--   Haskell variables currently in scope, and makes them available from
--   the C code.  If no parameters are present, the parentheses can be
--   omitted.
--
-- * The syntax of the @\<C code\>@ depends on the quasi-quoter used.
--   @cexp@ functions accept a C expression.  @citems@ functions accept
--   'C.BlockItem's, which are basically what goes inside a C
--   function.
--
-- === Variable capturing and type conversion.
--
-- The Haskell type of the inlined expression will be determined by the
-- C return type specified.  The conversion between the C type and the
-- Haskell type is performed according to the current 'Context' -- see
-- 'ctxConvertCTypeSpec'.  C pointers and arrays are both converted to
-- Haskell @'Ptr'@s, and function pointers are converted to @'FunPtr'@s.
-- Sized arrays are not supported.
--
-- Similarly, when capturing Haskell variables using the parameters
-- list, their type is assumed to be of the Haskell type corresponding
-- to the C type provided.  For example, if we capture variable @x@
-- using @double x@ in the parameter list, the code will expect a
-- variable @x@ of type @CDouble@ in Haskell.
--
-- === @pure@ and impure calls
--
-- Both @cexp@ and @citems@ quasi-quoters are present in impure (the
-- default) and pure (postfixed with @_pure@) versions.  The impure
-- version will generate expressions of type @'IO' a@, where @a@ is the
-- specified return type.  On the other hand, @pure@ versions will
-- generate pure code.  Moreover, this difference will also carry over
-- to function pointers.  Impure quasi-quoters will convert C function
-- pointers to @'IO'@ functions in Haskell.  For example, if an argument
-- is of type @int (*add)(int, int)@, the impure quasi-quoters will
-- expect a @'FunPtr' ('CInt' -> 'CInt' -> 'IO' 'CInt')@, while the pure
-- ones a @'FunPtr' ('CInt' -> 'CInt' -> 'IO' 'CInt')@.
--
-- Obviously pure quoters should be used with care, since if the C code
-- is not pure you can break referential transparency.
--
-- === Safe and @unsafe@ calls
--
-- Finally, @unsafe@ variants of the quasi-quoters are provided to call
-- the C code unsafely, in the sense that the C code will block the RTS,
-- with the advantage of a faster call to the foreign code. See
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1590008.4.3>
-- for more info.
--
-- == Examples
--
-- === Inline C expression
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import           "Foreign.C.Types"
-- import           "Language.C.Inline"
--
-- 'include' "\<math.h\>"
--
-- c_cos :: 'CDouble' -> 'CDouble'
-- c_cos x = ['cexp_pure_unsafe'| double(double x) { cos(x) } |]
-- @
--
-- === Inline C statements
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import qualified Data.Vector.Storable.Mutable as V
-- import           "Foreign.C.Types"
-- import           "Language.C.Inline"
--
-- 'include' "\<stdio.h\>"
--
-- parseVector :: 'CInt' -> 'IO' (V.IOVector 'CDouble')
-- parseVector len = do
--   vec <- V.new $ 'fromIntegral' len0
--   V.unsafeWith vec $ \\ptr -> ['citems'| void(int len, double *ptr) {
--     int i;
--     for (i = 0; i < len; i++) {
--       scanf("%lf ", &ptr[i]);
--     }
--   } |]
--   'return' vec
-- @

cexp :: TH.QuasiQuoter
cexp = genericQuote IO $ inlineExp TH.Safe

cexp_unsafe :: TH.QuasiQuoter
cexp_unsafe = genericQuote IO $ inlineExp TH.Unsafe

cexp_pure :: TH.QuasiQuoter
cexp_pure = genericQuote Pure $ inlineExp TH.Safe

cexp_pure_unsafe :: TH.QuasiQuoter
cexp_pure_unsafe = genericQuote Pure $ inlineExp TH.Unsafe

citems :: TH.QuasiQuoter
citems = genericQuote IO $ inlineItems TH.Safe

citems_unsafe :: TH.QuasiQuoter
citems_unsafe = genericQuote IO $ inlineItems TH.Unsafe

citems_pure :: TH.QuasiQuoter
citems_pure = genericQuote Pure $ inlineItems TH.Safe

citems_pure_unsafe :: TH.QuasiQuoter
citems_pure_unsafe = genericQuote Pure $ inlineItems TH.Unsafe

quoteCode
  :: (String -> TH.ExpQ)
  -- ^ The parser
  -> TH.QuasiQuoter
quoteCode p = TH.QuasiQuoter
  { TH.quoteExp = p
  , TH.quotePat = error "inline-c: quotePat not implemented (quoteCode)"
  , TH.quoteType = error "inline-c: quoteType not implemented (quoteCode)"
  , TH.quoteDec = error "inline-c: quoteDec not implemeted (quoteCode)"
  }

genericQuote
  :: Purity
  -- ^ Whether the call and the function pointers should be pure or not.
  -> (TH.TypeQ -> C.Type -> [(C.Id, C.Type)] -> String -> TH.ExpQ)
  -- ^ Function taking that something and building an expression, see
  -- 'inlineExp' for other args.
  -> TH.QuasiQuoter
genericQuote pure build = quoteCode $ \s -> do
  initialiseModuleState_
  ctx <- getContext
  ParseTypedC cType cParams cExp <-
    runParserInQ s (isTypeName (ctxCTypesTable ctx)) $ parseTypedC $ ctxCAntiQuoters ctx
  hsType <- cToHs ctx cType
  hsParams <- forM cParams $ \(cId, cTy, parTy) -> do
    case parTy of
      Plain s' -> do
        hsTy <- cToHs ctx cTy
        mbHsName <- TH.lookupValueName s'
        hsExp <- case mbHsName of
          Nothing -> do
            error $ "Cannot capture Haskell variable " ++ show cId ++
                    ", because it's not in scope. (genericQuote)"
          Just hsName -> do
            hsExp <- TH.varE hsName
            case pure of
              Pure -> return hsExp
              IO -> [| \cont -> cont $(return hsExp) |]
        return (hsTy, hsExp)
      AntiQuote antiId dyn -> do
        case Map.lookup antiId (ctxCAntiQuoters ctx) of
          Nothing ->
            error $ "IMPOSSIBLE: could not find anti-quoter " ++ show antiId ++
                    ". (genericQuote)"
          Just (SomeCAntiQuoter antiQ) -> case fromSomeEq dyn of
            Nothing ->
              error  $ "IMPOSSIBLE: could not cast value for anti-quoter " ++
                       show antiId ++ ". (genericQuote)"
            Just x ->
              caqMarshaller antiQ (ctxCTypesTable ctx) pure cTy x
  let hsFunType = convertCFunSig hsType $ map fst hsParams
  let cParams' = [(cId, cTy) | (cId, cTy, _) <- cParams]
  buildFunCall ctx (build hsFunType cType cParams' cExp) (map snd hsParams) []
  where
    cToHs :: Context -> C.Type -> TH.TypeQ
    cToHs ctx cTy = do
      mbHsTy <- convertCType (ctxCTypesTable ctx) pure cTy
      case mbHsTy of
        Nothing -> error $ "Could not resolve Haskell type for C type " ++ pretty80 cTy
        Just hsTy -> return hsTy

    buildFunCall :: Context -> TH.ExpQ -> [TH.Exp] -> [TH.Name] -> TH.ExpQ
    buildFunCall _ctx f [] args =
      foldl (\f' arg -> [| $f' $(TH.varE arg) |]) f args
    buildFunCall ctx f (hsExp : params) args = do
      case pure of
        Pure -> [|
           let arg = $(return hsExp)
           in $(buildFunCall ctx f params (args ++ ['arg]))
          |]
        IO -> [| $(return hsExp) $ \arg ->
            $(buildFunCall ctx f params (args ++ ['arg]))
          |]

    convertCFunSig :: TH.Type -> [TH.Type] -> TH.TypeQ
    convertCFunSig retType params0 = do
      go params0
      where
        go [] = case pure of
          Pure -> return retType
          IO -> [t| IO $(return retType) |]
        go (paramType : params) = do
          [t| $(return paramType) -> $(go params) |]

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""

prettyOneLine :: PP.Pretty a => a -> String
prettyOneLine x = PP.displayS (PP.renderCompact (PP.pretty x)) ""
