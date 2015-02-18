{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Language.C.Inline
    ( -- * Build process
      -- $building

      -- * Context
      -- $context
      module Language.C.Context
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

      -- * Direct handling
      --
      -- | The functions in this section let us access more the C file
      -- associated with the current module.  They can be used to build
      -- additional features on top of the basic machinery, and make no 
      -- use of the 'Context'.

      -- ** Emitting C code
    , emitLiteral
    , emitInclude
    , emitCode

      -- ** Embedding C code
      -- $embedding
    , Code(..)
    , embedCode
    , embedExp
    , embedItems
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
import           Data.Char (isSpace)

import           Language.C.Context

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
    error "setContext: the module has already been initialised."
  initialiseModuleState $ Just ctx
  return []

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

-- | Data type representing a list of C definitions with a typed and named entry
-- function.
--
-- We use it as a basis to embed and call C code.
data Code = Code
  { codeCallSafety :: TH.Safety
    -- ^ Safety of the foreign call
  , codeType :: TH.TypeQ
    -- ^ Type of the foreign call
  , codeFunName :: String
    -- ^ Name of the function to call in the code below.
  , codeDefs :: [C.Definition]
    -- ^ The C code.
  }

-- TODO use the #line CPP macro to have the functions in the C file
-- refer to the source location in the Haskell file they come from.
--
-- See <https://gcc.gnu.org/onlinedocs/cpp/Line-Control.html>.

-- | Embeds a piece of code inline.  The resulting 'TH.Exp' will have
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

-- | Same as 'embedItems', but with a single expression.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(embedExp
--   TH.Unsafe
--   [t| Double -> Double |]
--   [cty| double |] [cparams| double x |]
--   [cexp| cos(x) |])
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
  embedItems callSafety type_ cRetType cParams cItems
  where
    cItems = if cRetType == [C.cty| void |]
             then [C.citems| $exp:cExp; |]
             else [C.citems| return $exp:cExp; |]

-- | Same as 'embedCode', but accepts a list of 'C.BlockItem's instead than a
-- full-blown 'Code'.  A function containing the provided statement will be
-- automatically generated.
--
-- @
-- c_cos :: Double -> Double
-- c_cos = $(embedItems
--   TH.Unsafe
--   [t| Double -> Double |]
--   [cty| double |] [cparams| double x |]
--   [citems| return cos(x); |])
-- @
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
-- The functions below are the main interface to this library, and let
-- you easily embed C code in Haskell.
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
-- * The syntax of the @\<C code\>@ depends on the quasi-quoter used,
--   but generally speaking it will be either a C expression of the
--   specified return type or a list of C statements @return@ing
--   something of the specified return type.
--
-- The Haskell type of the inlined expression will be determined by the
-- return type specified.  The conversion between the C type and the
-- Haskell type is performed according to the current 'Context' -- see
-- 'ctxConvertCTypeSpec'.  Moreover, the type will be in 'IO' by
-- default, but "@pure@" quasi-quoters are provided if the C code is
-- pure.  Obviously this facility should be used with care, since if the
-- code is not pure you can break referential transparency.
--
-- Similarly, when capturing Haskell variables using the parameters
-- list, their type is assumed to be of the Haskell type corresponding
-- to the C type provided.  For example, if we capture variable @x@
-- using @double x@ in the parameter list, the code will expect a
-- variable @x@ of type @CDouble@ in Haskell.
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
-- c_cos :: CDouble -> CDouble
-- c_cos x = [cexp_pure_unsafe| double(double x) { cos(x) } |]
-- @
--
-- === Inline C statements
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import qualified Data.Vector.Storable.Mutable as V
-- import           Foreign.C.Types
-- import           Language.C.Inline
--
-- emitInclude "\<stdio.h\>"
--
-- parseVector :: CInt -> IO (V.IOVector CDouble)
-- parseVector len = do
--   vec <- V.new $ fromIntegral len0
--   V.unsafeWith vec $ \\ptr -> [citems| void(int len, double *ptr) {
--     int i;
--     for (i = 0; i < len; i++) {
--       scanf("%lf ", &ptr[i]);
--     }
--   } |]
--   return vec
-- @

cexp :: TH.QuasiQuoter
cexp = genericQuote False C.parseExp $ embedExp TH.Safe

cexp_unsafe :: TH.QuasiQuoter
cexp_unsafe = genericQuote False C.parseExp $ embedExp TH.Unsafe

cexp_pure :: TH.QuasiQuoter
cexp_pure = genericQuote True C.parseExp $ embedExp TH.Safe

cexp_pure_unsafe :: TH.QuasiQuoter
cexp_pure_unsafe = genericQuote True C.parseExp $ embedExp TH.Unsafe

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
  initialiseModuleState_
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
  bodyStr <- restOfInputNoBrace
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

    restOfInputNoBrace :: Parsec.Parser String
    restOfInputNoBrace = do
      inp <- Parsec.many1 $ Parsec.satisfy $ \_ -> True
      let revInp = dropWhile isSpace $ reverse inp
      case revInp of
        '}' : revInp' -> return $ reverse revInp'
        _ -> fail "No closing brace!"

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
