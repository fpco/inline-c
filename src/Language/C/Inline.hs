{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Each module that uses at least one of the TH functions below gets
-- a C file associated to it.  This C file must be built after the
-- Haskell code and linked appropriately.  If you use cabal, all you
-- have to do is declare each associated C file in the @.cabal@ file and
-- you are good.
--
-- For example we might have
--
-- @
-- executable foo
--   main-is:             Main.hs
--   hs-source-dirs:      src
--   -- Here the corresponding C sources must be listed for every module
--   -- that uses C snippets.
--   c-sources:           src/Main.c
--   ...
-- @
module Language.C.Inline
  ( Code(..)
    -- * Emitting
  , emitLiteral
  , emitInclude
  , emitCode
    -- * Embedding
  , embedCode
  , embedStm
  , embedExp
    -- * Quoting
  , cstm
  , cstm_unsafe
  , cstm_pure
  , cstm_pure_unsafe
  , cexp
  , cexp_unsafe
  , cexp_pure
  , cexp_pure_unsafe
  ) where

import           Prelude hiding (lex)

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
import           Control.Monad (void, unless, forM_)
import           System.IO.Error (isDoesNotExistError)
import           System.Directory (removeFile)
import           Data.Functor ((<$>))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec
import qualified Text.Parsec.String as Parsec
import           Foreign.C.Types
import           Data.Loc (Pos(..))
import qualified Data.ByteString.UTF8
import           Control.Applicative ((*>), (<*))

------------------------------------------------------------------------
-- Module compile-time state

-- | Records what module we are in.
{-# NOINLINE currentModuleRef #-}
currentModuleRef :: IORef (Maybe String)
currentModuleRef = unsafePerformIO $ newIORef Nothing

-- | Make sure that 'currentModuleRef' and the respective C file are up
-- to date.
initialiseModuleState :: TH.Q ()
initialiseModuleState = do
  cFile <- cSourceLoc
  mbModule <- TH.runIO $ readIORef currentModuleRef
  thisModule <- TH.loc_module <$> TH.location
  let recordThisModule = TH.runIO $ do
        -- If the file exists and this is the first time we write
        -- something from this module (in other words, if we are
        -- recompiling the module), kill the file first.
        removeIfExists cFile
        writeIORef currentModuleRef $ Just thisModule
  case mbModule of
    Nothing -> recordThisModule
    Just currentModule | currentModule == thisModule -> return ()
    Just _otherModule -> recordThisModule

------------------------------------------------------------------------
-- Emitting

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
  initialiseModuleState         -- Make sure that things are up-to-date
  cFile <- cSourceLoc
  TH.runIO $ appendFile cFile $ "\n" ++ s ++ "\n"
  return []

-- | Emits some definition to the module C file.
emitCode :: [C.Definition] -> TH.DecsQ
emitCode defs = do
  forM_ defs $ \def -> void $ emitLiteral $ show $ PrettyPrint.ppr def
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

-- TODO use the #line CPP macro to have the functions in the C file
-- refer to the source location in the Haskell file they come from.
--
-- See <https://gcc.gnu.org/onlinedocs/cpp/Line-Control.html>.

-- | Embeds a piece of code inline.  The resulting 'TH.Exp' will have
-- the type specified in the 'codeType'.
embedCode :: Code -> TH.ExpQ
embedCode Code{..} = do
  initialiseModuleState         -- Make sure that things are up-to-date
  -- Write out definitions
  void $ emitCode codeDefs
  -- Create and add the FFI declaration.  TODO absurdly, I need to
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
-- cos_of_1 :: Double
-- cos_of_1 = $(embedStm
--   TH.Unsafe
--   [t| Double |]
--   [cty| double |] []
--   [cstm| cos(1) |])
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
  embedStm callSafety type_ cRetType cParams [C.cstm| return $exp:cExp; |]

------------------------------------------------------------------------
-- Quoting sugar

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
  :: Bool
  -- ^ Whether the call should be pure or not
  -> C.P a
  -- ^ Parser producing something
  -> (TH.TypeQ -> C.Type -> [C.Param] -> a -> TH.ExpQ)
  -- ^ Function taking that something and building an expression, see
  -- 'embedExp' for other args.
  -> TH.QuasiQuoter
genericQuote pure p build = quoteCode $ \s -> do
  (cType, cParams, cExp) <- runCParser s $ parseTypedC p
  let cParams' = map cleanupParam cParams
  let hsType = cFunSigToHsType pure cType $ map snd cParams'
  buildFunCall (build hsType cType cParams cExp) $ map fst cParams'
  where
    buildFunCall :: TH.ExpQ -> [Maybe C.Id] -> TH.ExpQ
    buildFunCall f [] =
      f
    buildFunCall f (mbParamId : params) = case mbParamId of
      Nothing -> do
        error "Cannot capture Haskell variable if you don't give a name."
      Just name -> do
        mbHsName <- TH.lookupValueName $ case name of
          C.Id s _ -> s
          C.AntiId _ _ -> error "inline-c: got antiquotation (buildFunCall)"
        case mbHsName of
          Nothing -> do
            error $ "Cannot capture Haskell variable " ++ show name ++
                    ", because it's not in scope."
          Just hsName -> do
            buildFunCall [| $f $(TH.varE hsName) |] params

    cleanupParam param = case param of
      C.Param mbId ds d loc -> (mbId, C.Type ds d loc)
      _                     -> error "inline-c: got antiquotation (inlineExp)"
-- Type conversion

cFunSigToHsType :: Bool -> C.Type -> [C.Type] -> TH.TypeQ
cFunSigToHsType pure retType params0 = go params0
  where
    go [] = do
      let hsType = cTypeToHsType retType
      if pure then hsType else [t| IO $hsType |]
    go (paramType : params) = do
      [t| $(cTypeToHsType paramType) -> $(go params) |]

cTypeToHsType :: C.Type -> TH.TypeQ
cTypeToHsType [C.cty| void |] = [t| () |]
cTypeToHsType [C.cty| int |] = [t| CInt |]
cTypeToHsType [C.cty| double |] = [t| CDouble |]
cTypeToHsType _ = error "TODO cTypeToHsType"

-- cTypeToHsType (C.Type (C.DeclSpec [] [] tySpec noLoc) (C.DeclRoot _) _) =
--   fromSpec tySpec
--   where
--     fromSpec [C.cty| void |] = [t| () |]
--     fromSpec (C.Tchar Nothing _) = [t| CChar |]
--     fromSpec (C.Tchar (Just (C.Tsigned _)) _) = [t| CChar |]
--     fromSpec (C.Tchar (Just (C.Tunsigned _)) _) = [t| CUChar |]
--     fromSpec _ = error "TODO cTypeToHsType"


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
  :: Parsec.SourcePos -> String -> C.P a -> a
parseC parsecPos str p =
  let pos = Pos
        (Parsec.sourceName parsecPos)
        (Parsec.sourceLine parsecPos)
        (Parsec.sourceColumn parsecPos)
        0
      bytes = Data.ByteString.UTF8.fromString str
      -- TODO support user-defined C types.
      pstate = C.emptyPState [C.Antiquotation] [] bytes pos
  in case C.evalP p pstate of
    Left err ->
      -- TODO consider prefixing with "error while parsing C" or similar
      error $ show err
    Right x ->
      x

-- Note that we split the input this way because we cannot compose Happy
-- parsers easily.
parseTypedC :: C.P a -> Parsec.Parser (C.Type, [C.Param], a)
parseTypedC p = do
  -- Get stuff up to parens, and parse the type
  typePos <- Parsec.getPosition
  typeStr <- takeTillChar '('
  let cType = parseC typePos typeStr C.parseType
  -- Get stuff for params, and parse the type
  paramsPos <- Parsec.getPosition
  paramsStr <- takeTillChar ')'
  let cParams = parseC paramsPos paramsStr C.parseParams
  -- Get the body, and feed it to the given parser
  void $ lex $ Parsec.char '{'
  bodyPos <- Parsec.getPosition
  bodyStr <- takeTillChar '}'
  let cBody = parseC bodyPos bodyStr p
  -- Whew
  return (cType, cParams, cBody)
  where
    lex p' = do
      void p'
      Parsec.spaces

    takeTillChar :: Char -> Parsec.Parser String
    takeTillChar ch = do
      str <- Parsec.many1 $ Parsec.satisfy (/= ch)
      void $ lex $ Parsec.char ch
      return str
