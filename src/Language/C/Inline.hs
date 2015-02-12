{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.C.Inline
  ( Code(..)
  , embedCode
  , embedStm
  , embedExp
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.PrettyPrint.Mainland as PrettyPrint
import qualified Language.C as C
import qualified Language.C.Quote.C as C
import           Control.Exception (catch, throwIO)
import           System.FilePath (addExtension, dropExtension)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Control.Monad (unless, forM_)
import           System.IO.Error (isDoesNotExistError)
import           System.Directory (removeFile)
import           Data.Functor ((<$>))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

-- | Data type representing some typed C code.
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

appendCDefinition :: FilePath -> C.Definition -> IO ()
appendCDefinition fp cdef = do
  appendFile fp $ show (PrettyPrint.ppr cdef) ++ "\n"

cSourceLoc :: TH.Q FilePath
cSourceLoc = do
  thisFile <- TH.loc_filename <$> TH.location
  return $ dropExtension thisFile `addExtension` "c"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e = unless (isDoesNotExistError e) $ throwIO e

-- TODO use the #line CPP macro to have the functions in the C file
-- refer to the source location in the Haskell file they come from.
--
-- See <https://gcc.gnu.org/onlinedocs/cpp/Line-Control.html>.
embedCode :: Code -> TH.ExpQ
embedCode Code{..} = do
  cFile <- cSourceLoc
  -- First make sure that 'currentModule' and C file are up to date
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
  -- Write out definitions
  TH.runIO $ forM_ codeDefs $ appendCDefinition cFile
  -- Create and add the FFI declaration.  TODO absurdly, I need to
  -- 'newName' twice for things to work.  I found this hack in
  -- language-c-inline.  Why is this?
  ffiImportName <- TH.newName . show =<< TH.newName "inline_c_ffi"
  dec <- TH.forImpD TH.CCall codeCallSafety codeFunName ffiImportName codeType
  TH.addTopDecls [dec]
  [| $(TH.varE ffiImportName) |]

-- | Records what module we are in.
{-# NOINLINE currentModuleRef #-}
currentModuleRef :: IORef (Maybe String)
currentModuleRef = unsafePerformIO $ newIORef Nothing

uniqueCName :: IO String
uniqueCName = do
  -- UUID with the dashes removed
  unique <- filter (/= '-') . UUID.toString <$> UUID.nextRandom
  return $ "inline_c_" ++ unique

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
