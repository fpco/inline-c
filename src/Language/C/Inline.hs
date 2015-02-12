{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.C.Inline
  ( CCode(..)
  , embedCCode
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.PrettyPrint.Mainland as PrettyPrint
import qualified Language.C as C
import           Control.Exception (catch, throwIO)
import           System.FilePath (addExtension, dropExtension)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Control.Monad (unless, forM_)
import           System.IO.Error (isDoesNotExistError)
import           System.Directory (removeFile)
import           Data.Functor ((<$>))

-- | Data type representing some typed C code.
data CCode = CCode
  { cCallSafety :: TH.Safety
    -- ^ Safety of the foreign call
  , cType :: TH.TypeQ
    -- ^ Type of the foreign call
  , cFunName :: String
    -- ^ Name of the function to call in the code above.
  , cCode :: [C.Definition]
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
embedCCode :: CCode -> TH.ExpQ
embedCCode CCode{..} = do
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
  TH.runIO $ forM_ cCode $ appendCDefinition cFile
  -- Create and add the FFI declaration
  ffiImportName <- TH.newName "inline_c_ffi"
  dec <- TH.forImpD TH.CCall cCallSafety cFunName ffiImportName cType
  TH.addTopDecls [dec]
  [| $(TH.varE ffiImportName) |]

-- | Records what module we are in.
{-# NOINLINE currentModuleRef #-}
currentModuleRef :: IORef (Maybe String)
currentModuleRef = unsafePerformIO $ newIORef Nothing
