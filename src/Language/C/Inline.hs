{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.C.Inline
  ( CFunction(..)
  , buildCFunction
  , embedCFunction
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
import           Control.Monad (unless)
import           System.IO.Error (isDoesNotExistError)
import           System.Directory (removeFile)
import           Data.Functor ((<$>))

-- | Data type representing a C function with the information we need
-- (its type) to call it from Haskell.
data CFunction = CFunction
  { cFunSafety :: TH.Safety
    -- ^ Safety of the foreign call
  , cFunHSType :: TH.TypeQ
    -- ^ Type of the foreign call
  , cFunCType :: (C.Type, [C.Param])
    -- ^ Type of the C function (return type and parameters)
  , cFunBody :: [C.Stm]
    -- ^ Body of the function
  }

-- |
-- @
-- buildCFunction "add" $ CFunction
--   TH.Unsafe
--   [t| Int -> Int -> Int |]
--   ([cty| int |], [cparams| int x, int y |])
--   [cstms| { int z = x + y; return z; } |]
-- @
--
-- We accept @['C.Stm']@ insteaad of @['C.BlockItem']@ because there
-- does not seem to be a way to easily parse lists of items.
buildCFunction
  :: String
  -- ^ Function name
  -> CFunction
  -> C.Definition
buildCFunction funName CFunction{..} =
  [C.cedecl| $ty:(fst cFunCType) $id:funName($params:(snd cFunCType)) { $stms:cFunBody } |]

-- | Generates a fresh name for C functions
freshName :: TH.Q TH.Name
freshName = TH.newName "cfun"
-- TODO check that this is safe.  E.g. that we don't duplicate names
-- across modules.

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
embedCFunction :: CFunction -> TH.ExpQ
embedCFunction cfun@CFunction{..} = do
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
  -- Generate new name for C function
  funName <- freshName
  let cFunName = show funName
  -- Write out the C code to the right files
  let funDef = buildCFunction cFunName cfun
  TH.runIO $ appendCDefinition cFile funDef
  -- Create and add the FFI declaration
  dec <- TH.forImpD TH.CCall cFunSafety cFunName funName cFunHSType
  TH.addTopDecls [dec]
  [| $(TH.varE funName) |]

-- | Records what module we are in.
{-# NOINLINE currentModuleRef #-}
currentModuleRef :: IORef (Maybe String)
currentModuleRef = unsafePerformIO $ newIORef Nothing
