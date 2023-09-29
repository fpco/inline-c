{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module exposing a 'Context' to inline CUDA code.  We only have used
-- this for experiments, so use with caution.  See the CUDA tests to see
-- how to build inline CUDA code.
module Language.C.Inline.Cuda
  ( module Language.C.Inline
  , cudaCtx
  , Cpp.cppTypePairs
  , Cpp.using
  , Cpp.AbstractCppExceptionPtr
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types as CT
import qualified Language.C.Inline.Cpp as Cpp

import qualified Data.Map as Map
import           Control.Monad.IO.Class (liftIO)
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)

compileCuda :: String -> TH.Q FilePath
compileCuda src = do
  cuFile <- TH.addTempFile "cu"
  oFile <- TH.addTempFile "o"
  let (cmd,args) = ("nvcc", ["-c","-o",oFile, cuFile])
  (code, stdout, stderr) <- liftIO $ do
    writeFile cuFile src
    readProcessWithExitCode cmd args ""
  case code of
    ExitFailure _ -> fail $ "Compile Command: " ++ (foldl (\a b -> a ++ " " ++ b) " " (cmd : args)) ++ "\n" ++ "    Output: " ++ stdout ++ "\n" ++ "    Error: " ++ stderr
    ExitSuccess -> return oFile

-- | The equivalent of 'C.baseCtx' for CUDA.  It specifies the @.cu@
-- file extension for the CUDA file, so that nvcc will decide to build CUDA
-- instead of C.  See the @.cabal@ test target for an example on how to
-- build.
cudaCtx :: Context
cudaCtx = Cpp.cppCtx <> mempty
  { ctxForeignSrcLang = Just TH.RawObject
  , ctxOutput = Just $ \s -> "extern \"C\" {\n" ++ s ++ "\n}"
  , ctxEnableCpp = True
  , ctxRawObjectCompile = Just compileCuda
  , ctxTypesTable = Map.singleton (CT.TypeName "std::exception_ptr") [t|Cpp.AbstractCppExceptionPtr|]
  }

