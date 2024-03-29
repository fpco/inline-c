{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.C.Inline.FunPtr
  ( mkFunPtr
  , mkFunPtrFromName
  , peekFunPtr
  , uniqueFfiImportName
  ) where

import           Data.Maybe (isJust)
import           Foreign.Ptr (FunPtr)
import           System.Environment (lookupEnv)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

------------------------------------------------------------------------
-- FFI wrappers

-- | @$('mkFunPtr' [t| 'CDouble' -> 'IO' 'CDouble' |] @ generates a foreign import
-- wrapper of type
--
-- @
-- ('CDouble' -> 'IO' 'CDouble') -> 'IO' ('FunPtr' ('CDouble' -> 'IO' 'CDouble'))
-- @
--
-- And invokes it.
mkFunPtr :: TH.TypeQ -> TH.ExpQ
mkFunPtr hsTy = do
  ffiImportName <- uniqueFfiImportName
  -- See note [ghcide-support]
  usingGhcide <- TH.runIO $ isJust <$> lookupEnv "__GHCIDE__"
  if usingGhcide
    then do
      [e|error "inline-c: A 'usingGhcide' mkFunPtr stub was evaluated -- this should not happen" :: $(hsTy) -> IO (FunPtr $(hsTy)) |]
    else do -- Actual foreign function call generation.
      dec <- TH.forImpD TH.CCall TH.Safe "wrapper" ffiImportName [t| $(hsTy) -> IO (FunPtr $(hsTy)) |]
      TH.addTopDecls [dec]
      TH.varE ffiImportName

-- | @$('mkFunPtrFromName' 'foo)@, if @foo :: 'CDouble' -> 'IO'
-- 'CDouble'@, splices in an expression of type @'IO' ('FunPtr'
-- ('CDouble' -> 'IO' 'CDouble'))@.
mkFunPtrFromName :: TH.Name -> TH.ExpQ
mkFunPtrFromName name = do
  i <- TH.reify name
  case i of
#if MIN_VERSION_template_haskell(2,11,0)
    TH.VarI _ ty _ -> [| $(mkFunPtr (return ty)) $(TH.varE name) |]
#else
    TH.VarI _ ty _ _ -> [| $(mkFunPtr (return ty)) $(TH.varE name) |]
#endif
    _ -> fail "mkFunPtrFromName: expecting a variable as argument."

-- | @$('peekFunPtr' [t| 'CDouble' -> 'IO' 'CDouble' |])@ generates a foreign import
-- dynamic of type
--
-- @
-- 'FunPtr' ('CDouble' -> 'IO' 'CDouble') -> ('CDouble' -> 'IO' 'CDouble')
-- @
--
-- And invokes it.
peekFunPtr :: TH.TypeQ -> TH.ExpQ
peekFunPtr hsTy = do
  ffiImportName <- uniqueFfiImportName
  usingGhcide <- TH.runIO $ isJust <$> lookupEnv "__GHCIDE__"
  -- See note [ghcide-support]
  if usingGhcide
    then do
      [e|error "inline-c: A 'usingGhcide' peekFunPtr stub was evaluated -- this should not happen" :: FunPtr $(hsTy) -> $(hsTy) |]
    else do -- Actual foreign function call generation.
      dec <- TH.forImpD TH.CCall TH.Safe "dynamic" ffiImportName [t| FunPtr $(hsTy) -> $(hsTy) |]
      TH.addTopDecls [dec]
      TH.varE ffiImportName

-- TODO absurdly, I need to 'newName' twice for things to work.  I found
-- this hack in language-c-inline.  Why is this?
uniqueFfiImportName :: TH.Q TH.Name
uniqueFfiImportName = TH.newName . show =<< TH.newName "inline_c_ffi"
