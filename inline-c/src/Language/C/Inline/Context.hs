{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | A 'Context' is used to define the capabilities of the
-- TemplateHaskell code that handles the inline C code.  See the
-- documentation of the data type for more details.
--
-- In practice, a 'Context' will have to be defined for each library
-- that defines new C types, to allow the TemplateHaskell code to
-- interpret said types correctly.
module Language.C.Inline.Context
  ( Context(..)
  , CArray
  , convertCType
  , isTypeName
  , baseCtx
  , funPtrCtx
  ) where

import           Control.Applicative (empty, (<|>))
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import           Data.Monoid (Monoid(..))
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)
import qualified Language.Haskell.TH as TH
import           Data.Functor ((<$>))
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import           Data.Monoid ((<>))

import           Language.C.Inline.FunPtr
import qualified Language.C.Types as C

-- | A 'Context' stores information needed to convert C types to Haskell
-- types, and to convert Haskell values to C values.
--
-- 'Context's can be composed with their 'Monoid' instance, where
-- 'mappend' is right-biased -- in @'mappend' x y@ @y@ will take
-- precedence over @x@.
data Context = Context
  { ctxConvertCTypeSpec :: Map.Map C.TypeSpecifier TH.TypeQ
    -- ^ Maps 'TypeSpecifier's to Haskell types.  This is needed to
    -- convert C types to Haskell types, and to parse C types
    -- correctly.
    --
    -- Note that 'C.TypeSpecifier's that are not in the 'Context' map
    -- might not be parsed correctly.
  , ctxMarshaller :: TH.Type -> TH.Exp -> TH.Q (Maybe TH.Exp)
    -- ^ The marshaller takes the expected type of a captured variable
    -- and the 'TH.Exp' representing the captured variable itself and
    -- processes it into something else.
    --
    -- In 'baseCtx', the marshaller simply returns the expression
    -- unchanged, but for example the 'funPtrCtx' processes 'FunPtr'
    -- arguments so that Haskell functions are automatically converted
    -- to 'FunPtr's.
  }

instance Monoid Context where
  mempty = Context
    { ctxConvertCTypeSpec = Map.empty
    , ctxMarshaller = \_ _ -> runMaybeT empty
    }

  mappend ctx2 ctx1 = Context
    { ctxConvertCTypeSpec =
        ctxConvertCTypeSpec ctx1 <> ctxConvertCTypeSpec ctx2
    , ctxMarshaller = \hsTy hsArg -> runMaybeT $
        MaybeT (ctxMarshaller ctx1 hsTy hsArg) <|> MaybeT (ctxMarshaller ctx2 hsTy hsArg)
    }

-- | Context useful to work with vanilla C.  Used by default.
--
-- 'ctxConvertCTypeSpec': converts C basic types to their counterparts
-- in "Foreign.C.Types" (TODO currently slightly incomplete).
--
-- 'ctxMarshaller': Just returns the expression.
-- @
baseCtx :: Context
baseCtx = Context
  { ctxConvertCTypeSpec = baseConvertCTypeSpec
  , ctxMarshaller = \_ hsExp -> return $ Just hsExp
  }

baseConvertCTypeSpec :: Map.Map C.TypeSpecifier TH.TypeQ
baseConvertCTypeSpec = Map.fromList
  [ (C.Void, [t| () |])
  , (C.Char Nothing, [t| CChar |])
  , (C.Char (Just C.Signed), [t| CChar |])
  , (C.Char (Just C.Unsigned), [t| CUChar |])
  , (C.Short C.Signed, [t| CShort |])
  , (C.Short C.Unsigned, [t| CUShort |])
  , (C.Int C.Signed, [t| CInt |])
  , (C.Int C.Unsigned, [t| CUInt |])
  , (C.Long C.Signed, [t| CLong |])
  , (C.Long C.Unsigned, [t| CULong |])
  , (C.LLong C.Signed, [t| CLLong |])
  , (C.LLong C.Unsigned, [t| CULLong |])
  , (C.Float, [t| CFloat |])
  , (C.Double, [t| CDouble |])
  ]

-- | An alias for 'Ptr'.
type CArray = Ptr

------------------------------------------------------------------------
-- Type conversion

-- | Given a 'Context', it uses its 'ctxConvertCTypeSpec' to convert
-- arbitrary C types.
convertCType
  :: Context
  -> Bool
  -- ^ Whether function pointers should be pure or not.
  -> C.Type
  -> TH.Q (Maybe TH.Type)
convertCType ctx pure = runMaybeT . go
  where
    goDecl = go . C.parameterDeclarationType

    go :: C.Type -> MaybeT TH.Q TH.Type
    go cTy = case cTy of
      C.TypeSpecifier _specs cSpec ->
        case Map.lookup cSpec (ctxConvertCTypeSpec ctx) of
          Nothing -> mzero
          Just ty -> lift ty
      C.Ptr _quals (C.Proto retType pars) -> do
        hsRetType <- go retType
        hsPars <- mapM goDecl pars
        lift [t| FunPtr $(buildArr hsPars hsRetType) |]
      C.Ptr _quals cTy' -> do
        hsTy <- go cTy'
        lift [t| Ptr $(return hsTy) |]
      C.Array _mbSize cTy' -> do
        hsTy <- go cTy'
        lift [t| CArray $(return hsTy) |]
      C.Proto _retType _pars -> do
        -- We cannot convert standalone prototypes
        mzero

    buildArr [] hsRetType =
      if pure then [t| $(return hsRetType) |] else [t| IO $(return hsRetType) |]
    buildArr (hsPar : hsPars) hsRetType =
      [t| $(return hsPar) -> $(buildArr hsPars hsRetType) |]

isTypeName :: Context -> C.Id -> Bool
isTypeName ctx id' = Map.member (C.TypeName id') $ ctxConvertCTypeSpec ctx

------------------------------------------------------------------------
-- Function pointer removal

-- | This 'Context' includes a 'ctxMarshaller' that removes the need for
-- explicitely creating 'FunPtr's.
--
-- For example, if we capture a variable of C type @int (*f)(int, int)@
-- from the C code, the derived Haskell type will be @'FunPtr' ('Int' ->
-- 'Int' -> 'IO' 'Int')@.
--
-- Using the 'funPtrCtx' captured things of type @'Int' -> 'Int' -> 'IO'
-- 'Int'@ will automatically be converted to a 'FunPtr'.
funPtrCtx :: Context
funPtrCtx = mempty{ctxMarshaller = convertFunPtrs}
  where
    convertFunPtrs :: TH.Type -> TH.Exp -> TH.Q (Maybe TH.Exp)
    convertFunPtrs hsTy hsExp = case hsTy of
      -- TODO I think this 'unsafePerformIO' is safe, given that the
      -- arguments to the FFI call are passed immediately and do not
      -- leak anywhere else.  But check.
      TH.AppT (TH.ConT n) hsTy' | n == ''FunPtr ->
        Just <$> [| unsafePerformIO ($(mkFunPtr (return hsTy')) $(return hsExp)) |]
      _ -> return Nothing
