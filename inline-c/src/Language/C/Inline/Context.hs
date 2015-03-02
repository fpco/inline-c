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
  , baseCtx
  , funPtrCtx
  ) where

import           Control.Applicative (empty, (<|>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import           Data.Monoid (Monoid(..))
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)
import qualified Language.Haskell.TH as TH
import           Data.Functor ((<$>))
import           System.IO.Unsafe (unsafePerformIO)

import           Language.C.Inline.FunPtr
import qualified Language.C.Types as C

-- | A 'Context' stores information needed to convert C types to Haskell
-- types, and to convert Haskell values to C values.
--
-- 'Context's can be composed with their 'Monoid' instance, where
-- 'mappend' is right-biased -- in @'mappend' x y@ @y@ will take
-- precedence over @x@.
data Context = Context
  { ctxConvertCTypeSpec :: C.TypeSpec -> TH.Q (Maybe TH.Type)
    -- ^ Tries to convert a C type spec to an Haskell type. For example,
    -- for some context @ctx@, we might have that
    --
    -- @
    -- 'ctxConvertCTypeSpec' ctx [cty| int |] ==> 'Just' 'CInt'
    -- @
    --
    -- Note that here only the C basic types (in @language-c-quote@
    -- parlance the 'C.TypeSpec') need to be converted.  Conversion from
    -- derived types, such as pointers or arrays, is performed
    -- automatically by 'convertCType'.
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
    { ctxConvertCTypeSpec = \_ -> runMaybeT empty
    , ctxMarshaller = \_ _ -> runMaybeT empty
    }

  mappend ctx2 ctx1 = Context
    { ctxConvertCTypeSpec = \cty -> runMaybeT $
        MaybeT (ctxConvertCTypeSpec ctx1 cty) <|> MaybeT (ctxConvertCTypeSpec ctx2 cty)
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

baseConvertCTypeSpec :: C.TypeSpec -> TH.Q (Maybe TH.Type)
baseConvertCTypeSpec cspec = case cspec of
  C.Void -> Just <$> [t| () |]
  C.Char Nothing -> Just <$> [t| CChar |]
  C.Char (Just C.Signed) -> Just <$> [t| CChar |]
  C.Char (Just C.Unsigned) -> Just <$> [t| CUChar |]
  C.Short C.Signed -> Just <$> [t| CShort |]
  C.Short C.Unsigned -> Just <$> [t| CUShort |]
  C.Int C.Signed -> Just <$> [t| CInt |]
  C.Int C.Unsigned -> Just <$> [t| CUInt |]
  C.Long C.Signed -> Just <$> [t| CLong |]
  C.Long C.Unsigned -> Just <$> [t| CULong |]
  C.LLong C.Signed -> Just <$> [t| CLLong |]
  C.LLong C.Unsigned -> Just <$> [t| CULLong |]
  C.Float -> Just <$> [t| CFloat |]
  C.Double -> Just <$> [t| CDouble |]
  _ -> return Nothing

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
    goDecl (C.Declaration _ _quals cTy) = go cTy

    go :: C.Type -> MaybeT TH.Q TH.Type
    go cTy = case cTy of
      C.TypeSpec cSpec -> MaybeT $ ctxConvertCTypeSpec ctx cSpec
      C.Ptr _quals cTy' -> do
        hsTy <- go cTy'
        lift [t| Ptr $(return hsTy) |]
      C.Array _mbSize cTy' -> do
        hsTy <- go cTy'
        lift [t| CArray $(return hsTy) |]
      C.Proto retType pars -> do
        hsRetType <- go retType
        hsPars <- mapM goDecl pars
        lift [t| FunPtr $(buildArr hsPars hsRetType) |]

    buildArr [] hsRetType =
      if pure then [t| $(return hsRetType) |] else [t| IO $(return hsRetType) |]
    buildArr (hsPar : hsPars) hsRetType =
      [t| $(return hsPar) -> $(buildArr hsPars hsRetType) |]

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
