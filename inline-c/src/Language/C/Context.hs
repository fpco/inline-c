{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | A 'Context' is used to define the capabilities of the
-- TemplateHaskell code that handles the inline C code.  See the
-- documentation of the data type for more details.
--
-- In practice, a 'Context' will have to be defined for each library
-- that defines new C types, to allow the TemplateHaskell code to
-- interpret said types correctly.
module Language.C.Context
  ( Context(..)
  , CArray
  , convertCType
  , baseCtx
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.C as C
import qualified Language.C.Quote.C as C
import           Foreign.C.Types
import           Foreign.Ptr
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad (mzero, msum, guard)
import           Data.Monoid (Monoid(..))
import           Control.Applicative (empty, (<|>))
import           Data.List (isSuffixOf)

-- | A 'Context' stores information needed to:
--
-- * Parse inline C code;
-- * Convert C types to Haskell types;
-- * Interpret suffix types.
--
-- 'Context's can be composed with their 'Monoid' instance, where
-- 'mappend' is left-biased -- the lhs of 'mappend' will take precedence
-- in 'ctxConvertCTypeSpec' and 'ctxGetSuffixType'.
data Context = Context
  { ctxCTypes :: [String]
    -- ^ Additional named types for the C parser.  Currently, every type
    -- beyond standard C needs to be declared explicitely for the parser
    -- to work.
    --
    -- For example, if some library defines the type @Complex@, the
    -- string @"Complex"@ will have to be included.
    --
    -- TODO consider modifying @language-c-quote@ to lift this restriction.
  , ctxConvertCTypeSpec :: C.TypeSpec -> TH.Q (Maybe TH.Type)
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
  , ctxGetSuffixType :: String -> Maybe (String, C.Type)
    -- ^ Given an identifier, checks if it is suffix typed.  For
    -- example, for some context @ctx@, we might have that
    --
    -- @
    -- 'ctxGetSuffixType' ctx "n_int" ==> 'Just' ("n", [cty| int |])
    -- @
  }

instance Monoid Context where
  mempty = Context
    { ctxCTypes = mempty
    , ctxConvertCTypeSpec = \_ -> runMaybeT empty
    , ctxGetSuffixType = \_ -> empty
    }

  mappend ctx1 ctx2 = Context
    { ctxCTypes = ctxCTypes ctx1 ++ ctxCTypes ctx2
    , ctxConvertCTypeSpec = \cty -> runMaybeT $
        MaybeT (ctxConvertCTypeSpec ctx1 cty) <|> MaybeT (ctxConvertCTypeSpec ctx2 cty)
    , ctxGetSuffixType = \suff ->
        ctxGetSuffixType ctx1 suff <|> ctxGetSuffixType ctx2 suff
    }

-- | Context useful to work with vanilla C.  Used by default.
--
-- 'ctxCTypes': None.
--
-- 'ctxConvertCTypeSpec': converts C basic types to their counterparts
-- in "Foreign.C.Types" (TODO currently slightly incomplete).
--
-- 'ctxGetSuffixType':
--
-- @
-- "int"        ==> int
-- "uint"       ==> unsigned int
-- "long"       ==> long
-- "ulong"      ==> unsigned long
-- "char"       ==> char
-- "uchar"      ==> unsigned char
-- "float"      ==> float
-- "double"     ==> double
--
-- "int_ptr"    ==> int*
-- "uint_ptr"   ==> unsigned int*
-- "long_ptr"   ==> long*
-- "ulong_ptr"  ==> unsigned long*
-- "char_ptr"   ==> char*
-- "uchar_ptr"  ==> unsigned char*
-- "float_ptr"  ==> float*
-- "double_ptr" ==> double*
-- @
baseCtx :: Context
baseCtx = Context
  { ctxCTypes = []
  , ctxConvertCTypeSpec = baseConvertCTypeSpec
  , ctxGetSuffixType = baseGetSuffixType
  }

baseConvertCTypeSpec :: C.TypeSpec -> TH.Q (Maybe TH.Type)
baseConvertCTypeSpec cspec = runMaybeT $ case cspec of
  C.Tvoid{} -> lift [t| () |]
  C.Tchar (Just C.Tunsigned{}) _ -> lift [t| CUChar |]
  C.Tchar _ _ -> lift [t| CChar |]
  C.Tshort (Just C.Tunsigned{}) _ -> lift [t| CUShort |]
  C.Tshort _ _ -> lift [t| CShort |]
  C.Tint (Just C.Tunsigned{}) _ -> lift [t| CUInt |]
  C.Tint _ _ -> lift [t| CInt |]
  C.Tlong (Just C.Tunsigned{}) _ -> lift [t| CULong |]
  C.Tlong _ _ -> lift [t| CLong |]
  C.Tlong_long (Just C.Tunsigned{}) _ -> lift [t| CULLong |]
  C.Tlong_long _ _ -> lift [t| CLLong |]
  C.Tfloat{} -> lift [t| CFloat |]
  C.Tdouble{} -> lift [t| CDouble |]
  _ -> mzero

-- | An alias for 'Ptr'.
type CArray = Ptr

-- | Given a 'Context', it uses its 'ctxConvertCTypeSpec' to convert
-- arbitrary C types.
convertCType
  :: Context
  -> Bool
  -- ^ Whether function pointers should be pure or not.
  -> C.Type
  -> TH.Q (Maybe TH.Type)
convertCType ctx pure cTy = runMaybeT $ go $ simplifyCType cTy
  where
    go :: SimpleCType -> MaybeT TH.Q TH.Type
    go sTy = case sTy of
      SCTSpec cSpec -> MaybeT $ ctxConvertCTypeSpec ctx cSpec
      SCTPtr sTy' -> do
        hsTy <- go sTy'
        lift [t| Ptr $(return hsTy) |]
      SCTArray sTy' -> do
        hsTy <- go sTy'
        lift [t| CArray $(return hsTy) |]
      SCTFunPtr retType pars -> do
        hsRetType <- go retType
        hsPars <- mapM go pars
        lift [t| FunPtr $(buildArr hsPars hsRetType) |]

    buildArr [] hsRetType =
      if pure then [t| $(return hsRetType) |] else [t| IO $(return hsRetType) |]
    buildArr (hsPar : hsPars) hsRetType =
      [t| $(return hsPar) -> $(buildArr hsPars hsRetType) |]


baseGetSuffixType :: String -> Maybe (String, C.Type)
baseGetSuffixType s = msum
  [ do guard (('_' : suff) `isSuffixOf` s)
       return (take (length s - length suff - 1) s, ctype)
  | (suff, ctype) <- table
  ]
  where
    table =
      [ ("int", [C.cty| int |])
      , ("uint", [C.cty| unsigned int |])
      , ("long", [C.cty| long |])
      , ("ulong", [C.cty| unsigned long |])
      , ("char", [C.cty| char |])
      , ("uchar", [C.cty| unsigned char |])
      , ("float", [C.cty| float |])
      , ("double", [C.cty| double |])

      , ("int_ptr", [C.cty| int* |])
      , ("uint_ptr", [C.cty| unsigned int* |])
      , ("long_ptr", [C.cty| long* |])
      , ("ulong_ptr", [C.cty| unsigned long* |])
      , ("char_ptr", [C.cty| char* |])
      , ("uchar_ptr", [C.cty| unsigned char* |])
      , ("float_ptr", [C.cty| float* |])
      , ("double_ptr", [C.cty| double* |])
      ]

------------------------------------------------------------------------
-- Simple C types conversion

data SimpleCType
  = SCTSpec C.TypeSpec
  | SCTPtr SimpleCType
  | SCTArray SimpleCType
  | SCTFunPtr SimpleCType [SimpleCType]
  deriving (Show, Eq)

simplifyCType :: C.Type -> SimpleCType
simplifyCType cTy0 = case cTy0 of
  C.Type (C.DeclSpec _storage _quals cTySpec _) decl _ ->
    go cTySpec decl
  _ ->
    error "inline-c: got antiquotation (simplifyCType)"
  where
    go :: C.TypeSpec -> C.Decl -> SimpleCType
    go cTySpec decl0 = case decl0 of
      C.DeclRoot _ ->
        SCTSpec cTySpec
      C.Ptr _quals (C.Proto decl (C.Params params _ _) _) _ ->
        let processParam param = case param of
              C.Param _ (C.DeclSpec _ _ spec _) decl' _ -> go spec decl'
              _ -> error "inline-c: got antiquotation (simplifyCType)"
        in SCTFunPtr (go cTySpec decl) $ map processParam params
      C.Ptr _quals (C.OldProto decl [] _) _ ->
        SCTFunPtr (go cTySpec decl) []
      C.Ptr _quals C.OldProto{} _ ->
        error "inline-c: Old prototypes not supported (simplifyCType)"
      C.Ptr _quals decl _ ->
        SCTPtr $ go cTySpec decl
      C.Array _ C.ArraySize{} _ _ ->
        error "inline-c: Sized array not supported (simplifyCType)"
      C.Array _ C.VariableArraySize{} _ _ ->
        error "inline-c: Variably sized array not supported (simplifyCType)"
      C.Array _quals C.NoArraySize{} decl _ ->
        SCTArray $ go cTySpec decl
      C.Proto{} ->
        error "inline-c: Non-pointer function prototype (simplifyCType)"
      C.OldProto{} ->
        error "inline-c: Non-pointer function prototype (simplifyCType)"
      C.BlockPtr{} ->
        error "inline-c: Blocks not supported (simplifyCType)"
      C.AntiTypeDecl{} ->
        error "inline-c: got antiquotation (simplifyCType)"

