{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A 'Context' is used to define the capabilities of the Template Haskell code
-- that handles the inline C code. See the documentation of the data type for
-- more details.
--
-- In practice, a 'Context' will have to be defined for each library that
-- defines new C types, to allow the TemplateHaskell code to interpret said
-- types correctly.

module Language.C.Inline.Context
  ( -- * 'TypesTable'
    TypesTable
  , Purity(..)
  , convertType
  , CArray
  , typeNamesFromTypesTable

    -- * 'AntiQuoter'
  , AntiQuoter(..)
  , AntiQuoterId
  , SomeAntiQuoter(..)
  , AntiQuoters

    -- * 'Context'
  , Context(..)
  , baseCtx
  , funCtx
  , vecCtx
  , VecCtx(..)
  , bsCtx
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Word (Word8, Word16, Word32, Word64)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
import           Foreign.Storable (Storable)
import qualified Language.Haskell.TH as TH
import qualified Text.Parser.Token as Parser
import qualified Data.HashSet as HashSet

#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
import           Data.Traversable (traverse)
#endif

import           Language.C.Inline.FunPtr
import qualified Language.C.Types as C
import           Language.C.Inline.HaskellIdentifier

-- | A mapping from 'C.TypeSpecifier's to Haskell types.  Needed both to
-- parse C types, and to convert them to Haskell types.
type TypesTable = Map.Map C.TypeSpecifier TH.TypeQ

-- | A data type to indicate whether the user requested pure or IO
-- function from Haskell
data Purity
  = Pure
  | IO
  deriving (Eq, Show)

-- | Specifies how to parse and process an antiquotation in the C code.
--
-- All antiquotations (apart from plain variable capture) have syntax
--
-- @
-- $XXX:YYY
-- @
--
-- Where @XXX@ is the name of the antiquoter and @YYY@ is something
-- parseable by the respective 'aqParser'.
data AntiQuoter a = AntiQuoter
  { aqParser :: forall m. C.CParser HaskellIdentifier m => m (C.CIdentifier, C.Type C.CIdentifier, a)
    -- ^ Parses the body of the antiquotation, returning a hint for the name to
    -- assign to the variable that will replace the anti-quotation, the type of
    -- said variable, and some arbitrary data which will then be fed to
    -- 'aqMarshaller'.
    --
    -- The 'C.Type' has 'Void' as an identifier type to make sure that
    -- no names appear in it.
  , aqMarshaller :: Purity -> TypesTable -> C.Type C.CIdentifier -> a -> TH.Q (TH.Type, TH.Exp)
    -- ^ Takes the requested purity, the current 'TypesTable', and the
    -- type and the body returned by 'aqParser'.
    --
    -- Returns the Haskell type for the parameter, and the Haskell expression
    -- that will be passed in as the parameter.
    --
    -- If the the type returned is @ty@, the 'TH.Exp' __must__ have type @forall
    -- a. (ty -> IO a) -> IO a@. This allows to do resource handling when
    -- preparing C values.
    --
    -- Care must be taken regarding 'Purity'. Specifically, the generated IO
    -- computation must be idempotent to guarantee its safety when used in pure
    -- code. We cannot prevent the IO computation from being inlined, hence
    -- potentially duplicated. If non-idempotent marshallers are required (e.g.
    -- if an update to some global state is needed), it is best to throw an
    -- error when 'Purity' is 'Pure' (for example "you cannot use context X with
    -- @pure@"), which will show up at compile time.
  }

-- | An identifier for a 'AntiQuoter'.
type AntiQuoterId = String

-- | Existential wrapper around 'AntiQuoter'.
data SomeAntiQuoter = forall a. (Eq a, Typeable a) => SomeAntiQuoter (AntiQuoter a)

type AntiQuoters = Map.Map AntiQuoterId SomeAntiQuoter

-- | A 'Context' stores various information needed to produce the files with
-- the C code derived from the inline C snippets.
--
-- 'Context's can be composed with their 'Monoid' instance, where 'mappend' is
-- right-biased -- in @'mappend' x y@ @y@ will take precedence over @x@.
data Context = Context
  { ctxTypesTable :: TypesTable
    -- ^ Needed to convert C types to Haskell types.
  , ctxAntiQuoters :: AntiQuoters
    -- ^ Needed to parse and process antiquotations.
  , ctxFileExtension :: Maybe String
    -- ^ Will determine the extension of the file containing the inline
    -- C snippets.
  , ctxOutput :: Maybe (String -> String)
    -- ^ This function is used to post-process the functions generated
    -- from the C snippets.  Currently just used to specify C linkage
    -- when generating C++ code.
  }

instance Monoid Context where
  mempty = Context
    { ctxTypesTable = mempty
    , ctxAntiQuoters = mempty
    , ctxFileExtension = Nothing
    , ctxOutput = Nothing
    }

  mappend ctx2 ctx1 = Context
    { ctxTypesTable = ctxTypesTable ctx1 <> ctxTypesTable ctx2
    , ctxAntiQuoters = ctxAntiQuoters ctx1 <> ctxAntiQuoters ctx2
    , ctxFileExtension = ctxFileExtension ctx1 <|> ctxFileExtension ctx2
    , ctxOutput = ctxOutput ctx1 <|> ctxOutput ctx2
    }

-- | Context useful to work with vanilla C. Used by default.
--
-- 'ctxTypesTable': converts C basic types to their counterparts in
-- "Foreign.C.Types".
--
-- No 'ctxAntiQuoters'.
baseCtx :: Context
baseCtx = mempty
  { ctxTypesTable = baseTypesTable
  }

baseTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
baseTypesTable = Map.fromList
  [ (C.Void, [t| () |])
  -- Types from Foreign.C.Types in the order in which they are presented there,
  -- along with its documentation's section headers.
  --
  -- Integral types
  , (C.Char Nothing, [t| CChar |])
  , (C.Char (Just C.Signed), [t| CSChar |])
  , (C.Char (Just C.Unsigned), [t| CUChar |])
  , (C.Short C.Signed, [t| CShort |])
  , (C.Short C.Unsigned, [t| CUShort |])
  , (C.Int C.Signed, [t| CInt |])
  , (C.Int C.Unsigned, [t| CUInt |])
  , (C.Long C.Signed, [t| CLong |])
  , (C.Long C.Unsigned, [t| CULong |])
  , (C.TypeName "ptrdiff_t", [t| CPtrdiff |])
  , (C.TypeName "size_t", [t| CSize |])
  , (C.TypeName "wchar_t", [t| CWchar |])
  , (C.TypeName "sig_atomic_t", [t| CSigAtomic |])
  , (C.LLong C.Signed, [t| CLLong |])
  , (C.LLong C.Unsigned, [t| CULLong |])
  , (C.TypeName "intptr_t", [t| CIntPtr |])
  , (C.TypeName "uintptr_t", [t| CUIntPtr |])
  , (C.TypeName "intmax_t", [t| CIntMax |])
  , (C.TypeName "uintmax_t", [t| CUIntMax |])
  -- Numeric types
  , (C.TypeName "clock_t", [t| CClock |])
  , (C.TypeName "time_t", [t| CTime |])
  , (C.TypeName "useconds_t", [t| CUSeconds |])
  , (C.TypeName "suseconds_t", [t| CSUSeconds |])
  -- Floating types
  , (C.Float, [t| CFloat |])
  , (C.Double, [t| CDouble |])
  -- Other types
  , (C.TypeName "FILE", [t| CFile |])
  , (C.TypeName "fpos_t", [t| CFpos |])
  , (C.TypeName "jmp_buf", [t| CJmpBuf |])
  -- Types from stdint.h that can be statically mapped to their Haskell
  -- equivalents. Excludes int_fast*_t and int_least*_t and the corresponding
  -- unsigned types, since their sizes are platform-specific.
  , (C.TypeName "int8_t", [t| Int8 |])
  , (C.TypeName "int16_t", [t| Int16 |])
  , (C.TypeName "int32_t", [t| Int32 |])
  , (C.TypeName "int64_t", [t| Int64 |])
  , (C.TypeName "uint8_t", [t| Word8 |])
  , (C.TypeName "uint16_t", [t| Word16 |])
  , (C.TypeName "uint32_t", [t| Word32 |])
  , (C.TypeName "uint64_t", [t| Word64 |])
  ]

-- | An alias for 'Ptr'.
type CArray = Ptr

------------------------------------------------------------------------
-- Type conversion

-- | Given a 'Context', it uses its 'ctxTypesTable' to convert
-- arbitrary C types.
convertType
  :: Purity
  -> TypesTable
  -> C.Type C.CIdentifier
  -> TH.Q (Maybe TH.Type)
convertType purity cTypes = runMaybeT . go
  where
    goDecl = go . C.parameterDeclarationType

    go :: C.Type C.CIdentifier -> MaybeT TH.Q TH.Type
    go cTy = case cTy of
      C.TypeSpecifier _specs cSpec ->
        case Map.lookup cSpec cTypes of
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
      case purity of
        Pure -> [t| $(return hsRetType) |]
        IO -> [t| IO $(return hsRetType) |]
    buildArr (hsPar : hsPars) hsRetType =
      [t| $(return hsPar) -> $(buildArr hsPars hsRetType) |]

typeNamesFromTypesTable :: TypesTable -> C.TypeNames
typeNamesFromTypesTable cTypes = HashSet.fromList
  [ id' | C.TypeName id' <- Map.keys cTypes ]

------------------------------------------------------------------------
-- Useful contexts

getHsVariable :: String -> HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

convertType_ :: String -> Purity -> TypesTable -> C.Type C.CIdentifier -> TH.Q TH.Type
convertType_ err purity cTypes cTy = do
  mbHsType <- convertType purity cTypes cTy
  case mbHsType of
    Nothing -> fail $ "Cannot convert C type (" ++ err ++ ")"
    Just hsType -> return hsType

-- | This 'Context' includes a 'AntiQuoter' that removes the need for
-- explicitely creating 'FunPtr's, named @"fun"@.
--
-- For example, we can capture function @f@ of type @CInt -> CInt -> IO
-- CInt@ in C code using @$fun:(int (*f)(int, int))@.
--
-- When used in a @pure@ embedding, the Haskell function will have to be
-- pure too.  Continuing the example above we'll have @CInt -> CInt ->
-- IO CInt@.
--
-- Does not include the 'baseCtx', since most of the time it's going to
-- be included as part of larger contexts.
funCtx :: Context
funCtx = mempty
  { ctxAntiQuoters = Map.fromList [("fun", SomeAntiQuoter funPtrAntiQuoter)]
  }

funPtrAntiQuoter :: AntiQuoter HaskellIdentifier
funPtrAntiQuoter = AntiQuoter
  { aqParser = cDeclAqParser
  , aqMarshaller = \purity cTypes cTy cId -> do
      hsTy <- convertType_ "funCtx" purity cTypes cTy
      hsExp <- getHsVariable "funCtx" cId
      case hsTy of
        TH.AppT (TH.ConT n) hsTy' | n == ''FunPtr -> do
          hsExp' <- [| \cont -> do
              funPtr <- $(mkFunPtr (return hsTy')) $(return hsExp)
              x <- cont funPtr
              freeHaskellFunPtr funPtr
              return x
            |]
          return (hsTy, hsExp')
        _ -> fail "The `fun' marshaller captures function pointers only"
  }

-- | This 'Context' includes two 'AntiQuoter's that allow to easily use
-- Haskell vectors in C.
--
-- Specifically, the @vec-len@ and @vec-ptr@ will get the length and the
-- pointer underlying mutable ('V.IOVector') and immutable ('V.Vector')
-- storable vectors.
--
-- Note that if you use 'vecCtx' to manipulate immutable vectors you
-- must make sure that the vector is not modified in the C code.
--
-- To use @vec-len@, simply write @$vec-len:x@, where @x@ is something
-- of type @'V.IOVector' a@ or @'V.Vector' a@, for some @a@.  To use
-- @vec-ptr@ you need to specify the type of the pointer,
-- e.g. @$vec-len:(int *x)@ will work if @x@ has type @'V.IOVector'
-- 'CInt'@.
vecCtx :: Context
vecCtx = mempty
  { ctxAntiQuoters = Map.fromList
      [ ("vec-ptr", SomeAntiQuoter vecPtrAntiQuoter)
      , ("vec-len", SomeAntiQuoter vecLenAntiQuoter)
      ]
  }

-- | Type class used to implement the anti-quoters in 'vecCtx'.
class VecCtx a where
  type VecCtxScalar a :: *

  vecCtxLength :: a -> Int
  vecCtxUnsafeWith :: a -> (Ptr (VecCtxScalar a) -> IO b) -> IO b

instance Storable a => VecCtx (V.Vector a) where
  type VecCtxScalar (V.Vector a) = a

  vecCtxLength = V.length
  vecCtxUnsafeWith = V.unsafeWith

instance Storable a => VecCtx (VM.IOVector a) where
  type VecCtxScalar (VM.IOVector a) = a

  vecCtxLength = VM.length
  vecCtxUnsafeWith = VM.unsafeWith

vecPtrAntiQuoter :: AntiQuoter HaskellIdentifier
vecPtrAntiQuoter = AntiQuoter
  { aqParser = cDeclAqParser
  , aqMarshaller = \purity cTypes cTy cId -> do
      hsTy <- convertType_ "vecCtx" purity cTypes cTy
      hsExp <- getHsVariable "vecCtx" cId
      hsExp' <- [| vecCtxUnsafeWith $(return hsExp) |]
      return (hsTy, hsExp')
  }

vecLenAntiQuoter :: AntiQuoter HaskellIdentifier
vecLenAntiQuoter = AntiQuoter
  { aqParser = do
      hId <- C.parseIdentifier
      let cId = mangleHaskellIdentifier hId
      return (cId, C.TypeSpecifier mempty (C.Long C.Signed), hId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.Long C.Signed) -> do
          hsExp <- getHsVariable "vecCtx" cId
          hsExp' <- [| fromIntegral (vecCtxLength $(return hsExp)) |]
          hsTy <- [t| CLong |]
          hsExp'' <- [| \cont -> cont $(return hsExp') |]
          return (hsTy, hsExp'')
        _ -> do
          fail "impossible: got type different from `long' (vecCtx)"
  }


-- | 'bsCtx' serves exactly the same purpose as 'vecCtx', but only for
-- 'BS.ByteString'.  @vec-ptr@ becomes @bs-ptr@, and @vec-len@ becomes
-- @bs-len@.  You don't need to specify the type of the pointer in
-- @bs-ptr@, it will always be @char*@.
bsCtx :: Context
bsCtx = mempty
  { ctxAntiQuoters = Map.fromList
      [ ("bs-ptr", SomeAntiQuoter bsPtrAntiQuoter)
      , ("bs-len", SomeAntiQuoter bsLenAntiQuoter)
      ]
  }

bsPtrAntiQuoter :: AntiQuoter HaskellIdentifier
bsPtrAntiQuoter = AntiQuoter
  { aqParser = do
      hId <- C.parseIdentifier
      let cId = mangleHaskellIdentifier hId
      return (cId, C.Ptr [] (C.TypeSpecifier mempty (C.Char Nothing)), hId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.Ptr _ (C.TypeSpecifier _ (C.Char Nothing)) -> do
          hsTy <- [t| Ptr CChar |]
          hsExp <- getHsVariable "bsCtx" cId
          hsExp' <- [| \cont -> BS.unsafeUseAsCString $(return hsExp) $ \ptr -> cont ptr  |]
          return (hsTy, hsExp')
        _ ->
          fail "impossible: got type different from `unsigned char' (bsCtx)"
  }

bsLenAntiQuoter :: AntiQuoter HaskellIdentifier
bsLenAntiQuoter = AntiQuoter
  { aqParser = do
      hId <- C.parseIdentifier
      let cId = mangleHaskellIdentifier hId
      return (cId, C.TypeSpecifier mempty (C.Long C.Signed), hId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.Long C.Signed) -> do
          hsExp <- getHsVariable "bsCtx" cId
          hsExp' <- [| fromIntegral (BS.length $(return hsExp)) |]
          hsTy <- [t| CLong |]
          hsExp'' <- [| \cont -> cont $(return hsExp') |]
          return (hsTy, hsExp'')
        _ -> do
          fail "impossible: got type different from `long' (bsCtx)"
  }

-- Utils
------------------------------------------------------------------------

cDeclAqParser
  :: C.CParser HaskellIdentifier m
  => m (C.CIdentifier, C.Type C.CIdentifier, HaskellIdentifier)
cDeclAqParser = do
  cTy <- Parser.parens C.parseParameterDeclaration
  case C.parameterDeclarationId cTy of
    Nothing -> fail "Every captured function must be named (funCtx)"
    Just hId -> do
     let cId = mangleHaskellIdentifier hId
     cTy' <- deHaskellifyCType $ C.parameterDeclarationType cTy
     return (cId, cTy', hId)

deHaskellifyCType
  :: C.CParser HaskellIdentifier m
  => C.Type HaskellIdentifier -> m (C.Type C.CIdentifier)
deHaskellifyCType = traverse $ \hId -> do
  case C.cIdentifierFromString (unHaskellIdentifier hId) of
    Left err -> fail $ "Illegal Haskell identifier " ++ unHaskellIdentifier hId ++
                       " in C type:\n" ++ err
    Right x -> return x
