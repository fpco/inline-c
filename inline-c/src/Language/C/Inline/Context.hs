{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | A 'Context' is used to define the capabilities of the
-- TemplateHaskell code that handles the inline C code.  See the
-- documentation of the data type for more details.
--
-- In practice, a 'Context' will have to be defined for each library
-- that defines new C types, to allow the TemplateHaskell code to
-- interpret said types correctly.
module Language.C.Inline.Context
  ( -- * 'Purity'
    Purity(..)

    -- * 'CTypesTable'
  , CTypesTable
  , convertCType
  , CArray
  , isTypeName

    -- * ''CAntiQuoter'
  , CAntiQuoter(..)
  , CAntiQuoterId
  , SomeCAntiQuoter(..)
  , CAntiQuoters

    -- * 'Context'
  , Context(..)
  , baseCtx
  , funCtx
  , vecCtx
  ) where

import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Monoid (Monoid(..))
import           Data.Typeable (Typeable)
import qualified Data.Vector.Storable.Mutable as V
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)
import qualified Language.Haskell.TH as TH
import qualified Text.Parser.Token as Parser

import           Language.C.Inline.FunPtr
import qualified Language.C.Types as C

-- | Type more descriptive than 'Bool' and used in many functions here.
data Purity = Pure | IO
  deriving (Eq, Show)

-- | A mapping from 'C.TypeSpecifier's to Haskell types.  Needed both to
-- parse C types, and to convert them to Haskell types.
type CTypesTable = Map.Map C.TypeSpecifier TH.TypeQ

-- | Specifies how to parse and process an antiquotation in the C code.
--
-- All antiquotations (apart from plain variable capture) have syntax
--
-- @
-- $XXX:YYY
-- @
--
-- Where @XXX@ is the name of the antiquoter and @YYY@ is something
-- parseable by the respective 'caqParser'.
data CAntiQuoter a = CAntiQuoter
  { caqParser :: forall m. C.CParser m => m (String, C.Type, a)
    -- ^ Parses the body of the antiquotation, returning an hint for the
    -- name to assign to the variable that will replace the
    -- anti-quotation, the type of said variable, and some arbitrary
    -- data which will then be fed to 'caqMarshaller'.
  , caqMarshaller :: CTypesTable -> Purity -> C.Type -> a -> TH.Q (TH.Type, TH.Exp)
    -- ^ Takes the type and the body returned by 'caqParser', together
    -- with the required purity and the current 'CTypesTable'.
    --
    -- Returns the Haskell type for the parameter, and the Haskell
    -- expression that will be passed in as the parameter.
    --
    -- If the 'Purity' argument is 'Pure' and the type returned is @ty@,
    -- 'TH.Exp' *must* have type @ty@.
    --
    -- If the 'Purity' argument is 'IO' and the type returned is @ty@,
    -- the 'TH.Exp' *must* have type @forall a. (ty -> IO a) -> IO a@.
  }

-- | An identifier for a 'CAntiQuoter'.
type CAntiQuoterId = String

-- | Existential wrapper around 'CAntiQuoter'.
data SomeCAntiQuoter = forall a. (Eq a, Typeable a) => SomeCAntiQuoter (CAntiQuoter a)

type CAntiQuoters = Map.Map CAntiQuoterId SomeCAntiQuoter

-- | A 'Context' stores information needed to convert C types to Haskell
-- types, and to convert Haskell values to C values.
--
-- 'Context's can be composed with their 'Monoid' instance, where
-- 'mappend' is right-biased -- in @'mappend' x y@ @y@ will take
-- precedence over @x@.
data Context = Context
  { ctxCTypesTable :: CTypesTable
  , ctxCAntiQuoters :: CAntiQuoters
  }

instance Monoid Context where
  mempty = Context
    { ctxCTypesTable = mempty
    , ctxCAntiQuoters = mempty
    }

  mappend ctx2 ctx1 = Context
    { ctxCTypesTable = ctxCTypesTable ctx1 <> ctxCTypesTable ctx2
    , ctxCAntiQuoters = ctxCAntiQuoters ctx1 <> ctxCAntiQuoters ctx2
    }

-- | Context useful to work with vanilla C.  Used by default.
--
-- 'ctxCTypesTable': converts C basic types to their counterparts
-- in "Foreign.C.Types" (TODO currently slightly incomplete).
--
-- No 'ctxCAntiQuoters'.
baseCtx :: Context
baseCtx = mempty
  { ctxCTypesTable = baseCTypesTable
  }

baseCTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
baseCTypesTable = Map.fromList
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

-- | Given a 'Context', it uses its 'ctxCTypesTable' to convert
-- arbitrary C types.
convertCType
  :: CTypesTable
  -> Purity
  -- ^ Whether function pointers should be pure or not.
  -> C.Type
  -> TH.Q (Maybe TH.Type)
convertCType cTypes pure = runMaybeT . go
  where
    goDecl = go . C.parameterDeclarationType

    go :: C.Type -> MaybeT TH.Q TH.Type
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

    buildArr [] hsRetType = case pure of
      Pure -> [t| $(return hsRetType) |]
      IO -> [t| IO $(return hsRetType) |]
    buildArr (hsPar : hsPars) hsRetType =
      [t| $(return hsPar) -> $(buildArr hsPars hsRetType) |]

isTypeName :: CTypesTable -> C.Id -> Bool
isTypeName cTypes id' = Map.member (C.TypeName id') cTypes

------------------------------------------------------------------------
-- Useful contexts

getHsVariable :: String -> String -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName s
  case mbHsName of
    Nothing -> error $ "Cannot capture Haskell variable " ++ s ++
                       ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

convertCType_ :: String -> CTypesTable -> Purity -> C.Type -> TH.Q TH.Type
convertCType_ err cTypes pure cTy = do
  mbHsType <- convertCType cTypes pure cTy
  case mbHsType of
    Nothing -> error $ "Cannot convert C type (" ++ err ++ ")"
    Just hsType -> return hsType

-- | This 'Context' includes a 'CAntiQuoter' that removes the need for
-- explicitely creating 'FunPtr's, named @"fun"@.
--
-- For example, we can capture function @f@ of type @CInt -> CInt -> IO
-- CInt@ in C code using @$fun:(int (*f)(int, int))@.
funCtx :: Context
funCtx = mempty
  { ctxCAntiQuoters = Map.fromList [("fun", SomeCAntiQuoter funPtrCAntiQuoter)]
  }

funPtrCAntiQuoter :: CAntiQuoter String
funPtrCAntiQuoter = CAntiQuoter
  { caqParser = do
      cTy <- Parser.parens C.parseParameterDeclaration
      case C.parameterDeclarationId cTy of
        Nothing -> error "Every captured function must be named (funCtx)"
        Just id' -> do
         let s = C.unId id'
         return (s, C.parameterDeclarationType cTy, s)
  , caqMarshaller = \cTypes pure cTy cId -> do
      hsTy <- convertCType_ "funCtx" cTypes pure cTy
      hsExp <- getHsVariable "funCtx" cId
      case hsTy of
        TH.AppT (TH.ConT n) hsTy' | n == ''FunPtr -> case pure of
          IO -> do
            hsExp' <- [| \cont -> cont =<< $(mkFunPtr (return hsTy')) $(return hsExp) |]
            return (hsTy, hsExp')
          Pure -> error $ "Cannot convert functions to pointers " ++
                          "in pure quotation (funCtx)"
        _ -> error "The `fun' marshaller captures function pointers only"
  }

-- | This 'Context' includes two 'CAntiQuoter's that allow to easily use
-- Haskell vectors in C.
--
-- Specifically, the @vec-len@ and @vec-ptr@ will get the length and the
-- pointer underlying mutable storable vectors, 'V.IOVector'.
--
-- To use @vec-len@, simply write @$vec-len:x@, where @x@ is something
-- of type @'V.IOVector' a@, for some @a@.  To use @vec-ptr@ you need to
-- specify the type of the pointer, e.g. @$vec-len:(int *x)@ will work
-- if @x@ has type @'V.IOVector' 'CInt'@.
vecCtx :: Context
vecCtx = mempty
  { ctxCAntiQuoters = Map.fromList
      [ ("vec-ptr", SomeCAntiQuoter vecPtrCAntiQuoter)
      , ("vec-len", SomeCAntiQuoter vecLenCAntiQuoter)
      ]
  }

vecPtrCAntiQuoter :: CAntiQuoter String
vecPtrCAntiQuoter = CAntiQuoter
  { caqParser = do
      cTy <- Parser.parens C.parseParameterDeclaration
      case C.parameterDeclarationId cTy of
        Nothing -> error "Every captured vector must be named (vecCtx)"
        Just id' -> do
         let s = C.unId id'
         return (s, C.parameterDeclarationType cTy, s)
  , caqMarshaller = \cTypes pure cTy cId -> do
      hsTy <- convertCType_ "vecCtx" cTypes pure cTy
      hsExp <- getHsVariable "vecCtx" cId
      case pure of
       IO -> do
         hsExp' <- [| V.unsafeWith $(return hsExp) |]
         return (hsTy, hsExp')
       Pure -> do
         error $ "Cannot convert vectors to pointers " ++
                 "in pure quotation (vecCtx)"
  }

vecLenCAntiQuoter :: CAntiQuoter String
vecLenCAntiQuoter = CAntiQuoter
  { caqParser = do
      cId <- C.parseIdentifier
      let s = C.unId cId
      return (s, C.TypeSpecifier mempty (C.Long C.Signed), s)
  , caqMarshaller = \_cTypes pure cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.Long C.Signed) -> do
          hsExp <- getHsVariable "vecCtx" cId
          hsExp' <- [| fromIntegral (V.length $(return hsExp)) |]
          hsTy <- [t| CLong |]
          case pure of
            Pure -> do
              return (hsTy, hsExp')
            IO -> do
              hsExp'' <- [| \cont -> cont $(return hsExp') |]
              return (hsTy, hsExp'')
        _ -> do
          error "impossible: got type different from `long' (vecCtx)"
  }
