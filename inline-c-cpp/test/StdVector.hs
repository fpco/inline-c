{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module StdVector
  ( stdVectorCtx
  , instanceStdVector
  , CStdVector
  , StdVector()
  , StdVector.new
  , size
  , toVector
  , pushBack
  )

where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe
import Language.Haskell.TH.Syntax
import Foreign
import Foreign.C
import Language.Haskell.TH
import Data.Proxy
import Control.Exception (mask_)

data CStdVector a

stdVectorCtx :: C.Context
stdVectorCtx = C.cppCtx `mappend` C.cppTypePairs [ ("std::vector", [t| CStdVector |]) ]

newtype StdVector a = StdVector (ForeignPtr (CStdVector a))

class HasStdVector a where
  cNew :: IO (Ptr (CStdVector a))
  cDelete :: FunPtr (Ptr (CStdVector a) -> IO ())
  cSize :: Ptr (CStdVector a) -> IO CSize
  cCopyTo :: Ptr (CStdVector a) -> Ptr a -> IO ()
  cPushBack :: a -> Ptr (CStdVector a) -> IO ()

instanceStdVector :: String -> DecsQ
instanceStdVector cType = fmap concat $ sequence
  [ C.include "<vector>"
  , C.include "<algorithm>"
  , C.substitute
    [ ( "T", \_ -> cType )
    , ( "VEC", \var -> "$(std::vector<" ++ cType ++ ">* " ++ var ++ ")" )
    ] [d|
      instance HasStdVector $(C.getHaskellType False cType) where
        cNew = [CU.exp| std::vector<@T()>* { new std::vector<@T()>() } |]
        cDelete = [C.funPtr| void deleteStdVector(std::vector<@T()>* vec) { delete vec; } |]
        cSize vec = [CU.exp| size_t { @VEC(vec)->size() } |]
        cCopyTo vec dstPtr = [CU.block| void {
          const std::vector<@T()>* vec = @VEC(vec);
          std::copy(vec->begin(), vec->end(), $(@T()* dstPtr));
          } |]
        cPushBack value vec = [CU.exp| void { @VEC(vec)->push_back($(@T() value)) } |]
    |]
  ]

new :: forall a. HasStdVector a => IO (StdVector a)
new = mask_ $ do
  ptr <- cNew @a
  StdVector <$> newForeignPtr cDelete ptr

size :: HasStdVector a => StdVector a -> IO Int
size (StdVector fptr) = fromIntegral <$> withForeignPtr fptr cSize

toVector :: (HasStdVector a, Storable a) => StdVector a -> IO (VS.Vector a)
toVector stdVec@(StdVector stdVecFPtr) = do
  vecSize <- size stdVec
  hsVec <- VSM.new vecSize
  withForeignPtr stdVecFPtr $ \stdVecPtr ->
    VSM.unsafeWith hsVec $ \hsVecPtr ->
      cCopyTo stdVecPtr hsVecPtr
  VS.unsafeFreeze hsVec

pushBack :: HasStdVector a => StdVector a -> a -> IO ()
pushBack (StdVector fptr) value = withForeignPtr fptr (cPushBack value)