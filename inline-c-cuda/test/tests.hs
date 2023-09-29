{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import           Control.Exception.Safe
import           Control.Monad
import qualified Language.C.Inline.Context as CC
import qualified Language.C.Types as CT
import qualified Language.C.Inline.Cuda as C
import qualified Test.Hspec as Hspec
import           Test.Hspec (shouldBe)
import           Foreign.Ptr (Ptr)
import           Data.Monoid
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc
import           Foreign.Storable


C.context $ C.cudaCtx

C.include "<iostream>"
C.include "<stdexcept>"

[C.emitBlock|
__global__ void
vectorAdd(const float *A, const float *B, float *C, int numElements)
{
    int i = blockDim.x * blockIdx.x + threadIdx.x;

    if (i < numElements)
    {
        C[i] = A[i] + B[i];
    }
}
|]

cudaAllocaArray :: forall b. Int -> (Ptr C.CFloat -> IO b) -> IO b
cudaAllocaArray size func = do
  let csize = fromIntegral size
  alloca $ \(ptr_d_A :: Ptr (Ptr C.CFloat)) -> do
    [C.block| void {
      cudaError_t err = cudaMalloc((void **)$(float** ptr_d_A), $(int csize) * sizeof(float));
      if (err != cudaSuccess)
      {
          fprintf(stderr, "Failed to allocate device vector C (error code %s)!\n", cudaGetErrorString(err));
          exit(EXIT_FAILURE);
      }
    } |]
    d_A <- peekElemOff ptr_d_A 0
    ret <- func d_A
    [C.block| void {
      cudaError_t err = cudaFree($(float* d_A));
      if (err != cudaSuccess)
      {
          fprintf(stderr, "Failed to free device vector A (error code %s)!\n", cudaGetErrorString(err));
          exit(EXIT_FAILURE);
      }
    } |]
    return ret

cudaMemcpyHostToDevice :: Int -> Ptr C.CFloat -> Ptr C.CFloat -> IO ()
cudaMemcpyHostToDevice num host device = do
  let cnum = fromIntegral num
  [C.block| void {
      cudaError_t err = cudaMemcpy($(float* device), $(float* host), $(int cnum) * sizeof(float), cudaMemcpyHostToDevice);
      if (err != cudaSuccess)
      {
          fprintf(stderr, "Failed to copy vector from host to device (error code %s)!\n", cudaGetErrorString(err));
          exit(EXIT_FAILURE);
      }
  } |]

cudaMemcpyDeviceToHost :: Int -> Ptr C.CFloat -> Ptr C.CFloat -> IO ()
cudaMemcpyDeviceToHost num device host = do
  let cnum = fromIntegral num
  [C.block| void {
      cudaError_t err = cudaMemcpy($(float* host), $(float* device), $(int cnum) * sizeof(float), cudaMemcpyDeviceToHost);
      if (err != cudaSuccess)
      {
          fprintf(stderr, "Failed to copy vector C from device to host (error code %s)!\n", cudaGetErrorString(err));
          exit(EXIT_FAILURE);
      }
  } |]


main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Basic CUDA" $ do
    Hspec.it "Add vectors on device" $ do
      let numElements = 50000
          cNumElements = fromIntegral numElements
      allocaArray numElements $ \(h_A :: Ptr C.CFloat) -> do
        allocaArray numElements $ \(h_B :: Ptr C.CFloat) -> do
          allocaArray numElements $ \(h_C :: Ptr C.CFloat) -> do
            cudaAllocaArray numElements $ \(d_A :: Ptr C.CFloat) -> do
              cudaAllocaArray numElements $ \(d_B :: Ptr C.CFloat) -> do
                cudaAllocaArray numElements $ \(d_C :: Ptr C.CFloat) -> do
                  [C.block| void {
                    for (int i = 0; i < $(int cNumElements); ++i)
                    {
                        $(float* h_A)[i] = rand()/(float)RAND_MAX;
                        $(float* h_B)[i] = rand()/(float)RAND_MAX;
                    }
                  } |]
                  cudaMemcpyHostToDevice numElements h_A d_A
                  cudaMemcpyHostToDevice numElements h_B d_B
                  [C.block| void {
                    int threadsPerBlock = 256;
                    int blocksPerGrid =($(int cNumElements) + threadsPerBlock - 1) / threadsPerBlock;
                    vectorAdd<<<blocksPerGrid, threadsPerBlock>>>($(float* d_A), $(float* d_B), $(float* d_C), $(int cNumElements));
                  } |]
                  cudaMemcpyDeviceToHost numElements d_C h_C
                  lA <- peekArray numElements h_A
                  lB <- peekArray numElements h_B
                  lC <- peekArray numElements h_C
                  all (< 1e-5) (map (\((a,b),c) -> abs(a + b - c)) (zip (zip lA lB) lC)) `shouldBe` True
