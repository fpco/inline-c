{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import           Control.Applicative ((<*>))
import           Control.Monad (void)
import qualified Data.Array.Storable as A
import           Data.Functor ((<$>))
import           Data.Int (Int64)
import           Foreign.C.String (withCString)
import           Language.C.Inline.Nag

setContext nagCtx

include "<nag.h>"
include "<stdio.h>"
include "<nag_stdlib.h>"
include "<nagc06.h>"

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

parseBounds :: IO (Int64, Int64)
parseBounds = do
  (m, n) <- withPtr $ \m -> withPtr_ $ \n ->
    [cexp| void{ scanf("%*[^\n] %ld%ld%*[^\n]", $(long *m), $(long *n)) } |]
  return (fi m, fi n)

parseData :: (Int64, Int64) -> IO (A.StorableArray (Int64, Int64) Complex)
parseData (m0, n0) = do
  x <- A.newArray ((0, 0), (m0, n0)) $ Complex 0 0
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \xPtr -> [c| void(Complex *xPtr) {
      int i;
      for (i = 0; i < $(Integer m) * $(Integer n); i++)
        scanf(" ( %lf , %lf ) ", &xPtr[i].re, &xPtr[i].im);
    } |]
  return x

printGenComplxMat
  :: String -> A.StorableArray (Int64, Int64) Complex -> IO CInt
printGenComplxMat str x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  withCString str $ \str -> A.withStorableArray x $ \xPtr ->
    [c| int {
      NagError fail; INIT_FAIL(fail);
      nag_gen_complx_mat_print_comp(
        Nag_RowMajor, Nag_GeneralMatrix, Nag_NonUnitDiag, $(Integer n), $(Integer m),
        $(Complex *xPtr), $(Integer m), Nag_BracketForm, "%6.3f", $(char *str),
        Nag_NoLabels, 0, Nag_NoLabels, 0, 80, 0, NULL, &fail);
      return fail.code != NE_NOERROR;
    } |]

sumFftComplex2d
  :: CInt -> A.StorableArray (Int64, Int64) Complex -> IO CInt
sumFftComplex2d flag x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \xPtr -> [c| int {
    NagError fail; INIT_FAIL(fail);
    nag_sum_fft_complex_2d($(int flag), $(Integer m), $(Integer n), $(Complex *xPtr), &fail);
    return fail.code != NE_NOERROR;
  } |]

main :: IO ()
main = do
  bounds <- parseBounds
  x <- parseData bounds
  void $ printGenComplxMat "\n Original data values\n" x
  void $ sumFftComplex2d [cexp_pure| int{ Nag_ForwardTransform } |] x
  void $ printGenComplxMat "\n Components of discrete Fourier transform\n" x
  void $ sumFftComplex2d [cexp_pure| int{ Nag_BackwardTransform } |] x
  void $ printGenComplxMat "\n Original sequence as restored by inverse transform\n" x
