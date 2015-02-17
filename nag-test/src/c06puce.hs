{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import           Language.C.Inline
import qualified Language.C as C
import qualified Language.C.Quote.Nag as C
import           Language.C.Context.Nag
import           Foreign.C.Types
import qualified Data.Array.Storable as A
import           Text.RawString.QQ (r)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peek)
import           Data.Functor ((<$>))
import           Control.Applicative ((<*>))
import           Data.Int (Int64)
import           Control.Monad (void)
import           Foreign.C.String (withCString)

setContext nagCtx

emitInclude "<nag.h>"
emitInclude "<stdio.h>"
emitInclude "<nag_stdlib.h>"
emitInclude "<nagc06.h>"

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

parseBounds :: IO (Int64, Int64)
parseBounds =
  alloca $ \mPtr -> alloca $ \nPtr -> do
    [cexp| void(Integer *mPtr, Integer *nPtr) { scanf("%*[^\n] %ld%ld%*[^\n]", mPtr, nPtr) } |]
    (,) <$> (fi <$> peek mPtr) <*> (fi <$> peek nPtr)

parseData :: (Int64, Int64) -> IO (A.StorableArray (Int64, Int64) Complex)
parseData (m0, n0) = do
  x <- A.newArray ((0, 0), (m0, n0)) $ Complex 0 0
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \ptr -> [citems| void(Complex *ptr) {
      int i;
      for (i = 0; i < m_nint*n_nint; i++)
        scanf(" ( %lf , %lf ) ", &ptr[i].re, &ptr[i].im);
    } |]
  return x

printGenComplxMat
  :: String -> A.StorableArray (Int64, Int64) Complex -> IO CInt
printGenComplxMat str x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  withCString str $ \str_ptr -> A.withStorableArray x $ \x_ptr ->
    [citems| int(Complex *x_ptr, char *str_ptr) {
      NagError fail; INIT_FAIL(fail);
      nag_gen_complx_mat_print_comp(
        Nag_RowMajor, Nag_GeneralMatrix, Nag_NonUnitDiag, n_nint, m_nint,
        x_ptr, m_nint, Nag_BracketForm, "%6.3f", str_ptr,
        Nag_NoLabels, 0, Nag_NoLabels, 0, 80, 0, NULL, &fail);
      return fail.code != NE_NOERROR;
    } |]

sumFftComplex2d
  :: CInt -> A.StorableArray (Int64, Int64) Complex -> IO CInt
sumFftComplex2d flag x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \ptr -> [citems| int(Complex *ptr) {
    NagError fail; INIT_FAIL(fail);
    nag_sum_fft_complex_2d(flag_int, m_nint, n_nint, ptr, &fail);
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

