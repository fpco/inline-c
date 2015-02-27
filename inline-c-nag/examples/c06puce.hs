{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import           Control.Applicative ((<*>))
import           Control.Monad (void)
import qualified Data.Array.Storable as A
import           Data.Functor ((<$>))
import           Data.Int (Int64)
import           Foreign.C.String (withCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peek)
import qualified Language.C as C
import           Language.C.Inline
import           Language.C.Inline.Nag
import qualified Language.C.Quote.Nag as C
import           Text.RawString.QQ (r)

setContext nagCtx

include "<nag.h>"
include "<stdio.h>"
include "<nag_stdlib.h>"
include "<nagc06.h>"

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

parseBounds :: IO (Int64, Int64)
parseBounds =
  alloca $ \m -> alloca $ \n -> do
    [cexp| void{ scanf("%*[^\n] %ld%ld%*[^\n]", m_long_ptr, n_long_ptr) } |]
    (,) <$> (fi <$> peek m) <*> (fi <$> peek n)

parseData :: (Int64, Int64) -> IO (A.StorableArray (Int64, Int64) Complex)
parseData (m0, n0) = do
  x <- A.newArray ((0, 0), (m0, n0)) $ Complex 0 0
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \x -> [citems| void {
      int i;
      for (i = 0; i < m_nint*n_nint; i++)
        scanf(" ( %lf , %lf ) ", &x_ncompl_ptr[i].re, &x[i].im);
    } |]
  return x

printGenComplxMat
  :: String -> A.StorableArray (Int64, Int64) Complex -> IO CInt
printGenComplxMat str x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  withCString str $ \str -> A.withStorableArray x $ \x ->
    [citems| int {
      NagError fail; INIT_FAIL(fail);
      nag_gen_complx_mat_print_comp(
        Nag_RowMajor, Nag_GeneralMatrix, Nag_NonUnitDiag, n_nint, m_nint,
        x_ncompl_ptr, m_nint, Nag_BracketForm, "%6.3f", str_char_ptr,
        Nag_NoLabels, 0, Nag_NoLabels, 0, 80, 0, NULL, &fail);
      return fail.code != NE_NOERROR;
    } |]

sumFftComplex2d
  :: CInt -> A.StorableArray (Int64, Int64) Complex -> IO CInt
sumFftComplex2d flag x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \x -> [citems| int {
    NagError fail; INIT_FAIL(fail);
    nag_sum_fft_complex_2d(flag_int, m_nint, n_nint, x_ncompl_ptr, &fail);
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
