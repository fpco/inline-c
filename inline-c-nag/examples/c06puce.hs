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
import qualified Language.C.Inline.Nag as C

C.context C.nagCtx

C.include "<nag.h>"
C.include "<stdio.h>"
C.include "<nag_stdlib.h>"
C.include "<nagc06.h>"

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

parseBounds :: IO (Int64, Int64)
parseBounds = do
  (m, n) <- C.withPtr $ \m -> C.withPtr_ $ \n ->
    [C.exp| void{ scanf("%*[^\n] %ld%ld%*[^\n]", $(long *m), $(long *n)) } |]
  return (fi m, fi n)

parseData :: (Int64, Int64) -> IO (A.StorableArray (Int64, Int64) C.Complex)
parseData (m0, n0) = do
  x <- A.newArray ((0, 0), (m0, n0)) $ C.Complex 0 0
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \xPtr -> [C.stmts| void(Complex *xPtr) {
      int i;
      for (i = 0; i < $(Integer m) * $(Integer n); i++)
        scanf(" ( %lf , %lf ) ", &xPtr[i].re, &xPtr[i].im);
    } |]
  return x

printGenComplxMat
  :: String -> A.StorableArray (Int64, Int64) C.Complex -> IO CInt
printGenComplxMat str x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  withCString str $ \str -> A.withStorableArray x $ \xPtr ->
    [C.stmts| int {
      NagError fail; INIT_FAIL(fail);
      nag_gen_complx_mat_print_comp(
        Nag_RowMajor, Nag_GeneralMatrix, Nag_NonUnitDiag, $(Integer n), $(Integer m),
        $(Complex *xPtr), $(Integer m), Nag_BracketForm, "%6.3f", $(char *str),
        Nag_NoLabels, 0, Nag_NoLabels, 0, 80, 0, NULL, &fail);
      return fail.code != NE_NOERROR;
    } |]

sumFftComplex2d
  :: CInt -> A.StorableArray (Int64, Int64) C.Complex -> IO CInt
sumFftComplex2d flag x = do
  ((0, 0), (m0, n0)) <- A.getBounds x
  let (m, n) = (fi m0, fi n0)
  A.withStorableArray x $ \xPtr -> [C.stmts| int {
    NagError fail; INIT_FAIL(fail);
    nag_sum_fft_complex_2d($(int flag), $(Integer m), $(Integer n), $(Complex *xPtr), &fail);
    return fail.code != NE_NOERROR;
  } |]

main :: IO ()
main = do
  bounds <- parseBounds
  x <- parseData bounds
  void $ printGenComplxMat "\n Original data values\n" x
  void $ sumFftComplex2d <$> [C.exp| int{ Nag_ForwardTransform } |] <*> return x
  void $ printGenComplxMat "\n Components of discrete Fourier transform\n" x
  void $ sumFftComplex2d <$> [C.exp| int{ Nag_BackwardTransform } |] <*> return x
  void $ printGenComplxMat "\n Original sequence as restored by inverse transform\n" x
