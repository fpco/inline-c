{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Language.C.Inline
import           Language.C.Inline.Nag
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.Marshal.Alloc (alloca)
import           Data.Functor ((<$>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

setContext nagCtx

emitInclude "<nag.h>"
emitInclude "<math.h>"
emitInclude "<stdio.h>"
emitInclude "<nag_stdlib.h>"
emitInclude "<nage04.h>"
emitInclude "<nagx02.h>"

type CConst a = a

nelderMead
  :: V.Vector CDouble
  -- ^ Starting point
  -> (V.Vector CDouble -> CDouble)
  -- ^ Function to minimize
  -> CLong
  -- ^ Maximum number of iterations (must be >= 1).
  -> IO (Maybe (V.Vector CDouble))
  -- ^ Position of the minimum.  Error code and error message if
  -- something goes wrong.
nelderMead x pureFunct maxcal = do
    let n = fromIntegral $ V.length x
    -- Create mutable input/output vector for C code
    xMut <- V.thaw x
    -- Create function that the C code will use
    funct <-
      $(mkFunPtr [t| CLong -> CConst (Ptr CDouble) -> Ptr CDouble -> Ptr Nag_Comm -> IO () |]) $
      \n' xPtr fPtr _comm -> do
        xFPtr <- newForeignPtr_ xPtr
        let f = pureFunct $ V.unsafeFromForeignPtr0 xFPtr $ fromIntegral n'
        poke fPtr f
    -- Call the C code
    res <- alloca $ \fPtr -> VM.unsafeWith xMut $ \xMutPtr -> [citems|
      int(void (*funct)(Integer n, const double *xc, double *fc, Nag_Comm *comm)) {
          NagError fail;
          INIT_FAIL(fail);
          Nag_Comm comm;
          double tolf = sqrt(nag_machine_precision);
          double tolx = sqrt(tolf);
          nag_opt_simplex_easy(
            n_nint, xMutPtr_double_ptr, fPtr_double_ptr, tolf, tolx,
            funct, NULL, maxcal_nint, &comm, &fail);
          return fail.code != NE_NOERROR;
      }
    |]
    if res /= 0
      then return Nothing
      else Just <$> V.unsafeFreeze xMut

main :: IO ()
main = do
  let funct = \x ->
        let x0 = x V.! 0
            x1 = x V.! 1
        in exp x0 * (4*x0*(x0+x1)+2*x1*(x1+1.0)+1.0)
      start = V.fromList [-1, 1]
  Just best <- nelderMead start funct 100
  print best
