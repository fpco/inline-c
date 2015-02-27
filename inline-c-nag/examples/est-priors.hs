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

include "<nag.h>"
include "<math.h>"
include "<stdio.h>"
include "<nag_stdlib.h>"
include "<nage04.h>"
include "<nagx02.h>"

type CConst a = a

data Optimx x = Optimx
  { optimxBest :: !x
  , optimxMinCost :: !CDouble
  , optimxCalls :: !CLong
  } deriving (Eq, Show)

nelderMead
  :: V.Vector CDouble
  -- ^ Starting point
  -> (V.Vector CDouble -> CDouble)
  -- ^ Function to minimize
  -> CLong
  -- ^ Maximum number of iterations (must be >= 1).
  -> IO (Maybe (Optimx (V.Vector CDouble)))
  -- ^ Position of the minimum.  'Nothing' if something went wrong
  -- (prints the error message)
nelderMead x pureFunct maxcal = do
    let n = fromIntegral $ V.length x
    -- Create mutable input/output vector for C code
    xMut <- V.thaw x
    alloca $ \fPtr -> alloca $ \ncallsPtr -> VM.unsafeWith xMut $ \xMutPtr -> do
      -- Create function that the C code will use
      let funct n' xPtr fPtr _comm = do
            xFPtr <- newForeignPtr_ xPtr
            let f = pureFunct $ V.unsafeFromForeignPtr0 xFPtr $ fromIntegral n'
            poke fPtr f
      -- Create monitoring function, to record number of calls
      let monit _fmin _fmax _sim _n ncall _serror _vratio _comm = do
            poke ncallsPtr ncall
      -- Call the C code
      res <- [citems|
        int(void (*funct)(Integer n, const double *xc, double *fc, Nag_Comm *comm),
            void (*monit)(double fmin, double fmax, const double sim[], Integer n, Integer ncall, double serror, double vratio, Nag_Comm *comm)) {
            NagError fail; INIT_FAIL(fail);
            Nag_Comm comm;
            double tolf = sqrt(nag_machine_precision);
            double tolx = sqrt(tolf);
            nag_opt_simplex_easy(
              n_nint, xMutPtr_double_ptr, fPtr_double_ptr, tolf, tolx,
              funct, monit, maxcal_nint, &comm, &fail);
            if (fail.code != NE_NOERROR) {
              printf("Error from nag_opt_simplex_easy (e04cbc).\n%s\n", fail.message);
            }
            return fail.code != NE_NOERROR;
        }
      |]
      if res /= 0
        then return Nothing
        else do
          f <- peek fPtr
          x' <- V.unsafeFreeze xMut
          ncalls <- peek ncallsPtr
          return $ Just Optimx
            { optimxBest = x'
            , optimxMinCost = f
            , optimxCalls = ncalls
            }

main :: IO ()
main = do
  let funct = \x ->
        let x0 = x V.! 0
            x1 = x V.! 1
        in exp x0 * (4*x0*(x0+x1)+2*x1*(x1+1.0)+1.0)
      start = V.fromList [-1, 1]
  print =<< nelderMead start funct 500
