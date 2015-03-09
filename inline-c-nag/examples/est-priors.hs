{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Language.C.Inline
import           Language.C.Inline.Nag
import           Foreign.ForeignPtr
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.Marshal.Alloc (alloca)
import qualified Data.Vector.Storable as V

setContext nagCtx

include "<nag.h>"
include "<math.h>"
include "<stdio.h>"
include "<nag_stdlib.h>"
include "<nage04.h>"
include "<nagx02.h>"

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
  -> IO (Either String (Optimx (V.Vector CDouble)))
  -- ^ Position of the minimum.  'Left' if something went wrong, with
  -- error message.
nelderMead x pureFunct maxcal = do
    -- Create mutable input/output vector for C code
    xMut <- V.thaw x
    alloca $ \fPtr -> alloca $ \ncallsPtr -> do
      -- Create function that the C code will use
      let funct n' xPtr fPtr' _comm = do
            xFPtr <- newForeignPtr_ xPtr
            let f = pureFunct $ V.unsafeFromForeignPtr0 xFPtr $ fromIntegral n'
            poke fPtr' f
      -- Create monitoring function, to record number of calls
      let monit _fmin _fmax _sim _n ncall _serror _vratio _comm = do
            poke ncallsPtr ncall
      -- Call the C code
      res <- withNagError $ \fail_ -> [citems|
        void {
            Nag_Comm comm;
            double tolf = sqrt(nag_machine_precision);
            double tolx = sqrt(tolf);
            nag_opt_simplex_easy(
              $vec-len:xMut, $vec-ptr:(double *xMut), $(double *fPtr), tolf, tolx,
              $fun:(void (*funct)(Integer n, const double *xc, double *fc, Nag_Comm *comm)),
              $fun:(void (*monit)(double fmin, double fmax, const double sim[], Integer n, Integer ncall, double serror, double vratio, Nag_Comm *comm)),
              $(Integer maxcal), &comm, $(NagError *fail_));
        }
      |]
      case res of
        Left err -> return $ Left err
        Right () -> do
          f <- peek fPtr
          x' <- V.unsafeFreeze xMut
          ncalls <- peek ncallsPtr
          return $ Right Optimx
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
