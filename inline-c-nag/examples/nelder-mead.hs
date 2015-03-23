{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (poke)
import           Language.C.Inline.Nag

setContext nagCtx

include "<math.h>"
include "<nag.h>"
include "<nage04.h>"
include "<nagx02.h>"

nelderMead
  :: V.Vector Double
  -- ^ Starting point
  -> (V.Vector Double -> Double)
  -- ^ Function to minimize
  -> Nag_Integer
  -- ^ Maximum number of iterations (must be >= 1).
  -> IO (Either String (Double, V.Vector Double))
  -- ^ Position of the minimum.  'Left' if something went wrong, with
  -- error message. 'Right', together with the minimum cost and its
  -- position, if it could be found.
nelderMead xImm pureFunct maxcal = do
    -- Create function that the C code will use.
    let funct n xc fc _comm = do
          xc' <- newForeignPtr_ xc
          let f = pureFunct $ V.unsafeFromForeignPtr0 xc' $ fromIntegral n
          poke fc f
    -- Create mutable input/output vector for C code
    x <- V.thaw xImm
    -- Call the C code
    withNagError $ \fail_ -> do
      minCost <- [c| double {
          // The function takes an exit parameter to store the minimum
          // cost.
          double f;
          // We hardcode sensible values (see NAG documentation) for the
          // error tolerance, computed using NAG's nag_machine_precision.
          double tolf = sqrt(nag_machine_precision);
          double tolx = sqrt(tolf);
          // Call the function
          nag_opt_simplex_easy(
            // Get vector length and pointer.
            $vec-len:x, $vec-ptr:(double *x),
            &f, tolf, tolx,
            // Pass function pointer to our Haskell function using the fun
            // anti-quotation.
            $fun:(void (*funct)(Integer n, const double *xc, double *fc, Nag_Comm *comm)),
            // We do not provide a "monitoring" function.
            NULL,
            // Capture Haskell variable with the max number of iterations.
            $(Integer maxcal),
            // Do not provide the Nag_Comm parameter, which we don't need.
            NULL,
            // Pass the NagError parameter provided by withNagError
            $(NagError *fail_));
          return f;
        } |]
      -- Get a new immutable vector by freezing the mutable one.
      minCostPos <- V.freeze x
      return (minCost, minCostPos)

-- Optimize a two-dimensional function.  Example taken from
-- <http://www.nag.com/numeric/CL/nagdoc_cl24/examples/source/e04cbce.c>.
main :: IO ()
main = do
  let funct = \x ->
        let x0 = x V.! 0
            x1 = x V.! 1
        in exp x0 * (4*x0*(x0+x1)+2*x1*(x1+1.0)+1.0)
      start = V.fromList [-1, 1]
  Right (minCost, minPos) <- nelderMead start funct 500
  putStrLn $ "Minimum cost: " ++ show minCost
  putStrLn $ "End positition: " ++ show (minPos V.! 0) ++ ", " ++ show (minPos V.! 1)
