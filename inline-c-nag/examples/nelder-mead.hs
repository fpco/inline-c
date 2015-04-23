{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Vector.Storable as V
import           Foreign.C.Types
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (poke)
import qualified Language.C.Inline.Nag as C
import           System.IO.Unsafe (unsafePerformIO)

C.context C.nagCtx

C.include "<math.h>"
C.include "<nag.h>"
C.include "<nage04.h>"
C.include "<nagx02.h>"

{-# NOINLINE nelderMead #-}
nelderMead
  :: V.Vector CDouble
  -- ^ Starting point
  -> (V.Vector CDouble -> CDouble)
  -- ^ Function to minimize
  -> C.Nag_Integer
  -- ^ Maximum number of iterations (must be >= 1).
  -> Either String (CDouble, V.Vector CDouble)
  -- ^ Position of the minimum.  'Left' if something went wrong, with
  -- error message. 'Right', together with the minimum cost and its
  -- position, if it could be found.
nelderMead xImm pureFunct maxcal = unsafePerformIO $ do
    -- Create function that the C code will use.
    let funct n xc fc _comm = do
          xc' <- newForeignPtr_ xc
          let f = pureFunct $ V.unsafeFromForeignPtr0 xc' $ fromIntegral n
          poke fc f
    -- Create mutable input/output vector for C code
    x <- V.thaw xImm
    -- Call the C code
    C.withNagError $ \fail_ -> do
      minCost <- [C.stmts| double {
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

{-# NOINLINE oneVar #-}
oneVar
  :: (CDouble, CDouble)
  -- ^ Interval containing a minimum
  -> (CDouble -> CDouble)
  -- ^ Function to minimize
  -> C.Nag_Integer
  -- ^ Maximum number of iterations.
  -> Either String (CDouble, CDouble)
oneVar (a, b) fun max_fun = unsafePerformIO $ do
    let funct xc fc _comm = poke fc $ fun xc
    C.withNagError $ \fail_ -> C.withPtr $ \x -> C.withPtr_ $ \f -> do
      [C.stmts| void {
        double a = $(double a), b = $(double b);
        nag_opt_one_var_no_deriv(
          $fun:(void (*funct)(double, double*, Nag_Comm*)),
          0.0, 0.0, &a, &b, $(Integer max_fun), $(double *x), $(double *f),
          NULL, $(NagError *fail_));
        } |]

-- Optimize a two-dimensional function.  Example taken from
-- <http://www.nag.com/numeric/CL/nagdoc_cl24/examples/source/e04cbce.c>.
main :: IO ()
main = do
  let funct1 = \x ->
        let x0 = x V.! 0
            x1 = x V.! 1
        in exp x0 * (4*x0*(x0+x1)+2*x1*(x1+1.0)+1.0)
      start = V.fromList [-1, 1]
  let Right (minCost1, minPos1) = nelderMead start funct1 500
  putStrLn $ "Nelder-Mead"
  putStrLn $ "Minimum cost: " ++ show minCost1
  putStrLn $ "End positition: " ++ show (minPos1 V.! 0) ++ ", " ++ show (minPos1 V.! 1)

  let funct2 x = sin x / x
  let Right (minCost2, minPos2) = oneVar (3.5, 5) funct2 30
  putStrLn $ "One variable"
  putStrLn $ "Minimum cost: " ++ show minCost2
  putStrLn $ "End position: " ++ show minPos2
