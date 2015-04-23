{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad (forM, when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control (liftBaseOp)
import           Control.Monad.Trans.Except (runExceptT, ExceptT(..), throwE)
import           Data.Functor ((<$>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Nag as C
import           System.IO.Unsafe (unsafePerformIO)

C.context C.nagCtx

C.include "<math.h>"
C.include "<nag.h>"
C.include "<nagd02.h>"
C.include "<stdio.h>"

data Method
  = RK_2_3
  | RK_4_5
  | RK_7_8
  deriving (Eq, Show)

data SolveOptions = SolveOptions
  { _soMethod :: Method
  , _soTolerance :: CDouble
  , _soInitialStepSize :: CDouble
  } deriving (Eq, Show)

{-# NOINLINE solve #-}
solve
  :: SolveOptions
  -> (CDouble -> V.Vector CDouble -> V.Vector CDouble)
  -- ^ ODE to solve
  -> [CDouble]
  -- ^ @x@ values at which to approximate the solution.
  -> V.Vector CDouble
  -- ^ The initial values of the solution.
  -> Either String [(CDouble, V.Vector CDouble)]
  -- ^ Either an error, or the @y@ values corresponding to the @x@
  -- values input.
solve (SolveOptions method tol hstart) f xs y0 = unsafePerformIO $ runExceptT $ do
  when (length xs < 2) $
    throwE "You have to provide a minimum of 2 values for @x@"
  let tstart = head xs
  let tend = last xs
  iwsav <- lift $ VM.new liwsav
  rwsav <- lift $ VM.new lrwsav
  let thresh = V.replicate n 0
  methodInt <- lift $ case method of
    RK_2_3 -> [C.exp| int{ Nag_RK_2_3 } |]
    RK_4_5 -> [C.exp| int{ Nag_RK_4_5 } |]
    RK_7_8 -> [C.exp| int{ Nag_RK_7_8 } |]
  ExceptT $ C.withNagError $ \fail_ ->
    [C.exp| void{ nag_ode_ivp_rkts_setup(
      $(Integer n_c), $(double tstart), $(double tend), $vec-ptr:(double *y0),
      $(double tol), $vec-ptr:(double *thresh), $(int methodInt),
      Nag_ErrorAssess_off, $(double hstart), $vec-ptr:(Integer *iwsav),
      $vec-ptr:(double *rwsav), $(NagError *fail_))
    } |]
  ygot <- lift $ VM.new n
  ypgot <- lift $ VM.new n
  ymax <- lift $ VM.new n
  let fIO :: CDouble -> C.Nag_Integer -> Ptr CDouble -> Ptr CDouble -> Ptr C.Nag_Comm -> IO ()
      fIO t n y _yp  _comm = do
        yFore <- newForeignPtr_ y
        let yVec = VM.unsafeFromForeignPtr0 yFore $ fromIntegral n
        ypImm <- f t <$> V.unsafeFreeze yVec
        V.copy yVec ypImm
  liftBaseOp C.initNagError $ \fail_ -> do
    -- Tail because the first point is the start
    ys <- forM (tail xs) $ \t -> do
      ExceptT $ C.checkNagError fail_ $ [C.stmts| void {
          double tgot;
          nag_ode_ivp_rkts_range(
            $fun:(void (*fIO)(double t, Integer n, const double y[], double yp[], Nag_Comm *comm)),
            $(Integer n_c), $(double t), &tgot, $vec-ptr:(double *ygot),
            $vec-ptr:(double *ypgot), $vec-ptr:(double *ymax), NULL,
            $vec-ptr:(Integer *iwsav), $vec-ptr:(double *rwsav),
            $(NagError *fail_));
        } |]
      y <- lift $ V.freeze ygot
      return (t, y)
    return $ (tstart, y0) : ys
  where
    n = V.length y0
    liwsav = 130
    lrwsav = 350 + 32 * n
    n_c = fromIntegral n

main :: IO ()
main = do
  let opts = SolveOptions RK_4_5 1e-8 0
  let f _t y = V.fromList [y V.! 1, -(y V.! 0)]
  case solve opts f [0,pi/4..pi] (V.fromList [0, 1]) of
    Left err -> putStrLn err
    Right x -> print x
