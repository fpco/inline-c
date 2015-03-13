{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (Storable, peek, poke)
import           Language.C.Inline.Nag
import           Data.Functor ((<$>))
import           Data.IORef

setContext nagCtx

include "<stdio.h>"
include "<nag.h>"
include "<nagd02.h>"

type Fn
  =  CDouble
     -- ^ The indipendent variable @x@
  -> V.Vector CDouble
     -- ^ @y_i@ for @i = 1, 2, ..., neq@
  -> V.Vector CDouble
     -- ^ @f_i@ for @i = 1, 2, ..., neq@

type Jac
  =  CDouble
     -- ^ The indipendent variable @x@
  -> V.Vector CDouble
     -- ^ @y_i@ for @i = 1, 2, ..., neq@
  -> V.Vector CDouble
     -- ^ Jacobian matrix, @pw[(i - 1) * neq + j - 1@ must contain the
     -- value of @∂f_i/∂y_j@, for @i, j = 1, 2, ..., neq@

type Step a
  =  ( a
     ,    a
          -- ^ Some state
       -> CDouble
          -- ^ The indipendent variable @x@
       -> V.Vector CDouble
          -- ^ @y_i@ for @i = 1, 2, ..., neq@
       -> (CDouble, a)
     )

type Interval = (CDouble, V.Vector CDouble, CDouble)

type Result = [(CDouble, V.Vector CDouble)]

data Failure = Failure
  { _failureMessage :: String
  , _failureAt :: CDouble
  } deriving (Eq, Show)

vectorFromC :: Storable a => Nag_Integer -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' $ fromIntegral len

vectorToC :: Storable a => V.Vector a -> Nag_Integer -> Ptr a -> IO ()
vectorToC vec neq ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' $ fromIntegral neq) vec

solve :: CDouble -> Interval -> Fn -> Jac -> Step a -> IO (Either Failure Result)
solve tol (x, y, xend) fcn jac (stepState0, step)  = do
  let fcnIO neq x y f _comm = do
        fImm <- fcn x <$> vectorFromC neq y
        vectorToC fImm neq f
  let jacIO neq x y pw  _comm = do
        pwImm <- jac x <$> vectorFromC neq y
        vectorToC pwImm (neq*neq) pw
  resultRef <- newIORef []
  stepStateRef <- newIORef stepState0
  let stepIO neq xsol y _comm = do
        x <- peek xsol
        y' <- vectorFromC neq y
        modifyIORef resultRef ((x, y') :)
        stepState <- readIORef stepStateRef
        let (x', stepState') = step stepState x y'
        writeIORef stepStateRef stepState'
        poke xsol $ x' + 5
  xendRef <- newIORef 0
  res <- withNagError $ \fail_ -> do
    xend' <- [c| double {
        double x = $(double x);
        Nag_User comm;
        nag_ode_ivp_bdf_gen(
          $vec-len:y,
          $fun:(void (*fcnIO)(Integer neq, double x, const double y[], double f[], Nag_User *comm)),
          $fun:(void (*jacIO)(Integer neq, double x, const double y[], double pw[], Nag_User *comm)),
          &x, $vec-ptr:(double y[]), $(double xend), $(double tol), Nag_Relative,
          $fun:(void (*stepIO)(Integer neq, double *xsol, const double y[], Nag_User *comm)),
          NULLDFN, &comm, $(NagError *fail_));
        return x;
      } |]
    writeIORef xendRef xend'
    reverse <$> readIORef resultRef
  case res of
    Left s -> do
      xend' <- readIORef xendRef
      return $ Left $ Failure s xend'
    Right x -> return $ Right x

main :: IO ()
main = do
  let fcn _x y =
        let y0 = y V.! 0
            y1 = y V.! 1
            y2 = y V.! 2
        in V.fromList
          [ y0 * (-0.04) + y1 * 1e4 * y2
          , y0 * 0.04 - y1 * 1e4 * y2 - y1 * 3e7 * y1
          , y1 * 3e7 * y1
          ]
  let jac _x y =
        let _y0 = y V.! 0
            y1 = y V.! 1
            y2 = y V.! 2
        in V.fromList
          [  -0.04,  y2 * 1e4,                y1 * 1e4
          ,  0.04,   y2 * (-1e4) - y1 * 6e7,  y1 * (-1e4)
          ,  0.0,    y1 * 6e7,                0.0
          ]
  let x = 0.0
  let y = V.fromList [1.0, 0.0, 0.0]
  let xend = 10
  let step (k, h) _ _ = (xend - k * h, (k-1, h))
  let tol = 10**(-3)
  let k = 4
  let h = (xend - x) / (k + 1)
  res <- solve tol (x, y, xend) fcn jac ((k, h), step)
  print res
