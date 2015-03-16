{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (Storable, peek, poke)
import           Language.C.Inline.Nag
import           Data.Functor ((<$>))
import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)
import           Foreign.Ptr (nullFunPtr)

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

data Times a = Times
  { timesStartState :: a
  , timesStart :: CDouble
  , timesEnd :: CDouble
  , timesStep :: a -> CDouble -> V.Vector CDouble -> (a, CDouble)
  }

timesList :: CDouble -> CDouble -> Int -> Times Int
timesList start interval n = Times
  { timesStartState = 0
  , timesStart = start
  , timesEnd = start + interval * fromIntegral n
  , timesStep = \n' x _y -> if n' + 1 == n
      then (n' + 1, end)        -- This is to make sure that we step n times
      else (n' + 1, x + interval)
  }
  where
    end = start + interval * fromIntegral n

data Failure = Failure
  { _failureMessage :: String
  , _failureAt :: CDouble
  } deriving (Eq, Show)

type Result = [(CDouble, V.Vector CDouble)]

data ErrorControl
  = Relative
  | Absolute
  | Mixed
  deriving (Eq, Show)

data Options = Options
  { optionsTolerance :: !CDouble
  , optionsErrorControl :: !ErrorControl
  } deriving (Eq, Show)

{-# NOINLINE solve #-}
solve :: Options -> Fn -> Maybe Jac -> Times a -> V.Vector CDouble -> Either Failure Result
solve Options{..} fcn mbJac Times{..} y =
  unsafePerformIO $ do
    -- IO version of the right-hande function
    let fcnIO neq x y f _comm = do
          fImm <- fcn x <$> vectorFromC neq y
          vectorToC fImm neq f
    -- Function pointer for the Jacobian function.  We use a function
    -- pointer directly because we want it to be NULL if the user hasn't
    -- provided a function.
    jacFunPtr <- case mbJac of
      Nothing -> return nullFunPtr
      Just jac -> do
        let jacIO neq x y pw  _comm = do
              pwImm <- jac x <$> vectorFromC neq y
              vectorToC pwImm (neq*neq) pw
        $(mkFunPtr [t| Nag_Integer -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr Nag_User -> IO () |]) jacIO
    -- Stepping handling.  We store the trajectory and the stepping
    -- state in an 'IORef'.
    resultRef <- newIORef []
    stepStateRef <- newIORef timesStartState
    let stepIO neq xsol y _comm = do
          x <- peek xsol
          y' <- vectorFromC neq y
          modifyIORef resultRef ((x, y') :)
          stepState <- readIORef stepStateRef
          let (stepState', x') = timesStep stepState x y'
          writeIORef stepStateRef stepState'
          poke xsol x'
    -- Error control
    let err = case optionsErrorControl of
          Relative -> [cexp_pure| Nag_ErrorControl{ Nag_Relative } |]
          Absolute -> [cexp_pure| Nag_ErrorControl{ Nag_Absolute } |]
          Mixed -> [cexp_pure| Nag_ErrorControl{ Nag_Mixed } |]
    -- Record the last visited x in an 'IORef' to store it in the
    -- 'Failure' if there was a problem.
    xendRef <- newIORef timesStart
    res <- withNagError $ \fail_ -> do
      xend' <- [c| double {
          double x = $(double timesStart);
          Nag_User comm;
          nag_ode_ivp_bdf_gen(
            $vec-len:y,
            $fun:(void (*fcnIO)(Integer neq, double x, const double y[], double f[], Nag_User *comm)),
            $(void (*jacFunPtr)(Integer neq, double x, const double y[], double pw[], Nag_User *comm)),
            &x, $vec-ptr:(double y[]), $(double timesEnd),
            $(double optionsTolerance), $(Nag_ErrorControl err),
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
  let tol = 10**(-3)
  let t = timesList x 1 10
  let res = solve (Options tol Relative) fcn (Just jac) t y
  print res

-- Utils
------------------------------------------------------------------------

vectorFromC :: Storable a => Nag_Integer -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' $ fromIntegral len

vectorToC :: Storable a => V.Vector a -> Nag_Integer -> Ptr a -> IO ()
vectorToC vec neq ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' $ fromIntegral neq) vec
