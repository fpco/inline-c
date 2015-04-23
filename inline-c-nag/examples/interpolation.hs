{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.Coerce (coerce)
import           Data.Functor ((<$>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Graphics.Rendering.Chart.Backend.Cairo (toFile)
import           Graphics.Rendering.Chart.Easy (layout_title, (.=), plot, line, points, def)
import qualified Language.C.Inline.Nag as C
import           System.Environment (getArgs)

C.context C.nagCtx

C.include "<nag.h>"
C.include "<nage01.h>"

data Monotonic = Monotonic
  { _monotonicXs :: V.Vector CDouble
  , _monotonicYs :: V.Vector CDouble
  , _monotonicDs :: V.Vector CDouble
  }

monotonicInterpolate
  :: V.Vector Double -> V.Vector Double -> IO (Either String Monotonic)
monotonicInterpolate (coerce -> x) (coerce -> f) = do
  let n = V.length x
  if V.length f /= n
    then error "monotonicInterpolate: vectors of different lenghts"
    else do
      d <- VM.new n
      C.withNagError $ \fail_ -> do
        [C.exp| void{ nag_monotonic_interpolant(
          $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d), $(NagError *fail_)) } |]
        dImm <- V.unsafeFreeze d
        return $ Monotonic x f dImm

monotonicEvaluate :: Monotonic -> V.Vector Double -> IO (Either String (V.Vector Double))
monotonicEvaluate (Monotonic x f d) (coerce -> px) = do
  let m = V.length px
  pf <- VM.new m
  C.withNagError $ \fail_ -> do
    [C.exp| void{ nag_monotonic_evaluate(
      $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d),
      $vec-len:px, $vec-ptr:(double *px), $vec-ptr:(double *pf),
      $(NagError *fail_)) } |]
    coerce <$> V.unsafeFreeze pf

{-
monotonicEvaluate_ :: Monotonic -> Double -> IO (Either String Double)
monotonicEvaluate_ mntnc px = fmap (V.! 0) <$> monotonicEvaluate mntnc (V.fromList [px])

monotonicDeriv :: Monotonic -> V.Vector Double -> IO (Either String (V.Vector Double, V.Vector Double))
monotonicDeriv (Monotonic x f d) (coerce -> px) = do
  let m = V.length px
  pf <- VM.new m
  pd <- VM.new m
  withNagError $ \fail_ -> do
    [cexp| void{ nag_monotonic_deriv(
      $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d),
      $vec-len:px, $vec-ptr:(double *px), $vec-ptr:(double *pf), $vec-ptr:(double *pd),
      $(NagError *fail_)) } |]
    coerce <$> ((,) <$> V.unsafeFreeze pf <*> V.unsafeFreeze pd)

monotonicDeriv_ :: Monotonic -> Double -> IO (Either String (Double, Double))
monotonicDeriv_ mntnc px = do
  fmap (\(pf, pd) -> (pf V.! 0, pd V.! 0)) <$> monotonicDeriv mntnc (V.fromList [px])

monotonicIntg :: Monotonic -> (Double, Double) -> IO (Either String Double)
monotonicIntg (Monotonic x f d) (coerce -> (a, b)) =
  fmap coerce $ withNagError $ \fail_ -> withPtr_ $ \integral ->
    [cexp| void{ nag_monotonic_intg(
      $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d),
      $(double a), $(double b), $(double *integral), $(NagError *fail_)) } |]
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      let (xs, ys) = unzip pts
      mntnc <- assertNag $ monotonicInterpolate (V.fromList xs) (V.fromList ys)
      let lineXs = filter (< 20) [7.99,8..20] ++ [20]
      lineYs <- V.toList <$> assertNag (monotonicEvaluate mntnc (V.fromList lineXs))
      toFile def fn $ do
        layout_title .= "Interpolation test"
        plot (line "interpolation" [zip lineXs lineYs])
        plot (points "points" pts)
    _ -> do
      error "usage: interpolation FILE"
  where
    assertNag m = do
      x <- m
      case x of
        Left err -> error err
        Right y -> return y

    pts =
      [ ( 7.99, 0.00000E+0)
      , ( 8.09, 0.27643E-4)
      , ( 8.19, 0.43750E-1)
      , ( 8.70, 0.16918E+0)
      , ( 9.20, 0.46943E+0)
      , (10.00, 0.94374E+0)
      , (12.00, 0.99864E+0)
      , (15.00, 0.99992E+0)
      , (20.00, 0.99999E+0)
      ]

{-
main :: IO ()
main = do
  Right mntnc <- monotonicInterpolate x f
  Right pf <- monotonicEvaluate mntnc px
  print pf
  where
    n = 9

    x = V.fromList
          [  7.99
          ,  8.09
          ,  8.19
          ,  8.70
          ,  9.20
          , 10.00
          , 12.00
          , 15.00
          , 20.00
          ]

    f = V.fromList
          [ 0.00000E+0
          , 0.27643E-4
          , 0.43750E-1
          , 0.16918E+0
          , 0.46943E+0
          , 0.94374E+0
          , 0.99864E+0
          , 0.99992E+0
          , 0.99999E+0
          ]

    m = 11

    first = x V.! 0
    last = x V.! (n - 1)

    step = (last - first) / (m - 1);

    px = V.fromList $ [first,(first+step)..(last-1)] ++ [last]
-}
