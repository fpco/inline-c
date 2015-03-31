{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- import           Control.Applicative ((<*>))
-- import           Data.Functor ((<$>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Language.C.Inline.Nag

setContext nagCtx

include "<nag.h>"
include "<nage01.h>"

data Monotonic = Monotonic
  { _monotonicXs :: V.Vector CDouble
  , _monotonicYs :: V.Vector CDouble
  , _monotonicDs :: V.Vector CDouble
  }

monotonicInterpolate
  :: V.Vector CDouble -> V.Vector CDouble -> IO (Either String Monotonic)
monotonicInterpolate x f = do
  let n = V.length x
  if V.length f /= n
    then error "monotonicInterpolate: vectors of different lenghts"
    else do
      d <- VM.new n
      withNagError $ \fail_ -> do
        [cexp| void{ nag_monotonic_interpolant(
          $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d), $(NagError *fail_)) } |]
        dImm <- V.unsafeFreeze d
        return $ Monotonic x f dImm

monotonicEvaluate :: Monotonic -> V.Vector CDouble -> IO (Either String (V.Vector CDouble))
monotonicEvaluate (Monotonic x f d) px = do
  let m = V.length px
  pf <- VM.new m
  withNagError $ \fail_ -> do
    [cexp| void{ nag_monotonic_evaluate(
      $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d),
      $vec-len:px, $vec-ptr:(double *px), $vec-ptr:(double *pf),
      $(NagError *fail_)) } |]
    V.unsafeFreeze pf

-- monotonicEvaluate_ :: Monotonic -> CDouble -> IO (Either String CDouble)
-- monotonicEvaluate_ mntnc px = fmap (V.! 0) <$> monotonicEvaluate mntnc (V.fromList [px])

-- monotonicDeriv :: Monotonic -> V.Vector CDouble -> IO (Either String (V.Vector CDouble, V.Vector CDouble))
-- monotonicDeriv (Monotonic x f d) px = do
--   let m = V.length px
--   pf <- VM.new m
--   pd <- VM.new m
--   withNagError $ \fail_ -> do
--     [cexp| void{ nag_monotonic_deriv(
--       $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d),
--       $vec-len:px, $vec-ptr:(double *px), $vec-ptr:(double *pf), $vec-ptr:(double *pd),
--       $(NagError *fail_)) } |]
--     (,) <$> V.unsafeFreeze pf <*> V.unsafeFreeze pd

-- monotonicDeriv_ :: Monotonic -> CDouble -> IO (Either String (CDouble, CDouble))
-- monotonicDeriv_ mntnc px = do
--   fmap (\(pf, pd) -> (pf V.! 0, pd V.! 0)) <$> monotonicDeriv mntnc (V.fromList [px])

-- monotonicIntg :: Monotonic -> (CDouble, CDouble) -> IO (Either String CDouble)
-- monotonicIntg (Monotonic x f d) (a, b) =
--   withNagError $ \fail_ -> withPtr_ $ \integral ->
--     [cexp| void{ nag_monotonic_intg(
--       $vec-len:x, $vec-ptr:(double *x), $vec-ptr:(double *f), $vec-ptr:(double *d),
--       $(double a), $(double b), $(double *integral), $(NagError *fail_)) } |]

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
