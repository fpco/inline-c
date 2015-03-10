{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Language.C.Inline.Nag
import qualified Data.Vector.Storable as V

-- Set the 'Context' to the one provided by "Language.C.Inline.Nag".
-- This gives us access to NAG types such as 'Complex' and 'NagError',
-- and also includes the vector and function pointers anti-quoters.
setContext nagCtx

-- Include the headers files we need.
include "<nag.h>"
include "<nagc06.h>"

-- | Computes the discrete Fourier transform for the given sequence of
-- 'Complex' numbers.  Returns 'Left' if some error occurred, together
-- with the error message.
forwardFFT :: V.Vector Complex -> IO (Either String (V.Vector Complex))
forwardFFT x_orig = do
  -- "Thaw" the input vector -- the input is an immutable vector, and by
  -- "thawing" it we create a mutable copy of it.
  x <- V.thaw x_orig
  -- Use 'withNagError' to easily check whether the NAG operation was
  -- successful.
  withNagError $ \fail_ -> do
    [cexp| void {
       nag_sum_fft_complex_1d(
         // We're computing a forward transform
         Nag_ForwardTransform,
         // We take the pointer underlying 'x' and it's length, using the
         // appropriate anti-quoters
         $vec-ptr:(Complex *x), $vec-len:x,
         // And pass in the NagError structure given to us by
         // 'withNagError'.
         $(NagError *fail_))
      } |]
    -- Turn the mutable vector back to an immutable one using 'V.freeze'
    -- (the inverse of 'V.thaw').
    V.freeze x

-- Run our function with some sample data and print the results.
main :: IO ()
main = do
  let vec = V.fromList
        [ Complex 0.34907 (-0.37168)
        , Complex 0.54890 (-0.35669)
        , Complex 0.74776 (-0.31175)
        , Complex 0.94459 (-0.23702)
        , Complex 1.13850 (-0.13274)
        , Complex 1.32850   0.00074
        , Complex 1.51370   0.16298
        ]
  printVec vec
  Right vec_f <- forwardFFT vec
  printVec vec_f
  where
    printVec vec = do
      V.forM_ vec $ \(Complex re im) -> putStr $ show (re, im) ++ " "
      putStrLn ""
