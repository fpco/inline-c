{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import           Language.C.Inline
import qualified Language.Haskell.TH as TH
import qualified Language.C.Quote as C
import qualified Language.C.Quote.C as C
import           Foreign.C.Types

emitInclude "math.h"

test_embedCode :: Int
test_embedCode = c_add 1 2
  where
    c_add = $(embedCode $ Code
      TH.Unsafe                   -- Call safety
      [t| Int -> Int -> Int |]    -- Call type
      "francescos_add"            -- Call name
      -- C Code
      [C.cunit| int francescos_add(int x, int y) { int z = x + y; return z; } |])

test_embedStm :: Double
test_embedStm = $(embedStm
  TH.Unsafe
  [t| Double -> Double |]
  [C.cty| double |]
  [C.cparams| double x |]
  [C.cstm| return cos(x); |]) 1

test_embedExp :: Double
test_embedExp = $(embedExp
  TH.Unsafe
  [t| Double |]
  [C.cty| double |]
  []
  [C.cexp| sin(1) |])

emitCode [C.cunit|
int francescos_mul(int x, int y) {
  return x * y;
}
|]

foreign import ccall "francescos_mul" francescos_mul :: Int -> Int -> Int

test_cexp :: CDouble -> CDouble -> IO CDouble
test_cexp x y =
  [cexp| double(double x, double y){ cos(x) + cos(y) } |]

test_cexp_unsafe :: CDouble -> CDouble -> IO CDouble
test_cexp_unsafe x y =
  [cexp_unsafe| double(double x, double y){ cos(x) + cos(y) } |]

test_cexp_pure :: CDouble -> CDouble
test_cexp_pure x =
  [cexp_pure| double(double x){ cos(x) + sin(x) } |]

test_cexp_pure_unsafe :: CDouble -> CDouble
test_cexp_pure_unsafe x =
  [cexp_pure_unsafe| double(double x){ cos(x) + sin(x) } |]

main :: IO ()
main = do
  print test_embedCode
  print test_embedStm
  print test_embedExp
  print $ francescos_mul 3 4
  print =<< test_cexp 3 4
  print =<< test_cexp_unsafe 3 4
  print $ test_cexp_pure 4
  print $ test_cexp_pure_unsafe 4
