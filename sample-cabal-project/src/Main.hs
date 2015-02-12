{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import           Language.C.Inline
import qualified Language.Haskell.TH as TH
import           Language.C.Quote
import           Language.C.Quote.C

emitInclude "math.h"

test_embedCode :: Int
test_embedCode = c_add 1 2
  where
    c_add = $(embedCode $ Code
      TH.Unsafe                   -- Call safety
      [t| Int -> Int -> Int |]    -- Call type
      "francescos_add"            -- Call name
      -- C Code
      [cunit| int francescos_add(int x, int y) { int z = x + y; return z; } |])

test_embedStm :: Double
test_embedStm = $(embedStm
  TH.Unsafe
  [t| Double -> Double |]
  [cty| double |]
  [cparams| double x |]
  [cstm| return cos(x); |]) 1

test_embedExp :: Double
test_embedExp = $(embedExp
  TH.Unsafe
  [t| Double |]
  [cty| double |]
  []
  [cexp| sin(1) |])

emitCode [cunit|
int francescos_mul(int x, int y) {
  return x * y;
}
|]

foreign import ccall "francescos_mul" francescos_mul :: Int -> Int -> Int

main :: IO ()
main = do
  print test_embedCode
  print test_embedStm
  print test_embedExp
  print $ francescos_mul 3 4
