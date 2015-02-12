{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Language.C.Inline
import qualified Language.Haskell.TH as TH
import           Language.C.Quote
import           Language.C.Quote.C

test_embedCode :: Int
test_embedCode = c_add 1 2
  where
    c_add = $(embedCode $ Code
      TH.Unsafe                   -- Call safety
      [t| Int -> Int -> Int |]    -- Call type
      "add"                       -- Call name
      -- C Code
      [cunit| int add(int x, int y) { int z = x + y; return z; } |])

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

main :: IO ()
main = do
  print test_embedCode
  print test_embedStm
  print test_embedExp
