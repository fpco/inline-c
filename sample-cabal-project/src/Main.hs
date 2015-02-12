{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Language.C.Inline
import qualified Language.Haskell.TH as TH
import           Language.C.Quote
import           Language.C.Quote.C

test_embedCCode :: Int
test_embedCCode = c_add 1 2
  where
    c_add = $(embedCCode $ CCode
      TH.Unsafe                   -- Call safety
      [t| Int -> Int -> Int |]    -- Call type
      "add"                       -- Call name
      -- C Code
      [cunit| int add(int x, int y) { int z = x + y; return z; } |])

test_embedCStm :: Double
test_embedCStm = $(embedCStm
  TH.Unsafe
  [t| Double -> Double |]
  [cty| double |]
  [cparams| double x |]
  [cstm| return cos(x); |]) 1

test_embedCExp :: Double
test_embedCExp = $(embedCExp
  TH.Unsafe
  [t| Double |]
  [cty| double |]
  []
  [cexp| sin(1) |])

main :: IO ()
main = do
  print test_embedCCode
  print test_embedCStm
  print test_embedCExp
