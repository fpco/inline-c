{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Language.C.Inline
import qualified Language.Haskell.TH as TH
import           Language.C.Quote
import           Language.C.Quote.C

main :: IO ()
main = do
  print $ $(embedCFunction $ CFunction
    TH.Unsafe
    [t| Int -> Int -> Int |]
    ([cty| int |], [cparams| int x, int y |])
    [cstms| { int z = x + y; return z; } |]) 1 2
