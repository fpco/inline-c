{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import           Language.C.Inline

include "<math.h>"
include "<stdio.h>"

test_cexp :: CDouble -> CDouble -> IO CDouble
test_cexp x y =
  [cexp| double(double x, double y){ cos(x) + cos(y) } |]

test_cexp_unsafe :: CDouble -> CDouble -> IO CDouble
test_cexp_unsafe x y =
  [cexp_unsafe| double(double x, double y){ cos(x) + cos(y) } |]

test_voidExp :: IO ()
test_voidExp = [cexp| void { printf("Hello\n") } |]

main :: IO ()
main = do
  print =<< test_cexp 3 4
  print =<< test_cexp_unsafe 3 4
  test_voidExp
