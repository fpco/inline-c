{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import           Foreign.C.Types
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

import           TestModule
import           TestModule.Nested

C.include "<math.h>"
C.include "<stdio.h>"

test_cexp :: CDouble -> CDouble -> IO CDouble
test_cexp x y =
  [C.exp| double{ cos($(double x)) + cos($(double y)) } |]

test_cexp_unsafe :: CDouble -> CDouble -> IO CDouble
test_cexp_unsafe x y =
  [CU.exp| double{ cos($(double x)) + cos($(double y)) } |]

test_voidExp :: IO ()
test_voidExp = [C.exp| void { printf("Hello\n") } |]

main :: IO ()
main = do
  print =<< test_cexp 3 4
  print =<< test_cexp_unsafe 3 4
  test_voidExp
  print =<< testModule
  print =<< testModuleNested

