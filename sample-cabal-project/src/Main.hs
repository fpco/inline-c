{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import           Foreign.C.Types
import           Language.C.Inline
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import           Text.RawString.QQ (r)
import           Data.Monoid (mempty)

include "<math.h>"
include "<stdio.h>"

test_inlineCode :: Int
test_inlineCode = c_add 1 2
  where
    c_add = $(inlineCode $ Code
      TH.Unsafe                   -- Call safety
      [t| Int -> Int -> Int |]    -- Call type
      "francescos_add"            -- Call name
      -- C Code
      [r| int francescos_add(int x, int y) { int z = x + y; return z; } |])

test_inlineItems :: Double
test_inlineItems = $(inlineItems
  TH.Unsafe
  [t| Double -> Double |]
  (C.TypeSpecifier mempty C.Double)
  [("x", C.TypeSpecifier mempty C.Double)]
  [r| return cos(x); |]) 1

test_inlineExp :: Double
test_inlineExp = $(inlineExp
  TH.Unsafe
  [t| Double |]
  (C.TypeSpecifier mempty C.Double)
  []
  [r| sin(1) |])

emitLiteral [r|
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

test_suffixType1 :: CInt -> CInt -> CInt
test_suffixType1 x y = [cexp_pure| int{ $(int x) + $(int x) } |]

test_suffixType2 :: CInt -> CInt -> CInt
test_suffixType2 x y = [cexp_pure| int(){ $(int x) + $(int y) } |]

test_suffixType3 :: CInt -> CInt -> CInt
test_suffixType3 x y = [cexp_pure| int(int x){ x + $(int y) } |]

test_suffixType4 :: CInt -> CInt -> CInt
test_suffixType4 x y = [cexp_pure| int(int x){ $(int x) + $(int y) } |]

test_voidExp :: IO ()
test_voidExp = [cexp| void { printf("Hello\n") } |]

main :: IO ()
main = do
  print test_inlineCode
  print test_inlineItems
  print test_inlineExp
  print $ francescos_mul 3 4
  print =<< test_cexp 3 4
  print =<< test_cexp_unsafe 3 4
  print $ test_cexp_pure 4
  print $ test_cexp_pure_unsafe 4
  print $ test_suffixType1 1 2
  print $ test_suffixType2 1 2
  print $ test_suffixType3 1 2
  print $ test_suffixType4 1 2
  test_voidExp
