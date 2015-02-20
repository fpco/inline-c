{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Language.C.Quote.C as C
import           Language.C.Inline
import qualified Language.Haskell.TH as TH
import           Foreign.C.Types
import qualified Test.Hspec as Hspec

emitInclude "<math.h>"
emitInclude "<stdio.h>"

emitCode [C.cunit|
int francescos_mul(int x, int y) {
  return x * y;
}
|]

foreign import ccall "francescos_mul" francescos_mul :: Int -> Int -> Int

main :: IO ()
main = Hspec.hspec $ do
  tests
  Hspec.describe "TH" $ do
    Hspec.it "embedCode" $ do
      let c_add = $(embedCode $ Code
            TH.Unsafe                   -- Call safety
            [t| Int -> Int -> Int |]    -- Call type
            "francescos_add"            -- Call name
            -- C Code
            [C.cunit| int francescos_add(int x, int y) { int z = x + y; return z; } |])
      c_add 3 4 `Hspec.shouldBe` 7
    Hspec.it "embedItems" $ do
       let c_cos = $(embedItems
             TH.Unsafe
             [t| Double -> Double |]
             [C.cty| double |]
             [C.cparams| double x |]
             [C.citems| return cos(x); |])
       c_cos 1 `Hspec.shouldBe` cos 1
    Hspec.it "embedExp" $ do
      let x = $(embedExp
            TH.Safe
            [t| Double |]
            [C.cty| double |]
            []
            [C.cexp| sin(1) |])
      x `Hspec.shouldBe` sin 1
    Hspec.it "embedCode" $ do
      francescos_mul 3 4 `Hspec.shouldBe` 12
    Hspec.it "cexp" $ do
      let x = 3
      let y = 4
      z <- [cexp| double(double x, double y){ cos(x) + cos(y) } |]
      z `Hspec.shouldBe` cos x + cos y
    Hspec.it "cexp_unsafe" $ do
      let x = 2
      let y = 10
      z <- [cexp_unsafe| double(double x, double y){ cos(x) + cos(y) } |]
      z `Hspec.shouldBe` cos x + cos y
    Hspec.it "cexp_pure" $ do
      let x = 2
      let y = 10
      let z = [cexp_pure| double(double x, double y){ cos(x) + cos(y) } |]
      z `Hspec.shouldBe` cos x + cos y
    Hspec.it "cexp_pure_unsafe" $ do
      let x = 2
      let y = 10
      let z = [cexp_pure_unsafe| double(double x, double y){ cos(x) + cos(y) } |]
      z `Hspec.shouldBe` cos x + cos y
    Hspec.it "suffix type" $ do
      let x = 3
      let y = 4
      [cexp_pure| int { x_int + y_int } |] `Hspec.shouldBe` 7
    Hspec.it "void exp" $ do
      [cexp| void { printf("Hello\n") } |]
    Hspec.it "function pointer argument" $ do
      let ackermann m n
            | m == 0 = n + 1
            | m > 0 && n == 0 = ackermann (m - 1) 1
            | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))
            | otherwise = error "ackermann"
      ackermannPtr <- $(mkFunPtr [t| CInt -> CInt -> CInt |]) ackermann
      let x = 3
      let y = 4
      let z = [cexp_pure| int(int (*ackermannPtr)(int, int)) { ackermannPtr(x_int, y_int) } |]
      z `Hspec.shouldBe` ackermann x y
    Hspec.it "function pointer result" $ do
      c_cos <- [cexp| double (*)(double) { &cos } |]
      x <- $(peekFunPtr [t| CDouble -> IO CDouble |]) c_cos 1
      x `Hspec.shouldBe` cos 1
