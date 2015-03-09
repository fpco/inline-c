{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mempty)
import qualified Data.Vector.Storable.Mutable as V
import           Foreign.C.Types
import qualified Language.Haskell.TH as TH
import qualified Test.Hspec as Hspec
import           Text.RawString.QQ (r)
import           Data.Monoid ((<>))

import           Language.C.Inline
import qualified Language.C.Inline.ContextSpec
import qualified Language.C.Inline.ParseSpec
import qualified Language.C.Types as C
import qualified Language.C.Types.ParseSpec

setContext (baseCtx <> funCtx <> vecCtx)

include "<math.h>"
include "<stdio.h>"

emitLiteral [r|
int francescos_mul(int x, int y) {
  return x * y;
}
|]

foreign import ccall "francescos_mul" francescos_mul :: Int -> Int -> Int

main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Language.C.Types.Parse" Language.C.Types.ParseSpec.spec
  Hspec.describe "Language.C.Inline.Context" Language.C.Inline.ContextSpec.spec
  Hspec.describe "Language.C.Inline.Parse" Language.C.Inline.ParseSpec.spec
  Hspec.describe "TH integration" $ do
    Hspec.it "inlineCode" $ do
      let c_add = $(inlineCode $ Code
            TH.Unsafe                   -- Call safety
            [t| Int -> Int -> Int |]    -- Call type
            "francescos_add"            -- Call name
            -- C Code
            [r| int francescos_add(int x, int y) { int z = x + y; return z; } |])
      c_add 3 4 `Hspec.shouldBe` 7
    Hspec.it "inlineItems" $ do
      let c_add3 = $(inlineItems
            TH.Unsafe
            [t| CInt -> CInt |]
            (C.TypeSpecifier mempty (C.Int C.Signed))
            [("x", C.TypeSpecifier mempty (C.Int C.Signed))]
            [r| return x + 3; |])
      c_add3 1 `Hspec.shouldBe` 1 + 3
    Hspec.it "inlineExp" $ do
      let x = $(inlineExp
            TH.Safe
            [t| CInt |]
            (C.TypeSpecifier mempty (C.Int C.Signed))
            []
            [r| 1 + 4 |])
      x `Hspec.shouldBe` 1 + 4
    Hspec.it "inlineCode" $ do
      francescos_mul 3 4 `Hspec.shouldBe` 12
    Hspec.it "cexp" $ do
      let x = 3
      let y = 4
      z <- [cexp| int(int x, int y){ x + y + 5 } |]
      z `Hspec.shouldBe` x + y + 5
    Hspec.it "cexp_unsafe" $ do
      let x = 2
      let y = 10
      z <- [cexp_unsafe| int(int x, int y){ 7 + x + y } |]
      z `Hspec.shouldBe` x + y + 7
    Hspec.it "cexp_pure" $ do
      let x = 2
      let y = 10
      let z = [cexp_pure| int(int x, int y){ x + 10 + y } |]
      z `Hspec.shouldBe` x + y + 10
    Hspec.it "cexp_pure_unsafe" $ do
      let x = 2
      let y = 10
      let z = [cexp_pure_unsafe| int(int x, int y){ x * 2 + y } |]
      z `Hspec.shouldBe` x * 2 + y
    Hspec.it "suffix type" $ do
      let x = 3
      let y = 4
      [cexp_pure| int { $(int x) + $(int y) } |] `Hspec.shouldBe` 7
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
      let z = [cexp_pure| int { $(int (*ackermannPtr)(int, int))($(int x), $(int y)) } |]
      z `Hspec.shouldBe` ackermann x y
    Hspec.it "function pointer result" $ do
      c_add <- [cexp| int (*)(int, int) { &francescos_add } |]
      x <- $(peekFunPtr [t| CInt -> CInt -> IO CInt |]) c_add 1 2
      x `Hspec.shouldBe` 1 + 2
    Hspec.it "quick function pointer argument" $ do
      let ackermann m n
            | m == 0 = n + 1
            | m > 0 && n == 0 = ackermann (m - 1) 1
            | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))
            | otherwise = error "ackermann"
      let ackermann_ m n = return $ ackermann m n
      let x = 3
      let y = 4
      z <- [cexp| int { $fun:(int (*ackermann_)(int, int))($(int x), $(int y)) } |]
      z `Hspec.shouldBe` ackermann x y
    Hspec.it "vectors" $ do
      let n = 10
      vec <- V.replicate (fromIntegral n) 3
      sum <- V.unsafeWith vec $ \ptr -> [citems| int {
        int i;
        int x = 0;
        for (i = 0; i < $(int n); i++) {
          x += $(int *ptr)[i];
        }
        return x;
      } |]
      sum `Hspec.shouldBe` 3 * 10
    Hspec.it "quick vectors" $ do
      vec <- V.replicate 10 3
      sum <- [citems| int {
        int i;
        int x = 0;
        for (i = 0; i < $vec-len:vec; i++) {
          x += $vec-ptr:(int *vec)[i];
        }
        return x;
      } |]
      sum `Hspec.shouldBe` 3 * 10
