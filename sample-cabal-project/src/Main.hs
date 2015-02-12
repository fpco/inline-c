{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import           Language.C.Inline
import qualified Language.Haskell.TH as TH
import           Language.C.Quote
import           Language.C.Quote.C

main :: IO ()
main = do
  let c_add = $(embedCCode $ CCode
        TH.Unsafe                   -- Call safety
        [t| Int -> Int -> Int |]    -- Call type
        "add"                       -- Call name
        -- C Code
        [cunit| int add(int x, int y) { int z = x + y; return z; } |])
  print $ c_add 1 2

