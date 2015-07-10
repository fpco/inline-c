{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module TestModule.Nested (testModuleNested) where

import           Foreign.C.Types
import qualified Language.C.Inline as C

testModuleNested :: IO CDouble
testModuleNested = [C.exp| double{ 1 } |]
