{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module TestModule (testModule) where

import           Foreign.C.Types
import qualified Language.C.Inline as C

testModule :: IO CDouble
testModule = [C.exp| double{ 0 } |]
