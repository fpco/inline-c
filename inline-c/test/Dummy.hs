module Dummy (dummyFun) where

import           Foreign.C.Types

dummyFun :: CDouble -> CDouble
dummyFun = cos
