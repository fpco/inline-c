-- | This module exists because of TH staging restrictions.
module Dummy (dummyFun) where

import           Foreign.C.Types

dummyFun :: CDouble -> IO CDouble
dummyFun x = return $ cos x
