{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TemplateSpec where

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Context as CC
import qualified Language.C.Types as CT
import           Foreign
import           Foreign.C
import           Data.Monoid

data CppVector a

C.context $
  C.cppCtx
  <>
    C.cppTypePairs [
        ("std::vector" :: CT.CIdentifier, [t|CppVector|])
     ]

C.include "<cstdlib>"
C.include "<vector>"

-- compiles: we can return std::vector<int>*
returns_vec_of_int = do
  [C.block| std::vector<int>* {
                return ( (std::vector<int>*) NULL);
            }
  |] :: IO (Ptr (CppVector CInt))

-- compiles: we can return std::vector<long int>*
returns_vec_of_long_int = do
  [C.block| std::vector<long int>* {
                return ( (std::vector<long int>*) NULL);
            }
  |] :: IO (Ptr (CppVector CLong))

-- compiles: we can return std::vector<short>*
returns_vec_of_short = do
  [C.block| std::vector<short>* {
                return ( (std::vector<short>*) NULL);
            }
  |] :: IO (Ptr (CppVector CShort))

-- compiles: we can return std::vector<short int>*
returns_vec_of_short_int = do
  [C.block| std::vector<short int>* {
                return ( (std::vector<short int>*) NULL);
            }
  |] :: IO (Ptr (CppVector CShort))

-- compiles: we can return std::vector<unsigned int>*
returns_vec_of_unsigned_int = do
  [C.block| std::vector<unsigned int>* {
                return ( (std::vector<unsigned int>*) NULL);
            }
  |] :: IO (Ptr (CppVector CUInt))

-- compiles: we can return long*
returns_ptr_to_long = do
  [C.block| long* {
                return ( (long*) NULL);
            }
  |] :: IO (Ptr CLong)

-- compiles: we can return unsigned long*
returns_ptr_to_unsigned_long = do
  [C.block| unsigned long* {
                return ( (unsigned long*) NULL);
            }
  |] :: IO (Ptr CULong)

-- compiles: we can return std::vector<long>*
returns_vec_of_long = do
  [C.block| std::vector<long>* {
                return ( (std::vector<long>*) NULL);
            }
  |] :: IO (Ptr (CppVector CLong))

-- compiles: we can return std::vector<long long>*
returns_vec_of_long_long = do
  [C.block| std::vector<long long>* {
                return ( (std::vector<long long>*) NULL);
            }
  |] :: IO (Ptr (CppVector CLLong))

