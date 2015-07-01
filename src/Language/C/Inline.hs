{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Enable painless embedding of C code in Haskell code. If you're interested
-- in how to use the library, skip to the "Inline C" section. To build, read the
-- first two sections.
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified "Language.C.Inline" as C
-- @

module Language.C.Inline
  ( -- * Build process
    -- $building

    -- * Contexts
    Context
  , baseCtx
  , funCtx
  , vecCtx
  , bsCtx
  , context

    -- * Inline C
    -- $quoting
  , exp
  , pure
  , block
  , include
  , verbatim

    -- * 'Ptr' utils
  , withPtr
  , withPtr_
  , WithPtrs(..)

    -- * 'FunPtr' utils
    --
    -- Functions to quickly convert from/to 'FunPtr's. They're provided here
    -- since they can be useful to work with Haskell functions in C, and
    -- vice-versa. However, consider using 'funCtx' if you're doing this
    -- a lot.
  , mkFunPtr
  , mkFunPtrFromName
  , peekFunPtr

    -- * C types re-exports
    --
    -- Re-export these to avoid errors when `inline-c` generates FFI calls GHC
    -- needs the constructors for those types.
  , module Foreign.C.Types
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Prelude hiding (exp)
#else
import           Prelude hiding (exp, pure)
#endif

import           Control.Monad (void)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peek, Storable)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

import           Language.C.Inline.Context
import           Language.C.Inline.Internal
import           Language.C.Inline.FunPtr

-- $building
--
-- Each module that uses at least one of the TH functions in this module gets
-- a C file associated to it, where the filename of said file will be the same
-- as the module but with a `.c` extension. This C file must be built after the
-- Haskell code and linked appropriately. If you use cabal, all you have to do
-- is declare each associated C file in the @.cabal@ file.
--
-- For example:
--
-- @
-- executable foo
--   main-is:             Main.hs, Foo.hs, Bar.hs
--   hs-source-dirs:      src
--   -- Here the corresponding C sources must be listed for every module
--   -- that uses C code.  In this example, Main.hs and Bar.hs do, but
--   -- Foo.hs does not.
--   c-sources:           src\/Main.c, src\/Bar.c
--   -- These flags will be passed to the C compiler
--   cc-options:          -Wall -O2
--   -- Libraries to link the code with.
--   extra-libraries:     -lm
--   ...
-- @
--
-- Note that currently @cabal repl@ is not supported, because the C code is not
-- compiled and linked appropriately.
--
-- If we were to compile the above manually, we could:
--
-- @
-- $ ghc -c Main.hs
-- $ cc -c Main.c -o Main_c.o
-- $ ghc Foo.hs
-- $ ghc Bar.hs
-- $ cc -c Bar.c -o Bar_c.o
-- $ ghc Main.o Foo.o Bar.o Main_c.o Bar_c.o -lm -o Main
-- @

------------------------------------------------------------------------
-- Quoting sugar

-- $quoting
--
-- The quasiquoters below are the main interface to this library, for inlining
-- C code into Haskell source files.
--
-- In general, quasiquoters are used like so:
--
-- @
-- [C.XXX| int { \<C code\> } |]
-- @
--
-- Where @C.XXX@ is one of the quasi-quoters defined in this section.
--
-- This syntax stands for a piece of typed C, decorated with a type:
--
-- * The first type to appear (@int@ in the example) is the type of said C code.
--
-- * The syntax of the @\<C code\>@ depends on on the quasi-quoter used, and the
--   anti-quoters available. The @exp@ quasi-quoter expects a C expression. The
--   @block@ quasi-quoter expects a list of statements, like the body of
--   a function. Just like a C function, a block has a return type, matching the
--   type of any values in any @return@ statements appearing in the block.
--
-- See also the @README.md@ file for more documentation.
--
-- === Anti-quoters
--
-- Haskell variables can be captured using anti-quoters.  @inline-c@
-- provides a basic anti-quoting mechanism extensible with user-defined
-- anti-quoters (see "Language.C.Inline.Context").  The basic
-- anti-quoter lets you capture Haskell variables, for
-- example we might say
--
-- @
-- let x = pi / 3 in ['C.exp'| double { cos($(double x)) } |]
-- @
--
-- Which would capture the Haskell variable @x@ of type @'CDouble'@.
--
-- In C expressions the @$@ character is denoted using @$$@.
--
-- === Variable capture and the typing relation
--
-- The Haskell type of the inlined expression is determined by the specified
-- C return type. The relation between the C type and the Haskell type is
-- defined in the current 'Context' -- see 'convertCType'. C pointers and
-- arrays are both converted to Haskell @'Ptr'@s, and function pointers are
-- converted to @'FunPtr'@s. Sized arrays are not supported.
--
-- Similarly, when capturing Haskell variables using anti-quoting, their
-- type is assumed to be of the Haskell type corresponding to the C type
-- provided.  For example, if we capture variable @x@ using @double x@
-- in the parameter list, the code will expect a variable @x@ of type
-- 'CDouble' in Haskell (when using 'baseCtx').
--
-- === Purity
--
-- The 'exp' and 'block' quasi-quotes denote computations in the 'IO' monad.
-- 'pure' denotes a pure value, expressed as a C expression.
--
-- === Safe and @unsafe@ calls
--
-- @unsafe@ variants of the quasi-quoters are provided in
-- "Language.C.Inline.Unsafe" to call the C code unsafely, in the sense that the
-- C code will block the RTS, but with the advantage of a faster call to the
-- foreign code. See
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1590008.4.3>.
--
-- == Examples
--
-- === Inline C expression
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import qualified "Language.C.Inline" as C
-- import qualified "Language.C.Inline.Unsafe" as CU
-- import           "Foreign.C.Types"
--
-- C.'include' "\<math.h\>"
--
-- c_cos :: 'CDouble' -> IO 'CDouble'
-- c_cos x = [C.exp| double { cos($(double x)) } |]
--
-- faster_c_cos :: 'CDouble' -> IO 'CDouble'
-- faster_c_cos x = [CU.exp| double { cos($(double x)) } |]
-- @
--
-- === Inline C statements
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import qualified Data.Vector.Storable.Mutable as V
-- import qualified "Language.C.Inline" as C
-- import           "Foreign.C.Types"
--
-- C.'include' "\<stdio.h\>"
--
-- parseVector :: 'CInt' -> 'IO' (V.IOVector 'CDouble')
-- parseVector len = do
--   vec <- V.new $ 'fromIntegral' len0
--   V.unsafeWith vec $ \\ptr -> [C.'block'| void {
--     int i;
--     for (i = 0; i < $(int len); i++) {
--       scanf("%lf ", &$(double *ptr)[i]);
--     }
--   } |]
--   'return' vec
-- @

-- | C expressions.
exp :: TH.QuasiQuoter
exp = genericQuote IO $ inlineExp TH.Safe

-- | Variant of 'exp', for use with expressions known to have no side effects.
--
-- BEWARE: use this function with caution, only when you know what you are
-- doing. If an expression does in fact have side-effects, then indiscriminate
-- use of 'pure' may endanger referential transparency, and in principle even
-- type safety.
pure :: TH.QuasiQuoter
pure = genericQuote Pure $ inlineExp TH.Safe

-- | C code blocks (i.e. statements).
block :: TH.QuasiQuoter
block = genericQuote IO $ inlineItems TH.Safe

-- | Emits a CPP include directive for C code associated with the current
-- module. To avoid having to escape quotes, the function itself adds them when
-- appropriate, so that
--
-- @
-- include "foo.h" ==> #include "foo.h"
-- @
--
-- but
--
-- @
-- include "\<foo\>" ==> #include \<foo\>
-- @
include :: String -> TH.DecsQ
include s
  | null s = fail "inline-c: empty string (include)"
  | head s == '<' = verbatim $ "#include " ++ s
  | otherwise = verbatim $ "#include \"" ++ s ++ "\""

-- | Emits an arbitrary C string to the C code associated with the
-- current module.  Use with care.
verbatim :: String -> TH.DecsQ
verbatim s = do
  void $ emitVerbatim s
  return []

------------------------------------------------------------------------
-- 'Ptr' utils

-- | Like 'alloca', but also peeks the contents of the 'Ptr' and returns
-- them once the provided action has finished.
withPtr :: (Storable a) => (Ptr a -> IO b) -> IO (a, b)
withPtr f = do
  alloca $ \ptr -> do
    x <- f ptr
    y <- peek ptr
    return (y, x)

withPtr_ :: (Storable a) => (Ptr a -> IO ()) -> IO a
withPtr_ f = do
  (x, ()) <- withPtr f
  return x

-- | Type class with methods useful to allocate and peek multiple
-- pointers at once:
--
-- @
-- withPtrs_ :: (Storable a, Storable b) => ((Ptr a, Ptr b) -> IO ()) -> IO (a, b)
-- withPtrs_ :: (Storable a, Storable b, Storable c) => ((Ptr a, Ptr b, Ptr c) -> IO ()) -> IO (a, b, c)
-- ...
-- @
class WithPtrs a where
  type WithPtrsPtrs a :: *
  withPtrs :: (WithPtrsPtrs a -> IO b) -> IO (a, b)

  withPtrs_ :: (WithPtrsPtrs a -> IO ()) -> IO a
  withPtrs_ f = do
    (x, _) <- withPtrs f
    return x

instance (Storable a, Storable b) => WithPtrs (a, b) where
  type WithPtrsPtrs (a, b) = (Ptr a, Ptr b)
  withPtrs f = do
    (a, (b, x)) <- withPtr $ \a -> withPtr $ \b -> f (a, b)
    return ((a, b), x)

instance (Storable a, Storable b, Storable c) => WithPtrs (a, b, c) where
  type WithPtrsPtrs (a, b, c) = (Ptr a, Ptr b, Ptr c)
  withPtrs f = do
    (a, ((b, c), x)) <- withPtr $ \a -> withPtrs $ \(b, c) -> f (a, b, c)
    return ((a, b, c), x)

instance (Storable a, Storable b, Storable c, Storable d) => WithPtrs (a, b, c, d) where
  type WithPtrsPtrs (a, b, c, d) = (Ptr a, Ptr b, Ptr c, Ptr d)
  withPtrs f = do
    (a, ((b, c, d), x)) <- withPtr $ \a -> withPtrs $ \(b, c, d) -> f (a, b, c, d)
    return ((a, b, c, d), x)

instance (Storable a, Storable b, Storable c, Storable d, Storable e) => WithPtrs (a, b, c, d, e) where
  type WithPtrsPtrs (a, b, c, d, e) = (Ptr a, Ptr b, Ptr c, Ptr d, Ptr e)
  withPtrs f = do
    (a, ((b, c, d, e), x)) <- withPtr $ \a -> withPtrs $ \(b, c, d, e) -> f (a, b, c, d, e)
    return ((a, b, c, d, e), x)

instance (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f) => WithPtrs (a, b, c, d, e, f) where
  type WithPtrsPtrs (a, b, c, d, e, f) = (Ptr a, Ptr b, Ptr c, Ptr d, Ptr e, Ptr f)
  withPtrs fun = do
    (a, ((b, c, d, e, f), x)) <- withPtr $ \a -> withPtrs $ \(b, c, d, e, f) -> fun (a, b, c, d, e, f)
    return ((a, b, c, d, e, f), x)

instance (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f, Storable g) => WithPtrs (a, b, c, d, e, f, g) where
  type WithPtrsPtrs (a, b, c, d, e, f, g) = (Ptr a, Ptr b, Ptr c, Ptr d, Ptr e, Ptr f, Ptr g)
  withPtrs fun = do
    (a, ((b, c, d, e, f, g), x)) <- withPtr $ \a -> withPtrs $ \(b, c, d, e, f, g) -> fun (a, b, c, d, e, f, g)
    return ((a, b, c, d, e, f, g), x)

------------------------------------------------------------------------
-- setContext alias

-- | Sets the 'Context' for the current module.  This function, if
-- called, must be called before any of the other TH functions in this
-- module.  Fails if that's not the case.
context :: Context -> TH.DecsQ
context ctx = do
  setContext ctx
  return []
