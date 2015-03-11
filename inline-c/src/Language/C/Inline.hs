{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The main goal of this module is to allow painless embedding of C
-- code in Haskell code.  If you're interested in how to use the
-- library, skip to the "Inline C" section.  To build, read the first
-- two sections.
module Language.C.Inline
    ( -- * Build process
      -- $building

      -- * Contexts
      -- $context
      Context
    , baseCtx
    , funCtx
    , vecCtx
    , setContext

      -- * Inline C
      -- $quoting
    , cexp
    , cexp_unsafe
    , cexp_pure
    , cexp_pure_unsafe
    , c
    , c_unsafe
    , c_pure
    , c_pure_unsafe
    , include

      -- * 'FunPtr' utils
      --
      -- | Functions to quickly convert from/to 'FunPtr's.  They're provided
      -- here since they can be useful to work with Haskell functions
      -- in C, and vice-versa.  However, consider using 'funCtx' if you're
      -- doing this a lot.
    , mkFunPtr
    , peekFunPtr

      -- * Re-exports
    , module Foreign.C.Types
    , Ptr
    , FunPtr
    ) where

import           Control.Monad (forM)
import qualified Data.Map as Map
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.C.Types as C
import           Language.C.Inline.Context
import           Language.C.Inline.Internal
import           Language.C.Inline.FunPtr

------------------------------------------------------------------------
-- Module compile-time state

-- $building
--
-- Each module that uses at least one of the TH functions in this module
-- gets a C file associated to it, where the filename of said file will
-- be the same as the module but with a C extension.  This C file must
-- be built after the Haskell code and linked appropriately.  If you use
-- cabal, all you have to do is declare each associated C file in the
-- @.cabal@ file and you are good.
--
-- For example we might have
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
-- Note that currently @cabal repl@ is not supported, because the C code
-- is not compiled and linked appropriately.
--
-- If we were to compile the above manaully we could do
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
-- The functions below are the main interface to this library, and let
-- you easily inline C code in Haskell.
--
-- In general, they are used like so:
--
-- @
-- [cXXX| int(double x, float y) { \<C code\> } |]
-- @
--
-- Where @cXXX@ is one of the quasi-quoters defined in this section.
--
-- The syntax is essentially representing an anonymous C function:
--
-- * The first type to appear (@int@ in the example) is the return type
--   of said function.
--
-- * The arguments list (@(double x, float y)@ in the example) captures
--   Haskell variables currently in scope, and makes them available from
--   the C code.  If no parameters are present, the parentheses can be
--   omitted.
--
-- * The syntax of the @\<C code\>@ depends on on the quasi-quoter used,
--   and the anti-quoters available.  @cexp@ functions accept a C
--   expression.  @c@ functions accept a list of statemens, like the body
--   of a function.
--
-- See also the @tutorial.md@ file for more documentation.
--
-- === Variable capturing and type conversion.
--
-- The Haskell type of the inlined expression will be determined by the
-- C return type specified.  The conversion between the C type and the
-- Haskell type is performed according to the current 'Context' -- see
-- 'convertCType'.  C pointers and arrays are both converted to
-- Haskell @'Ptr'@s, and function pointers are converted to @'FunPtr'@s.
-- Sized arrays are not supported.
--
-- Similarly, when capturing Haskell variables using the parameters
-- list, their type is assumed to be of the Haskell type corresponding
-- to the C type provided.  For example, if we capture variable @x@
-- using @double x@ in the parameter list, the code will expect a
-- variable @x@ of type @CDouble@ in Haskell.
--
-- === Anti-quoters
--
-- Apart from using parameter lists, Haskell variables can be captured
-- using anti-quoters.  @inline-c@ provides a basic anti-quoting
-- mechanism extensible with user-defined anti-quoters (see
-- "Language.C.Inline.Context").  The basic anti-quoter lets you capture
-- Haskell variables on the fly, for example we might say
--
-- @
-- ['cexp'| double { cos($(double x)) } |]
-- @
--
-- Which would capture the Haskell variable @x@ of type @'CDouble'@.
--
-- Parameter list capturing and anti-quoting can be freely mixed.
--
-- === @pure@ and impure calls
--
-- Both @cexp@ and @c@ quasi-quoters are present in impure (the
-- default) and pure (postfixed with @_pure@) versions.  The impure
-- version will generate expressions of type @'IO' a@, where @a@ is the
-- specified return type.  On the other hand, @pure@ versions will
-- generate pure code.  Moreover, this difference will also carry over
-- to function pointers.  Impure quasi-quoters will convert C function
-- pointers to @'IO'@ functions in Haskell.  For example, if an argument
-- is of type @int (*add)(int, int)@, the impure quasi-quoters will
-- expect a @'FunPtr' ('CInt' -> 'CInt' -> 'IO' 'CInt')@, while the pure
-- ones a @'FunPtr' ('CInt' -> 'CInt' -> 'IO' 'CInt')@.
--
-- Obviously pure quoters should be used with care, since if the C code
-- is not pure you can break referential transparency.
--
-- === Safe and @unsafe@ calls
--
-- Finally, @unsafe@ variants of the quasi-quoters are provided to call
-- the C code unsafely, in the sense that the C code will block the RTS,
-- with the advantage of a faster call to the foreign code. See
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1590008.4.3>
-- for more info.
--
-- == Examples
--
-- === Inline C expression
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import           "Foreign.C.Types"
-- import           "Language.C.Inline"
--
-- 'include' "\<math.h\>"
--
-- c_cos :: 'CDouble' -> 'CDouble'
-- c_cos x = ['cexp_pure_unsafe'| double { cos($(double x)) } |]
-- @
--
-- === Inline C statements
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import qualified Data.Vector.Storable.Mutable as V
-- import           "Foreign.C.Types"
-- import           "Language.C.Inline"
--
-- 'include' "\<stdio.h\>"
--
-- parseVector :: 'CInt' -> 'IO' (V.IOVector 'CDouble')
-- parseVector len = do
--   vec <- V.new $ 'fromIntegral' len0
--   V.unsafeWith vec $ \\ptr -> ['c'| void(double *ptr) {
--     int i;
--     for (i = 0; i < $(int len); i++) {
--       scanf("%lf ", &ptr[i]);
--     }
--   } |]
--   'return' vec
-- @

cexp :: TH.QuasiQuoter
cexp = genericQuote IO $ inlineExp TH.Safe

cexp_unsafe :: TH.QuasiQuoter
cexp_unsafe = genericQuote IO $ inlineExp TH.Unsafe

cexp_pure :: TH.QuasiQuoter
cexp_pure = genericQuote Pure $ inlineExp TH.Safe

cexp_pure_unsafe :: TH.QuasiQuoter
cexp_pure_unsafe = genericQuote Pure $ inlineExp TH.Unsafe

c :: TH.QuasiQuoter
c = genericQuote IO $ inlineItems TH.Safe

c_unsafe :: TH.QuasiQuoter
c_unsafe = genericQuote IO $ inlineItems TH.Unsafe

c_pure :: TH.QuasiQuoter
c_pure = genericQuote Pure $ inlineItems TH.Safe

c_pure_unsafe :: TH.QuasiQuoter
c_pure_unsafe = genericQuote Pure $ inlineItems TH.Unsafe

quoteCode
  :: (String -> TH.ExpQ)
  -- ^ The parser
  -> TH.QuasiQuoter
quoteCode p = TH.QuasiQuoter
  { TH.quoteExp = p
  , TH.quotePat = error "inline-c: quotePat not implemented (quoteCode)"
  , TH.quoteType = error "inline-c: quoteType not implemented (quoteCode)"
  , TH.quoteDec = error "inline-c: quoteDec not implemeted (quoteCode)"
  }

genericQuote
  :: Purity
  -- ^ Whether the call and the function pointers should be pure or not.
  -> (TH.TypeQ -> C.Type -> [(C.Id, C.Type)] -> String -> TH.ExpQ)
  -- ^ Function taking that something and building an expression, see
  -- 'inlineExp' for other args.
  -> TH.QuasiQuoter
genericQuote pure build = quoteCode $ \s -> do
  initialiseModuleState_
  ctx <- getContext
  ParseTypedC cType cParams cExp <-
    runParserInQ s (isTypeName (ctxCTypesTable ctx)) $ parseTypedC $ ctxCAntiQuoters ctx
  hsType <- cToHs ctx cType
  hsParams <- forM cParams $ \(cId, cTy, parTy) -> do
    case parTy of
      Plain s' -> do
        hsTy <- cToHs ctx cTy
        mbHsName <- TH.lookupValueName s'
        hsExp <- case mbHsName of
          Nothing -> do
            error $ "Cannot capture Haskell variable " ++ show cId ++
                    ", because it's not in scope. (genericQuote)"
          Just hsName -> do
            hsExp <- TH.varE hsName
            case pure of
              Pure -> return hsExp
              IO -> [| \cont -> cont $(return hsExp) |]
        return (hsTy, hsExp)
      AntiQuote antiId dyn -> do
        case Map.lookup antiId (ctxCAntiQuoters ctx) of
          Nothing ->
            error $ "IMPOSSIBLE: could not find anti-quoter " ++ show antiId ++
                    ". (genericQuote)"
          Just (SomeCAntiQuoter antiQ) -> case fromSomeEq dyn of
            Nothing ->
              error  $ "IMPOSSIBLE: could not cast value for anti-quoter " ++
                       show antiId ++ ". (genericQuote)"
            Just x ->
              caqMarshaller antiQ (ctxCTypesTable ctx) pure cTy x
  let hsFunType = convertCFunSig hsType $ map fst hsParams
  let cParams' = [(cId, cTy) | (cId, cTy, _) <- cParams]
  buildFunCall ctx (build hsFunType cType cParams' cExp) (map snd hsParams) []
  where
    cToHs :: Context -> C.Type -> TH.TypeQ
    cToHs ctx cTy = do
      mbHsTy <- convertCType (ctxCTypesTable ctx) pure cTy
      case mbHsTy of
        Nothing -> error $ "Could not resolve Haskell type for C type " ++ pretty80 cTy
        Just hsTy -> return hsTy

    buildFunCall :: Context -> TH.ExpQ -> [TH.Exp] -> [TH.Name] -> TH.ExpQ
    buildFunCall _ctx f [] args =
      foldl (\f' arg -> [| $f' $(TH.varE arg) |]) f args
    buildFunCall ctx f (hsExp : params) args = do
      case pure of
        Pure -> [|
           let arg = $(return hsExp)
           in $(buildFunCall ctx f params (args ++ ['arg]))
          |]
        IO -> [| $(return hsExp) $ \arg ->
            $(buildFunCall ctx f params (args ++ ['arg]))
          |]

    convertCFunSig :: TH.Type -> [TH.Type] -> TH.TypeQ
    convertCFunSig retType params0 = do
      go params0
      where
        go [] = case pure of
          Pure -> return retType
          IO -> [t| IO $(return retType) |]
        go (paramType : params) = do
          [t| $(return paramType) -> $(go params) |]

-- | Emits an include CPP statement for C code associated with the current
-- module.  To avoid having to escape quotes, the function itself adds
-- them when appropriate, so that
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
  | null s = error "inline-c: empty string (include)"
  | head s == '<' = emitLiteral $ "#include " ++ s
  | otherwise = emitLiteral $ "#include \"" ++ s ++ "\""

------------------------------------------------------------------------
-- Utils

pretty80 :: PP.Pretty a => a -> String
pretty80 x = PP.displayS (PP.renderPretty 0.8 80 (PP.pretty x)) ""
