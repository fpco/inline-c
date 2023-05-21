- 0.9.1.8: Tighten ansi-wl-pprint upper bound, see issue #144.
- 0.9.1.7: Allow arbitrary number of C++ templates, see PR #141.
- 0.9.1.6: Fix mistakenly unsafe call, see issue #137.
- 0.9.1.5: Support multi-token types in C++ template arguments, see issue #125 and PR #126.
- 0.9.1.4: Support GHC 8.10, including better C++ flags handling, see PR #121.
- 0.9.1.3: Work around spurious test failures, see PR #118.
- 0.9.1.2: Update haddock for `Language.C.Inline.Interruptible.pure`.
- 0.9.1.1: Use `unsafeDupablePerformIO` rather than `unsafePerformIO`. See issue #115 and PR #117.
- 0.9.1.0: Add `Language.C.Inline.substitute` and `Language.C.Inline.getHaskellType`.
- 0.9.0.0: Add support for C++ namespace and template.
- 0.8.0.1: Compatibility with GHC 8.8
- 0.8: Add code locations.
- 0.7.0.1: Add more docs for `funPtr`
- 0.7.0.0: Add `funPtr` quasi-quoter
- 0.6.0.6: Support GHC 8.4
- 0.6.0.5: Update readme
- 0.6.0.4: Remove QuickCheck dependency
- 0.6.0.3: Remove cryptohash dependencies
- 0.6.0.2: Update haddock
- 0.6.0.0: Use `addDependentFile` so separate compilation is not needed.
- 0.5.6.0: Add `ForeignPtr` anti-quoter
- 0.5.5.9: Make tests work with QuickCheck < 2.9
- 0.5.5.8: Add workaround for QuickCheck-2.9 bug. See issue #51
- 0.5.5.2: Add docs regarding internals. See issue #41.
- 0.5.5.1: Add support for Interruptible calls. The version skip is
  simply because I forgot to update the changelog for 0.5.5.0.
- 0.5.4.3: Fix haddock docs.
- 0.5.4.2: Generate unique C names by prefixing the already generated
  name with the Haskell module name.  See issue #25.
- 0.5.4.1: Do not generate C code when haddock is type checking.  See
  issue #24.
- 0.5.4.0: Allow Haskell identifiers in anti-quotes.  See issue #23.
- 0.5.3.4: Fix `bsCtx` docs.
- 0.5.3.3:
  * Fix errors when using parallel builds.  See issue #22.
  * Use `fail` rather than `error` in the `Q` monad.
- 0.5.3.2: Make type errors with default anti-quoter much saner.
- 0.5.3.1: Fix leak of `FunPtr` when using `funCtx`.
- 0.5.3.0: Recognize more standard library types.  See pull request #19.
- 0.5.2.1: Convert `signed char` to `CSChar`.  See pull request #18.
- 0.5.2.0: Make `bs-ptr` use `char` instead of `unsigned char`.  See
  issue #16.
