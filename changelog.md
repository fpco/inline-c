- 0.5.5.7: Add workaround for QuickCheck-2.9 bug. See issue #51
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
