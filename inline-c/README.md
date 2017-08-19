# inline-c

`inline-c` lets you seamlessly call C libraries and embed
high-performance inline C code in Haskell modules. Haskell and C can
be freely intermixed in the same source file, and data passed to and
from code in either language with minimal overhead. No FFI required.

`inline-c` is Haskell's escape hatch (or one of) to the wild world of
legacy code and high-performance numerical and system libraries. It
has other uses too: you can also think of `inline-c` as to Haskell
what inline Assembly is to C — a convenient means to eke out a little
bit of extra performance in those rare cases where C still beats
Haskell.

GHCi support is currently limited to using `-fobject-code`, see
the [last section](#ghci) for more info.

## Getting started

Let's say we want to compute the cosine of a number using C from
Haskell. `inline-c` let's you write this function call inline, without
any need for a binding to the foreign function:

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline as C

C.include "<math.h>"

main :: IO ()
main = do
  x <- [C.exp| double{ cos(1) } |]
  print x
```

`inline-c` leverages the [quasiquotation][ghc-manual-quasiquotation]
language extension implemented in GHC.
[Template Haskell][ghc-manual-template-haskell] is also required.
Importing the `Language.C.Inline` module brings in scope most required
Haskell definitions. `C.include "<math.h>"` brings into scope the
foreign function `cos()` that we wish to call. Finally, in the `main`
function, `[C.exp| double { cos(1) } |]` denotes an inline C expression
of type `double`. `cexp` stands for "C expression". It is a custom
quasiquoter provided by `inline-c`.

A `C.exp` quasiquotation always includes a type annotation for the
inline C expression. This annotation determines the type of the
quasiquotation in Haskell. Out of the box, `inline-c` knows how to map
many common C types to Haskell type. In this case,

```
[C.exp| double { cos(1) } |] :: IO CDouble
```

For pure C expression like these we also provide `C.pure`, which works
exactly the same but without the `IO`:

```
[C.pure| double { cos(1) } |] :: CDouble
```

Obviously extra care must be taken when using `C.pure`: the embedded C
code must be referentially transparent.

## Multiple statements

`inline-c` allows embedding arbitrary C code, not just expressions, in
the form of a sequence of statements, using the `c` quasiquoter:

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline as C

C.include "<stdio.h>"

main :: IO ()
main = do
  x <- [C.block| int {
      // Read and sum 5 integers
      int i, sum = 0, tmp;
      for (i = 0; i < 5; i++) {
        scanf("%d", &tmp);
        sum += tmp;
      }
      return sum;
    } |]
  print x
```

Just as with `C.exp`, we need a type annotation on the entire C block.
The annotation specifies the return type. That is, the type of the
expression in any return statement.

## Capturing Haskell variables -- parameter declaration

`inline-c` allows referring to Haskell variables inside C expressions
and code blocks. We do so by "anti-quoting" them.

Let's say that we wanted to parameterize the function we wrote above
by how many numbers we should read. We can do so by defining a Haskell
function whose parameter we can refer to from within C:

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Language.C.Inline as C
import           Foreign.C.Types

C.include "<stdio.h>"

-- | @readAndSum n@ reads @n@ numbers from standard input and returns
-- their sum.
readAndSum :: CInt -> IO CInt
readAndSum n  = [C.block| int {
    // Read and sum n integers
    int i, sum = 0, tmp;
    for (i = 0; i < $(int n); i++) {
      scanf("%d", &tmp);
      sum += tmp;
    }
    return sum;
  } |]

main :: IO ()
main = do
  x <- readAndSum 5
  print x
```

Here, the Haskell variable `n` is captured right where we need it using
`$(int n)`.  Standard anti-quotation (we'll talk about additional ones
later) consists of a `$` followed by a C declaration in parenthesis.
Note that any valid Haskell identifiers can be used when anti-quoting,
including ones including constructors, qualified names, names containing
unicode, etc.

For each anti-quotation, a variable with a matching type is expected in
the Haskell environment.  In this case `inline-c` expects a variable
named `n` of type `CInt`, which is the case.

## What can be captured and returned?

All C types correspond to exactly one Haskell type. Basic types (`int`,
`long`, `double`, `float`, etc.) get converted to their Haskell
equivalents `CInt`, `CLong`, `CDouble`, `CFloat`. Pointers and arrays
get converted to `Ptr`. Function pointers get converted to `FunPtr`.

`inline-c` can also handle user-defined structs and enums, provided that
they are instances of `Storable` and that you tell `inline-c` about them
using [contexts](#contexts).

## Contexts

Everything beyond the base functionality provided by `inline-c` is
specified in a structure that we call "`Context`".  From a user
perspective, if we want to use anything but the default context
(`C.baseCtx`), we must set the `C.Context` explicitly using the
`C.context` function.  The next two sections include several examples.

The `C.Context` allows to extend `inline-c` to support

* Custom C types beyond the basic ones;
* And [additional anti-quoters](#more-anti-quoters).

`C.Context`s can be composed using their `Monoid` instance.

Ideally a `C.Context` will be provided for each C library that should be
used with `inline-c`. The user can then combine multiple contexts
together if multiple libraries are to be used in the same program. See
the [`inline-c-nag` package](https://github.com/fpco/inline-c-nag) for
an example of using a `C.Context` tailored for a library.

For information regarding how to define `C.Context`s, see the
Haddock-generated API documentation for `Language.C.Inline.Context`.

## More anti-quoters

Besides the basic anti-quoter, which captures variables as they are,
some more anti-quoters are provided with additional functionality.  As
mentioned, `inline-c` can easily be extended with anti-quoters defined
by the user, using [contexts](#contexts).

### Vectors

The `vec-len` and `vec-ptr` anti-quoters in the `C.vecCtx` context let us
easily use [Haskell vectors](http://hackage.haskell.org/package/vector)
in C.  Continuing along the "summing" theme, we can write code that sums
Haskell vectors in C:

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Language.C.Inline as C
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Monoid ((<>))
import           Foreign.C.Types

-- To use the vector anti-quoters, we need the 'C.vecCtx' along with the
-- 'C.baseCtx'.
C.context (C.baseCtx <> C.vecCtx)

sumVec :: VM.IOVector CDouble -> IO CDouble
sumVec vec = [C.block| double {
    double sum = 0;
    int i;
    for (i = 0; i < $vec-len:vec; i++) {
      sum += $vec-ptr:(double *vec)[i];
    }
    return sum;
  } |]

main :: IO ()
main = do
  x <- sumVec =<< V.thaw (V.fromList [1,2,3])
  print x
```

The `vec-len` anti-quoter is used simply by specifying the vector we
want to get the length of (in our case, `vec`).  To use the `vec-ptr`
anti-quoter it is also required to specify the pointer type we want.
Since `vec` is a vector of `CDouble`s, we want a pointer to `double`s.

## ByteStrings

The `bs-len` and `bs-ptr` ant-quoters in the `C.bsCtx` context work
exactly the same as the `vec-len` and `vec-ptr` counterparts, but with
strict `ByteString`s.  The only difference is that it is no necessary to
specify the type of the pointer from C -- it is always going to be
`char *`:

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import           Foreign.C.Types
import qualified Language.C.Inline as C

C.context (C.baseCtx <> C.bsCtx)

-- | Count the number of set bits in a 'BS.ByteString'.
countSetBits :: BS.ByteString -> IO CInt
countSetBits bs = [C.block|
    int {
      int i, bits = 0;
      for (i = 0; i < $bs-len:bs; i++) {
        char ch = $bs-ptr:bs[i];
        bits += (ch * 01001001001ULL & 042104210421ULL) % 017;
      }
      return bits;
    }
  |]
```

### Function pointers

Using the `fun` anti-quoter, present in the `C.funCtx` context, we can
easily turn Haskell function into function pointers.

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Language.C.Inline as C

-- To use the function pointer anti-quoter, we need the 'C.funCtx along with
-- the 'C.baseCtx'.
C.context (C.baseCtx <> C.funCtx)

ackermann :: CLong -> CLong -> CLong
ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | otherwise = ackermann (m - 1) (ackermann m (n - 1))

main :: IO ()
main = do
  let ackermannIO m n = return $ ackermann m n
  let x = 3
  let y = 4
  z <- [C.exp| long{
      $fun:(long (*ackermannIO)(long, long))($(long x), $(long y))
    } |]
  print z
```

In this example, we capture a Haskell function of type `CLong -> CLong
-> IO CLong`, `ackermannIO`, to a function pointer in C, using the `fun`
anti-quoter.  Note how we need to specify the function pointer type when
we capture `ackermannIO`, using standard C declaration syntax.  Also
note that the `fun` anti-quoter works with `IO` functions, and so we
needed to modify `ackermann` to make it have the right type.

In general, when anti-quoting, if the type can be inferred (like in the
case of `vec-len`), only the Haskell identifier appears.  If it can't,
the target C type and the Haskell identifier are mentioned using C
declaration syntax.

## GHCi

Currently `inline-c` does not work in interpreted mode. However, GHCi
can still be used using the `-fobject-code` flag. For speed, we
reccomend passing `-fobject-code -O0`, for example

```
stack ghci --ghci-options='-fobject-code -O0'
```

or

```
cabal repl --ghc-options='-fobject-code -O0'
```

[ghc-manual-quasiquotation]:
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/template-haskell.html#th-quasiquotation
[ghc-manual-template-haskell]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/template-haskell.html
