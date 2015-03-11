# `inline-c` tutorial

`inline-c` is a library to seamlessy write C code -- using C libraries
-- from Haskell.  In this tutorial we'll work by example, starting with
simple use cases and gradually showing every feature with more complex
examples.

Build details are reserved to the [last section](#how-to-build).

## Getting started

Let's say we want to compute the cosine of a number using C from
Haskell.  With `inline-c` it's a breeze:

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Language.C.Inline

include "<math.h>"

main :: IO ()
main = do
  x <- [cexp| double{ cos(1) } |]
  print x
```

The code is quite self-explanatory, but it's still worth analyzing it
piece by piece.

The first three lines enable the Haskell language extensions that we
need to work with `inline-c`, and import the `inline-c` module,
`Language.C.Inline`.

The line containing `inline "<math.h>"` will have the effect of an
`#include <math.h>` directive being included in the C code that will
contain all the inline C that follows.

Finally, in the `main` function, `[cexp| double { cos(1) } |]` is the
expression which actually includes the C code, `cexp` standing for "C
expression".  Every inline C code is included as a typed block, as shown
in this case, where the block has type `double`.

The type of the inline C Haskell expression will be determined by the
type of the C block.  In this case, since the C block is typed `double`,
the Haskell expression `[cexp| double { cos(1) } |]` will have type `IO
CDouble`.

We can also include C code as pure, non-IO Haskell expressions, using
`cexp_pure`:

```
main :: IO ()
main = do
  let x = [cexp_pure| double{ cos(1) } |]
  print x
```

Obviously in this case the user needs to make sure that the C code is
referentially transparent.

## Multiple statements

We can also include C code composed of multiple statements, rather than
a single expression.  This is accomplished with the `c`
quasi-quoter, instead of `cexp`.

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Language.C.Inline

include "<stdio.h>"

main :: IO ()
main = do
  x <- [c| int {
      // Read and sum 5 integers
      int i, sum = 0, tmp;
      for (i = 0; i < 5; i++) {
        scanf("%d ", &tmp);
        sum += tmp;
      }
      return sum;
    } |]
  print x
```

Note how, when we use `c`, we still type the whole C block (`int`
in this case), but we must `return` the value explicitly, like we would in
a C function.

A pure version of `c` exists too -- `c_pure` -- but in this
case we must not use it, since the C code is impure.

## Capturing Haskell variables -- parameter declaration

`inline-c` offers the possibility of using Haskell variables from C.  We
can do so in two ways: declaring a parameter and using an anti-quoter.
We'll talk about the former first.

Let's say that we wanted to parametrize the function we wrote above by
how many numbers we should read.  We can do so by defining an Haskell
function with the right parameter and using said parameter from C.

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Language.C.Inline

include "<stdio.h>"

-- | @readAndSum n@ reads @n@ numbers from standard input and returns
-- their sum.
readAndSum :: CInt -> IO CInt
readAndSum n  = [c| int (int n) {
    // Read and sum n integers
    int i, sum = 0, tmp;
    for (i = 0; i < n; i++) {
      scanf("%d ", &tmp);
      sum += tmp;
    }
    return sum;
  } |]

main :: IO ()
main = do
  x <- readAndSum 5
  print x
```

The interesting bit in the code above is right next to the type
declaration for the C block, `int (int n) { ... }`.  Along the return
type, `int`, we also have a parameter list, in this case composed of
just one parameter, `int n`.  The parameters specified in this way are
then available in the C code, as shown here, `n` being used throughout
the C snippet.

For each specified parameter, a variable with a matching type is
expected in the Haskell environment.  In this case `inline-c` expects a
variable named `n` of type `CInt`, which is the case.

## Capturing Haskell variables -- anti-quotation

The second way to capture Haskell variables is by "anti-quoting" them.

```
readAndSum :: CInt -> IO CInt
readAndSum n  = [c| int {
    // Read and sum n integers
    int i, sum = 0, tmp;
    for (i = 0; i < $(int n); i++) {
      scanf("%d ", &tmp);
      sum += tmp;
    }
    return sum;
  } |]
```

In this case the Haskell variable `n` is not captured via a parameter
declaration, but right where we need it using `$(int n)`.  When the
captured variable is used one, anti-quotation provide a cheaper notation
to do what we need.

Note that parameter declarations and anti-quotations can be mixed
freely.

## What can be captured and returned?

Every of the stock types in the C language can be easily represented in
Haskell.  Basic types (`int`, `long`, `double`, `float`, etc.) get
converted to their Haskell equivalents in `Foreign.C.Types`.  Pointers
and arrays get converted to `Ptr`.  Function pointers get converted to
`FunPtr`.

`inline-c` can also handle user-defined structs and enums, provided that
they are instances of `Storable` and that you tell `inline-c` about them
using [contexts](#contexts).

## Contexts

Everything beyond the base functionality provided by `inline-c` is
specified in a structure that we call "`Context`".  From a user
perspective, if we want to use anything but the default context
(`baseCtx`), before using any of the facilities provided by `inline-c`
we must specify the `Context` we want to use using the `setContext`
function.

Specifically, the `Context` allows to extend `inline-c` to support

* Custom C types beyond the basic ones;
* And [additional anti-quoters](#more-anti-quoters).

`Context`s can be composed using their `Monoid` instance, as we will see
in the next section.

Ideally a `Context` will be provided for each C library that should be
used with `inline-c`, so that the user will simply use that, or combine
multiple ones if multiple libraries are to be used.  See the following
section for examples using a `Context` tailored for a library -- in this
case NAG.

For information regarding how to define `Context`s, see the Haddock
docs for `Language.C.Inline.Context`.

## More anti-quoters

Besides the basic anti-quoter, which captures variables as they are,
some more anti-quoters are provided with additional functionality.  In
fact, `inline-c` can easily be extended with anti-quoters defined by the
user, using [contexts](#contexts).

### Vectors

The `vec-len` and `vec-ptr` anti-quoters in the `vecCtx` context let us
easily use [Haskell vectors](http://hackage.haskell.org/package/vector)
in C.  Continuing along the "summing" theme, we can write code that sums
Haskell vectors in C:

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Language.C.Inline
import qualified Data.Vector.Storable.Mutable as V
import           Data.Monoid ((<>))

-- To use the vector anti-quoters, we need the 'vecCtx' along with the
-- 'baseCtx'.
setContext (baseCtx <> vecCtx)

sumVec :: V.IOVector CDouble -> IO CDouble
sumVec vec = [c| double {
    double sum = 0;
    int i;
    for (i = 0; i < $vec-len:vec; i++) {
      sum += $vec-ptr:(double *vec)[i];
    }
    return sum
  } |]

main :: IO ()
main = do
  x <- sumVec =<< V.fromList [1,2,3]
  print x
```

The `vec-len` anti-quoter is used simply by specifying the vector we
want to get the length of (in our case, `vec`).  To use the `vec-ptr`
anti-quoter it is also required to specify the pointer type we want.
Since `vec` is a vector of `CDouble`s, we want a pointer to `double`s.

### Function pointers

Using the `fun` anti-quoter, present in the `funCtx` context, we can
easily turn Haskell function into function pointers.

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Language.C.Inline

-- To use the function pointer anti-quoter, we need the 'funCtx along with
-- the 'baseCtx'.
setContext (baseCtx <> funCtx)

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
  z <- [cexp| long{
      $fun:(int (*ackermannIO)(int, int))($(long x), $(long y))
    } |]
  print z
```

In this example, we capture a Haskell function of type `CLong -> CLong
-> IO CLong`, `ackermannIO`, to a function pointer in C, using the `fun`
anti-quoter.  Note how we need to specify the function pointer type when
we capture `ackermannIO`, using standard C declaration syntax.  In
general, when anti-quoting, if the type can be inferred (like in the
case of `vec-len`), only the Haskell identifier appears.  If it can't,
the target C type and the Haskell identifier are mentioned using C
declaration syntax.

## Using `inline-c` with NAG

The `inline-c-nag` package provides a `Context` and various utilities
which make it easy to use the NAG library from Haskell.  We present two
examples which not only demonstrate that but also show a nice mix of the
features available in `inline-c`.

### One dimensional FFT

In this first example we will compute the forward discrete Fourier
transform of a sequence of complex numbers, using the
[`nag_sum_fft_complex_1d`](http://www.nag.com/numeric/CL/nagdoc_cl24/html/C06/c06pcc.html)
function in the NAG library.

While the example is short it showcases various features, including the
already seen ordinary and vector anti-quoting; but also some NAG
specific goodies such as handling of custom types (complex numbers) and
error handling using the `withNagError` function, defined in the
`Language.C.Inline.Nag` module provided by `inline-c-nag`.

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Language.C.Inline.Nag
import qualified Data.Vector.Storable as V

-- Set the 'Context' to the one provided by "Language.C.Inline.Nag".
-- This gives us access to NAG types such as 'Complex' and 'NagError',
-- and also includes the vector and function pointers anti-quoters.
setContext nagCtx

-- Include the headers files we need.
include "<nag.h>"
include "<nagc06.h>"

-- | Computes the discrete Fourier transform for the given sequence of
-- 'Complex' numbers.  Returns 'Left' if some error occurred, together
-- with the error message.
forwardFFT :: V.Vector Complex -> IO (Either String (V.Vector Complex))
forwardFFT x_orig = do
  -- "Thaw" the input vector -- the input is an immutable vector, and by
  -- "thawing" it we create a mutable copy of it.
  x <- V.thaw x_orig
  -- Use 'withNagError' to easily check whether the NAG operation was
  -- successful.
  withNagError $ \fail_ -> do
    [cexp| void {
       nag_sum_fft_complex_1d(
         // We're computing a forward transform
         Nag_ForwardTransform,
         // We take the pointer underlying 'x' and it's length, using the
         // appropriate anti-quoters
         $vec-ptr:(Complex *x), $vec-len:x,
         // And pass in the NagError structure given to us by
         // 'withNagError'.
         $(NagError *fail_))
      } |]
    -- Turn the mutable vector back to an immutable one using 'V.freeze'
    -- (the inverse of 'V.thaw').
    V.freeze x

-- Run our function with some sample data and print the results.
main :: IO ()
main = do
  let vec = V.fromList
        [ Complex 0.34907 (-0.37168)
        , Complex 0.54890 (-0.35669)
        , Complex 0.74776 (-0.31175)
        , Complex 0.94459 (-0.23702)
        , Complex 1.13850 (-0.13274)
        , Complex 1.32850   0.00074
        , Complex 1.51370   0.16298
        ]
  printVec vec
  Right vec_f <- forwardFFT vec
  printVec vec_f
  where
    printVec vec = do
      V.forM_ vec $ \(Complex re im) -> putStr $ show (re, im) ++ " "
      putStrLn ""
```

Note how we're able to use the `nag_sum_fft_complex_1d` function just
for the feature we need, using the `Nag_ForwardTransform` enum directly
in the C code, instead of having to define some Haskell interface for
it.  Using facilities provided by `inline-c-nag` we're also able to have
nice error handling, automatically extracting the error returned by NAG
if something goes wrong.

### Nelder-Mead optimization

For a more complex example, we'll write an Haskell function that
performs Nelder-Mead optimization using the
[`nag_opt_simplex_easy`](http://www.nag.com/numeric/CL/nagdoc_cl24/html/E04/e04cbc.html)
function provided by NAG.

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (poke)
import           Language.C.Inline.Nag

setContext nagCtx

include "<math.h>"
include "<nag.h>"
include "<nage04.h>"
include "<nagx02.h>"

nelderMead
  :: V.Vector CDouble
  -- ^ Starting point
  -> (V.Vector CDouble -> CDouble)
  -- ^ Function to minimize
  -> Nag_Integer
  -- ^ Maximum number of iterations (must be >= 1).
  -> IO (Either String (V.Vector CDouble))
  -- ^ Position of the minimum.  'Left' if something went wrong, with
  -- error message.
nelderMead xImm pureFunct maxcal = do
    -- Create function that the C code will use.
    let funct n xc fc _comm = do
          xc' <- newForeignPtr_ xc
          let f = pureFunct $ V.unsafeFromForeignPtr0 xc' $ fromIntegral n
          poke fc f
    -- Create mutable input/output vector for C code
    x <- V.thaw xImm
    -- Call the C code
    withNagError $ \fail_ -> do
      [c| void {
          // The function requires an exit parameter where to store the minimum
          // cost.  We use a variable defined in C to have something to work
          // with, although we do not use the result that wil be stored in it.
          double f;
          // We hardcode sensible values (see NAG documentation) for the
          // error tolerance, computed using NAG's nag_machine_precision.
          double tolf = sqrt(nag_machine_precision);
          double tolx = sqrt(tolf);
          // Call the function
          nag_opt_simplex_easy(
            // Get vector length and pointer.
            $vec-len:x, $vec-ptr:(double *x),
            &f, tolf, tolx,
            // Pass function pointer to our Haskell function using the fun
            // anti-quotation.
            $fun:(void (*funct)(Integer n, const double *xc, double *fc, Nag_Comm *comm)),
            // We do not provide a "monitoring" function.
            NULL,
            // Capture Haskell variable with the max number of iterations.
            $(Integer maxcal),
            // Do not provide the Nag_Comm parameter, which we don't need.
            NULL,
            // Pass the NagError parameter provided by withNagError
            $(NagError *fail_));
        } |]
      -- Get a new immutable vector by freezing the mutable one.
      V.freeze x

-- Optimize a two-dimensional function.  Example taken from
-- <http://www.nag.com/numeric/CL/nagdoc_cl24/examples/source/e04cbce.c>.
main :: IO ()
main = do
  let funct = \x ->
        let x0 = x V.! 0
            x1 = x V.! 1
        in exp x0 * (4*x0*(x0+x1)+2*x1*(x1+1.0)+1.0)
      start = V.fromList [-1, 1]
  Right end <- nelderMead start funct 500
  printVec end
  where
    printVec vec = do
      V.forM_ vec $ \x -> putStr $ show x ++ " "
      putStrLn ""
```

Again, in this example we use a function with a very complex and
powerful signature, such as the one for the Nelder-Mead optimization in
NAG, in a very specific way -- avoiding the high cost of having to
specify a well-designed Haskell interface for it.

## How to build

Each module that uses at least one of the `inline-c` functions gets a C
file associated to it, where the filename of said file will be the same
as the module but with a C extension.  This C file must be built after
the Haskell code and linked appropriately.  If you use cabal, all you
have to do is declare each associated C file in the @.cabal@ file and
you are good.

For example we might have

```
executable foo
  main-is:             Main.hs, Foo.hs, Bar.hs
  hs-source-dirs:      src
  -- Here the corresponding C sources must be listed for every module
  -- that uses C code.  In this example, Main.hs and Bar.hs do, but
  -- Foo.hs does not.
  c-sources:           src/Main.c, src/Bar.c
  -- These flags will be passed to the C compiler
  cc-options:          -Wall -O2
  -- Libraries to link the code with.
  extra-libraries:     -lm
  ...
```

Note that currently @cabal repl@ is not supported, because the C code
is not compiled and linked appropriately.

See `sample-cabal-project` for a working example.

If we were to compile the above manaully we could do

```
$ ghc -c Main.hs
$ cc -c Main.c -o Main_c.o
$ ghc Foo.hs
$ ghc Bar.hs
$ cc -c Bar.c -o Bar_c.o
$ ghc Main.o Foo.o Bar.o Main_c.o Bar_c.o -lm -o Main
```
