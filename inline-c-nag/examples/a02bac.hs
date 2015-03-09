{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
import           Language.C.Inline.Nag
import           Text.RawString.QQ (r)

include "<nag.h>"
include "<stdio.h>"
include "<nag_stdlib.h>"
include "<naga02.h>"

-- Code dump test

emitLiteral [r|
int test_emitCode(void)
{
  Integer     exit_status = 0;
  Complex     v, w, z;
  double      r, theta, x, y;
  Nag_Boolean equal, not_equal;

  printf("nag_complex (a02bac) Example Program Results\n");

  x = 2.0;
  y = -3.0;
  /* nag_complex (a02bac).
   * Complex number from real and imaginary parts
   */
  z = nag_complex(x, y);

  printf("  %-21s  %s    %8s =  %7.4f, %7.4f\n", "", "", "x, y",
          x, y);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex",
          "z", "(x,y)", z.re, z.im);
  /* nag_complex_real (a02bbc).
   * Real part of a complex number
   */
  printf("  %-21s: %s    %8s =  %7.4f\n", "nag_complex_real",
          "", "real(z)", nag_complex_real(z));
  /* nag_complex_imag (a02bcc).
   * Imaginary part of a complex number
   */
  printf("  %-21s: %s    %8s =  %7.4f\n", "nag_complex_imag",
          "", "imag(z)", nag_complex_imag(z));
  /* nag_complex (a02bac), see above. */
  v = nag_complex(3.0, 1.25);
  /* nag_complex (a02bac), see above. */
  w = nag_complex(2.5, -1.75);
  printf("  %-21s: %s    %8s = (%7.4f, %7.4f)\n", "nag_complex", "",
          "v", v.re, v.im);
  printf("  %-21s: %s    %8s = (%7.4f, %7.4f)\n", "nag_complex", "",
          "w", w.re, w.im);
  /* nag_complex_add (a02cac).
   * Addition of two complex numbers
   */
  z = nag_complex_add(v, w);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_add",
          "z", "v+w", z.re, z.im);
  /* nag_complex_subtract (a02cbc).
   * Subtraction of two complex numbers
   */
  z = nag_complex_subtract(v, w);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n",
          "nag_complex_subtract", "z", "v-w", z.re, z.im);
  /* nag_complex_multiply (a02ccc).
   * Multiplication of two complex numbers
   */
  z = nag_complex_multiply(v, w);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n",
          "nag_complex_multiply", "z", "v*w", z.re, z.im);
  /* nag_complex_divide (a02cdc).
   * Quotient of two complex numbers
   */
  z = nag_complex_divide(v, w);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_divide",
          "z", "v/w", z.re, z.im);
  /* nag_complex_negate (a02cec).
   * Negation of a complex number
   */
  z = nag_complex_negate(w);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_negate",
          "z", "-w", z.re, z.im);
  /* nag_complex_conjg (a02cfc).
   * Conjugate of a complex number
   */
  z = nag_complex_conjg(w);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_conjg",
          "z", "conjg(w)", z.re, z.im);
  /* nag_complex_equal (a02cgc).
   * Equality of two complex numbers
   */
  equal = nag_complex_equal(v, w);
  if (equal)
    printf("  %-21s: %s == %s\n", "nag_complex_equal", "v", "w");
  else
    printf("  %-21s: %s != %s\n", "nag_complex_equal", "v", "w");
  /* nag_complex_not_equal (a02chc).
   * Inequality of two complex numbers
   */
  not_equal = nag_complex_not_equal(w, z);
  if (not_equal)
    printf("  %-21s: %s != %s\n\n", "nag_complex_not_equal", "w", "z");
  else
    printf("  %-21s: %s == %s\n\n", "nag_complex_not_equal", "w", "z");

  /* nag_complex_arg (a02dac).
   * Argument of a complex number
   */
  theta = nag_complex_arg(z);
  printf("  %-21s: %s    %8s =  %7.4f\n", "nag_complex_arg", "",
          "arg(z)", theta);
  /* nag_complex_abs (a02dbc).
   * Modulus of a complex number
   */
  r = nag_complex_abs(z);
  printf("  %-21s: %s = %8s =  %7.4f\n", "nag_complex_abs", "r",
          "abs(z)", r);
  /* nag_complex_sqrt (a02dcc).
   * Square root of a complex number
   */
  v = nag_complex_sqrt(z);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_sqrt",
          "v", "sqrt(z)", v.re, v.im);
  /* nag_complex_i_power (a02ddc).
   * Complex number raised to integer power
   */
  v = nag_complex_i_power(z, (Integer) 3);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_i_power",
          "v", "z**3", v.re, v.im);
  /* nag_complex_r_power (a02dec).
   * Complex number raised to real power
   */
  v = nag_complex_r_power(z, 2.5);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_r_power",
          "v", "z**2.5", v.re, v.im);
  /* nag_complex_c_power (a02dfc).
   * Complex number raised to complex power
   */
  v = nag_complex_c_power(z, w);
  printf("  %-21s: %s = %8s = (%7.4f,%8.4f)\n", "nag_complex_c_power",
          "v", "z**w", v.re, v.im);
  /* nag_complex_log (a02dgc).
   * Complex logarithm
   */
  v = nag_complex_log(z);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_log",
          "v", "log(z)", v.re, v.im);
  /* nag_complex_exp (a02dhc).
   * Complex exponential
   */
  z = nag_complex_exp(v);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_exp",
          "z", "exp(v)", z.re, z.im);
  /* nag_complex_sin (a02djc).
   * Complex sine
   */
  v = nag_complex_sin(z);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_sin",
          "v", "sin(z)", v.re, v.im);
  /* nag_complex_cos (a02dkc).
   * Complex cosine
   */
  v = nag_complex_cos(z);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_cos",
          "v", "cos(z)", v.re, v.im);
  /* nag_complex_tan (a02dlc).
   * Complex tangent
   */
  v = nag_complex_tan(z);
  printf("  %-21s: %s = %8s = (%7.4f, %7.4f)\n", "nag_complex_tan",
          "v", "tan(z)", v.re, v.im);
  /* nag_complex_divide (a02cdc), see above. */
  v = nag_complex_divide(nag_complex_sin(z), nag_complex_cos(z));
  printf("  %-21s:%13s = (%7.4f, %7.4f)\n", "nag_complex_divide",
          "sin(z)/cos(z)", v.re, v.im);

  return exit_status;
}
|]

main :: IO ()
main = do
  [cexp| void{ test_emitCode() } |]

