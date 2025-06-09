Very quick and dirty "inline Verilog" support for Haskell. See `tests.hs` for examples.

Currently missing or untested:

* Inputs/outputs wider than 64 bits.
* `struct` ports.
* Multidimensional input/output ports, e.g. `reg [15:0] foo [3:0][3:0]` .
* Importing.

All of the above should be easy, I just didn't bother yet.

Also, `inline-verilog` does not make much sense for non-combinatorial circuits (e.g. stuff that does IO).