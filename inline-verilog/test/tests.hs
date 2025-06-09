{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Language.Verilog.Inline as V
import           Data.Word
import           Text.RawString.QQ
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable (peek)
import           Data.Bits
import           Text.Printf
import           Test.Hspec
import qualified Test.QuickCheck as QC

V.verilog

hello_world :: IO ()
hello_world =
  [V.block|
    module;
      initial begin
        $display("Hello World!");
      end
    endmodule
  |]

adder :: Word16 -> Word16 -> IO (Bool, Word16)
adder a b = alloca $ \sum -> alloca $ \carry_out -> do
  [V.block|
    module (
        input  [15:0] a,
        input  [15:0] b,
        output [15:0] sum,
        output        carry_out
    );
        assign {carry_out, sum} = a + b;
    endmodule
  |]
  (,) <$> peek carry_out <*> peek sum

V.verbatim [r|
  module popcount #(
    parameter WIDTH = 32
  ) (
    input wire [WIDTH-1:0]  data,
    // The output width must be large enough to hold the maximum possible
    // count, which is equal to WIDTH. $clog2(WIDTH) gives the number of
    // bits to address WIDTH items (e.g., 6 for 64), so we need one extra
    // bit to hold the value of WIDTH itself.
    output reg [$clog2(WIDTH):0] count
  );

    integer i;

    // This combinational block continuously calculates the popcount.
    // Synthesis tools will typically implement this as an efficient adder tree.
    always @(*) begin
      count = 0;
      for (i = 0; i < WIDTH; i = i + 1) begin
        count = count + {{$clog2(WIDTH){1'b0}}, data[i]};
      end
    end

  endmodule
|]

bitManip :: Word64 -> Word32 -> Word8 -> Word8 -> IO (Word64, Word32, Bool)
bitManip in_A in_B shuffle_control op_select = do
  alloca $ \out_shuffled -> alloca $ \out_op_result -> alloca $ \out_popcount_A_gt_B -> do
    [V.block|
      module (
        input wire [63:0] in_A,               // Primary 64-bit data input for shuffling and operations
        input wire [31:0] in_B,               // Secondary 32-bit data input for operations
        input wire [2:0]  shuffle_control,    // Selects the shuffle operation for in_A
        input wire [1:0]  op_select,          // Selects the bitwise operation for in_A[31:0] and in_B
        output reg [63:0] out_shuffled,       // Output of the 64-bit shuffle operation
        output reg [31:0] out_op_result,      // Output of the 32-bit bitwise operation
        output reg        out_popcount_A_gt_B // '1' if popcount of in_A > popcount of in_B
      );
        // -- Section 1: Parallel Bit Shuffling --
        // This block shuffles the 64 bits of `in_A` based on `shuffle_control`.
        // This functionality remains unchanged.
        always @(*) begin
          case (shuffle_control)
            3'b000: out_shuffled = in_A; // No change
            3'b001: out_shuffled = {in_A[31:0], in_A[63:32]}; // Swap halves
            3'b010: out_shuffled = {<<{in_A}}; // Bit reversal
            3'b011: out_shuffled = (in_A << 16) | (in_A >> 48); // 16-bit barrel rotate left
            3'b100: out_shuffled = {in_A[0],  in_A[1],  in_A[2],  in_A[3],  in_A[4],  in_A[5],  in_A[6],  in_A[7],
                                  in_A[15:8], in_A[23:16], in_A[31:24], in_A[39:32],
                                  in_A[47:40], in_A[55:48], in_A[63:56]}; // Swap bytes within words
            default: out_shuffled = 64'hDEADBEEF_DEADBEEF; // Default case
          endcase
        end

        // -- Section 2: Parallel Bitwise Operation --
        // This block performs a bitwise operation between the lower 32 bits of `in_A`
        // and the 32 bits of `in_B`. This functionality remains unchanged.
        always @(*) begin
          case (op_select)
            2'b00: out_op_result = in_A[31:0] & in_B;  // 32 parallel ANDs
            2'b01: out_op_result = in_A[31:0] | in_B;  // 32 parallel ORs
            2'b10: out_op_result = in_A[31:0] ^ in_B;  // 32 parallel XORs
            default: out_op_result = ~in_A[31:0];      // 32 parallel NOTs
          endcase
        end

        // -- Section 3: Parallel Population Count Comparison (Refactored) --
        // Wires to connect to the outputs of the Popcount modules.
        wire [$clog2(64):0] popcount_A_result; // Result from 64-bit popcount, 7 bits wide
        wire [$clog2(32):0] popcount_B_result; // Result from 32-bit popcount, 6 bits wide

        // Instantiate the Popcount module for the 64-bit input 'in_A'.
        // The WIDTH parameter is set to 64.
        popcount #(.WIDTH(64)) popcount_A_inst (
          .data(in_A),
          .count(popcount_A_result)
        );

        // Instantiate the Popcount module for the 32-bit input 'in_B'.
        // The WIDTH parameter is set to 32.
        popcount #(.WIDTH(32)) popcount_B_inst (
          .data(in_B),
          .count(popcount_B_result)
        );

        // Perform the comparison on the results from the instantiated modules.
        always @(*) begin
          if (popcount_A_result > {1'b0, popcount_B_result}) begin
            out_popcount_A_gt_B = 1'b1;
          end else begin
            out_popcount_A_gt_B = 1'b0;
          end
        end

      endmodule
    |]
    (,,) <$> peek out_shuffled <*> peek out_op_result <*> peek out_popcount_A_gt_B

-- The main function that encapsulates the logic of the Verilog module.
-- It takes the same inputs and returns a tuple with the three outputs.
bitManipHs
  :: Word64 -- ^ in_A: Primary 64-bit data input
  -> Word32 -- ^ in_B: Secondary 32-bit data input
  -> Word8  -- ^ shuffle_control: 3-bit control signal (0-7)
  -> Word8  -- ^ op_select: 2-bit control signal (0-3)
  -> (Word64, Word32, Bool) -- ^ (out_shuffled, out_op_result, out_popcount_A_gt_B)
bitManipHs in_A in_B shuffle_control op_select =
  (out_shuffled, out_op_result, out_popcount_A_gt_B)
 where
  -- Section 1: Parallel Bit Shuffling
  -- Translates the Verilog case statement for shuffling `in_A`.
  -- Operations that are a single netlist in hardware require masking,
  -- shifting, and combining in software.
  out_shuffled = case shuffle_control of
    0 -> in_A -- No change
    1 -> (in_A `shiftL` 32) .|. (in_A `shiftR` 32) -- Swap halves
    2 -> bitReverse64 in_A -- Bit reversal
    3 -> rotateL in_A 16 -- 16-bit barrel rotate left
    4 -> custom_shuffle in_A -- The complex byte-swapping pattern
    _ -> 0xDEADBEEFDEADBEEF -- Default case

  -- Helper function for the specific byte shuffle from the Verilog.
  -- This is a great example of something trivial in hardware (rewiring)
  -- that is complex and slow in software.
  custom_shuffle :: Word64 -> Word64
  custom_shuffle val =
    let -- Extract each byte from the input value
        byte0 = (val `shiftR` 0)  .&. 0xFF
        byte1 = (val `shiftR` 8)  .&. 0xFF
        byte2 = (val `shiftR` 16) .&. 0xFF
        byte3 = (val `shiftR` 24) .&. 0xFF
        byte4 = (val `shiftR` 32) .&. 0xFF
        byte5 = (val `shiftR` 40) .&. 0xFF
        byte6 = (val `shiftR` 48) .&. 0xFF
        byte7 = (val `shiftR` 56) .&. 0xFF
        -- Scatter the first 8 bits individually to the top byte of the output,
        -- which is what the Verilog `{in_A[0], in_A[1], ...}` syntax implies.
        bit_scatter = sum [ if testBit val i then bit (63-i) else 0 | i <- [0..7] ]
    in
        bit_scatter              -- Bits 0-7  -> Bits 63-56
        .|. (byte1 `shiftL` 48)  -- Byte 1    -> Bits 55-48
        .|. (byte2 `shiftL` 40)  -- Byte 2    -> Bits 47-40
        .|. (byte3 `shiftL` 32)  -- Byte 3    -> Bits 39-32
        .|. (byte4 `shiftL` 24)  -- Byte 4    -> Bits 31-24
        .|. (byte5 `shiftL` 16)  -- Byte 5    -> Bits 23-16
        .|. (byte6 `shiftL` 8)   -- Byte 6    -> Bits 15-8
        .|. byte7                -- Byte 7    -> Bits 7-0

  -- Section 2: Parallel Bitwise Operation
  -- Translates the Verilog case statement for the bitwise operation.
  -- `fromIntegral` truncates the 64-bit `in_A` to 32 bits to match `in_B`.
  in_A_32 = fromIntegral in_A :: Word32
  out_op_result = case op_select of
    0 -> in_A_32 .&. in_B
    1 -> in_A_32 .|. in_B
    2 -> in_A_32 `xor` in_B
    _ -> complement in_A_32 -- Default is NOT

  -- Section 3: Parallel Population Count Comparison
  -- `popCount` is a built-in, efficient software equivalent to the hardware
  -- adder tree described in the Verilog comments.
  popcount_A = popCount in_A
  popcount_B = popCount in_B
  out_popcount_A_gt_B = popcount_A > popcount_B

main :: IO ()
main = hspec $ do
  describe "Verilog Adder" $ do
    -- Section 1: Property-based testing with QuickCheck
    -- This section verifies the adder's correctness against the native Haskell
    -- addition for a large number of random inputs.
    context "Property-based tests (QuickCheck)" $ do
      it "should match Haskell's native addition and carry logic" $
        -- The 'property' function tells Hspec to run a QuickCheck test.
        QC.property $ \(a :: Word16, b :: Word16) -> do
          -- Call the adder function (our FFI-bound Verilog module)
          (verilog_carry, verilog_sum) <- adder a b

          -- Calculate the expected results using a wider Integer type in Haskell
          -- to avoid overflow and correctly determine the carry bit.
          let expected_full_sum = (fromIntegral a :: Integer) + (fromIntegral b :: Integer)
          let expected_sum = fromIntegral expected_full_sum :: Word16
          let expected_carry = expected_full_sum > fromIntegral (maxBound :: Word16)

          -- Assert that the Verilog results match the Haskell results
          verilog_sum `shouldBe` expected_sum
          verilog_carry `shouldBe` expected_carry

    -- Section 2: Edge case testing
    -- This section manually tests specific values that are often sources of bugs,
    -- such as zero, maximum values, and inputs that just barely cause a carry.
    context "Manual edge case tests" $ do
      it "handles adding zero to zero" $ do
        (carry, sum_res) <- adder 0 0
        sum_res `shouldBe` 0
        carry `shouldBe` False

      it "handles a simple addition without carry" $ do
        (carry, sum_res) <- adder 100 250
        sum_res `shouldBe` 350
        carry `shouldBe` False

      it "handles adding zero to the maximum value" $ do
        (carry, sum_res) <- adder maxBound 0
        sum_res `shouldBe` maxBound
        carry `shouldBe` False

      it "correctly generates a carry when wrapping around" $ do
        -- 65535 + 1 = 65536, which is 2^16.
        -- The sum should be 0, and the carry should be 1.
        (carry, sum_res) <- adder maxBound 1
        sum_res `shouldBe` 0
        carry `shouldBe` True

      it "handles adding the maximum value to itself" $ do
        -- 65535 + 65535 = 131070 = 1*65536 + 65534
        -- The sum should be 65534 (0xFFFE), and the carry should be 1.
        (carry, sum_res) <- adder maxBound maxBound
        sum_res `shouldBe` (maxBound - 1)
        carry `shouldBe` True

  describe "Bit manipulation" $ do
    -- Run the same set of tests for both implementations
    context "Haskell Implementation" $
      bitManipSpec (\a b c d -> return (bitManipHs a b c d))
    context "Verilog (Simulated) Implementation" $
      bitManipSpec bitManip

type BitManipFn = Word64 -> Word32 -> Word8 -> Word8 -> IO (Word64, Word32, Bool)

-- A generic test suite that can be run on any function of type BitManipFn.
bitManipSpec :: BitManipFn -> Spec
bitManipSpec bitManipFn = do
  -- Test Case 1: Baseline and AND Operation
  context "with shuffle_control=0 (no change) and op_select=0 (AND)" $ do
    let in_A   = 0xAAAAAAAAAAAAAAAA
    let in_B   = 0xFFFF0000
    (shuf, op, pop) <- runIO (bitManipFn in_A in_B 0 0)

    it "passes through in_A unchanged" $
      shuf `shouldBe` 0xAAAAAAAAAAAAAAAA

    it "performs a bitwise AND" $
      op `shouldBe` 0xAAAA0000

    it "correctly compares population counts (A > B)" $
      pop `shouldBe` True

  -- Test Case 2: Swap Halves and XOR Operation
  context "with shuffle_control=1 (swap halves) and op_select=2 (XOR)" $ do
    let in_A = 0x123456789ABCDEF0
    let in_B = 0xFFFFFFFF
    (shuf, op, pop) <- runIO (bitManipFn in_A in_B 1 2)

    it "swaps the upper and lower halves of in_A" $
      shuf `shouldBe` 0x9ABCDEF012345678

    it "performs a bitwise XOR" $
      op `shouldBe` 0x6543210F

    it "correctly compares population counts (A == B)" $
      pop `shouldBe` False

  -- Test Case 3: Bit Reversal and OR Operation
  context "with shuffle_control=2 (bit reverse) and op_select=1 (OR)" $ do
    let in_A = 0xF000000000000001
    let in_B = 0x0F0F0F0F
    (shuf, op, pop) <- runIO (bitManipFn in_A in_B 2 1)

    it "reverses the bits of in_A" $
      shuf `shouldBe` 0x800000000000000F

    it "performs a bitwise OR" $
      op `shouldBe` 0x0F0F0F0F

    it "correctly compares population counts (A < B)" $
      pop `shouldBe` False

  -- Test Case 4: Barrel Rotate and NOT Operation
  context "with shuffle_control=3 (rotate) and op_select=3 (NOT)" $ do
    let in_A = 0x0001000200030004
    let in_B = 0x00000000
    (shuf, op, pop) <- runIO (bitManipFn in_A in_B 3 3)

    it "rotates in_A left by 16 bits" $
      shuf `shouldBe` 0x0002000300040001

    it "performs a bitwise NOT (default case)" $
      op `shouldBe` 0xFFFCFFFB

    it "correctly compares population counts (A > B)" $
      pop `shouldBe` True
