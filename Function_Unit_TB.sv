`timescale 1ns/1ps
//////////////////////////////////////////////////////////////////////////////////
// Engineer: Bradley Ward
// File:     Function_Unit_TB.sv
// Purpose:  Self-checking testbench for Function_Unit (ALU + Shifter muxed to F)
// Notes:
//   - ALU opcodes in RTL are 4-bit, with LSB as CIN (carry/borrow control).
//   - Testbench drives S_ALU = opcode[3:1] and CIN = opcode[0].
//   - Subtract path uses borrow = ~carry_out convention (rubric).
//////////////////////////////////////////////////////////////////////////////////

module Function_Unit_TB;
import Function_Unit_pkg::*; // <- bring in exp_t + opcode constants + models

  localparam int WIDTH = 16;

  // DUT I/O
  logic                 clk;
  logic [WIDTH-1:0]     A, B;
  logic                 MF_SEL;
  logic [2:0]           S_ALU;
  logic [1:0]           S_SHF;
  logic                 CIN;
  wire  [WIDTH-1:0]     F;
  wire                  V, C, N, Z;

  // Expected bundle from the package
  exp_t                 exp_q;

  // Instantiate DUT
  Function_Unit dut (
    .A      (A),
    .B      (B),
    .MF_SEL (MF_SEL),
    .S_ALU  (S_ALU),
    .S_SHF  (S_SHF),
    .CIN    (CIN),
    .F      (F),
    .V      (V),
    .C      (C),
    .N      (N),
    .Z      (Z)
  );

  // Clock
  initial clk = 1'b0;
  always #10 clk = ~clk;    // 20 ns period

  // VCD (unguarded, always on)
  initial begin
    $dumpfile("Function_Unit_TB.vcd");
    $dumpvars(0, Function_Unit_TB);
  end
  // -------------------------------
  // Pretty banner helper
  // -------------------------------
  task automatic banner(string s);
    $display("\n[ %0t ] --- %s ---", $time, s);
  endtask

  // -------------------------------
  // Your result checker (unchanged)
  // -------------------------------
  task automatic check_result(
    input string   tag,
    input int      expF,
    input logic    expV,
    input logic    expC,
    input logic    expN,
    input logic    expZ
  );
    if ( {V, C, N, Z, F} === {expV, expC, expN, expZ, expF[15:0]} ) begin
    $display("[%0t] ✅ PASS: %s | F=%0d V=%0d C=%0d N=%0d Z=%0d",
             $time, tag, F, V, C, N, Z);
    end else begin
    $display("[%0t] ❌ FAIL: %s", $time, tag);
    $display("    Expected F=%0d V=%0d C=%0d N=%0d Z=%0d",
             expF, expV, expC, expN, expZ);
    $display("    Got      F=%0d V=%0d C=%0d N=%0d Z=%0d",
             F, V, C, N, Z);
    // Make exp_q “used” and helpful in debug
    $display("    Golden   F=%0d V=%0d C=%0d N=%0d Z=%0d",
             exp_q.f, exp_q.v, exp_q.c, exp_q.n, exp_q.z);
  end
endtask

  // ------------------------------------------------------------
  // compute_expected(tag)
  // Now calls the package golden models (model_alu/model_shf),
  // keeps exp_q for debugging, and uses your checker/prints.
  // ------------------------------------------------------------
  task automatic compute_expected(input string tag);
    exp_t e;
    begin
      if (MF_SEL) begin
        e = model_shf(A, S_SHF);              // shifter model
      end else begin
        e = model_alu(A, B, S_ALU, CIN);      // ALU model
      end
      exp_q = e; // keep a copy if you want to probe in waveforms

      // Use your standard checker for the PASS/FAIL line
      check_result(tag, int'(e.f), e.v, e.c, e.n, e.z);
    end
  endtask

  // ------------------------------------------------------------
  // Helpers preserving cadence:
  // drive → @(posedge clk) → compute_expected(tag) → @(posedge clk)
  // ------------------------------------------------------------
  task automatic logic_case(
  input logic [15:0] a_i, input logic [15:0] b_i,
  input logic [2:0]  op,  input string       tag
  );
    MF_SEL = 1'b0;
    S_ALU  = op;
    S_SHF  = 2'b00;
    CIN    = 1'b0;
    A      = a_i;
    B      = b_i;

    @(posedge clk);
    compute_expected(tag);
    @(posedge clk);
  endtask

  task automatic shift_case(
  input logic [15:0] a_i,
  input logic [1:0]  shf,
  input string       tag
  );
    MF_SEL = 1'b1;
    S_SHF  = shf;
    S_ALU  = 3'b000;   // don't care; make deterministic
    CIN    = 1'b0;
    A      = a_i;
    B      = '0;

    @(posedge clk);
    compute_expected(tag);
    @(posedge clk);
  endtask

  // Drive an ALU op (MF_SEL=0) with optional carry-in, then check
  task automatic arith_case(
  input  logic [15:0] a_i,
  input  logic [15:0] b_i,
  /* verilator lint_off UNUSEDSIGNAL */
  input  logic [3:0]  op4,     // <— widened to 4 bits
  /* verilator lint_on UNUSEDSIGNAL */
  input  logic        cin_i,
  input  string       tag
  );
  // Drive DUT
  MF_SEL = 1'b0;               // select ALU
  S_ALU  = op4[2:0];           // <— explicit, no width warning
  S_SHF  = 2'b00;
  CIN    = cin_i;
  A      = a_i;
  B      = b_i;

  @(posedge clk);
  compute_expected(tag);
  @(posedge clk);
  endtask

  // ------------------------------------------------------------
  // Test sequence: mirrors your three phases and prints
  // (If you have an existing sequence you like, paste it here.)
  // Using named opcodes from the package improves readability.
  // ------------------------------------------------------------
  initial begin
    // init
    A=0; B=0; MF_SEL=0; S_ALU=0; S_SHF=0; CIN=0;

    $display("----------------------------------------------------------");
    $display("     FUNCTION UNIT TESTBENCH - BORROW & OVERFLOW CHECK    ");
    $display("----------------------------------------------------------\n");

    // PHASE 1: SHIFT TESTS
    banner("PHASE 1: SHIFT TESTS");

    $display("[%0t] Computing expected for SHF PASS", $time);
    shift_case(16'hF00F, ShfPass, "SHF PASS");

    $display("[%0t] Computing expected for SHL", $time);
    shift_case(16'hEFFF, ShiftL,  "SHL");

    $display("[%0t] Computing expected for SHR", $time);
    shift_case(16'hF06F, ShiftR,  "SHR");

    $display("[%0t] Computing expected for SRA", $time);
    shift_case(16'hF87F, ShiftRA, "SRA");

    // PHASE 2: ALU ARITHMETIC TESTS
    banner("PHASE 2: ALU ARITHMETIC TESTS");

    // --- Basic ADD / ADD+Carry cases ---
    $display("[%0t] Computing expected for ADD Case 0", $time);
    arith_case(16'h0000, 16'h0000, Add, 1'b0, "ADD Case 0");

    $display("[%0t] Computing expected for ADD+Carry Case 0", $time);
    arith_case(16'h0000, 16'h0000, AddCarry, 1'b1, "ADD+Carry Case 0");

    $display("[%0t] Computing expected for ADD Carry-out (FFFF + 1)", $time);
    arith_case(16'hFFFF, 16'h0001, Add, 1'b0, "ADD Carry-out (FFFF + 1)");

    $display("[%0t] Computing expected for ADD Overflow + (7FFF + 1)", $time);
    arith_case(16'h7FFF, 16'h0001, Add, 1'b0, "ADD Overflow + (7FFF + 1)");

    $display("[%0t] Computing expected for ADD Overflow - (8000 + 8000)", $time);
    arith_case(16'h8000, 16'h8000, Add, 1'b0, "ADD Overflow - (8000 + 8000)");


    // --- SUB / SUB+Borrow cases ---
    $display("[%0t] Computing expected for SUB Case 0", $time);
    arith_case(16'h0000, 16'h0000, Sub, 1'b0, "SUB Case 0");

    $display("[%0t] Computing expected for SUB+Borrow Case 0", $time);
    arith_case(16'h0000, 16'h0001, Sub, 1'b0, "SUB+Borrow Case 0");

    $display("[%0t] Computing expected for SUB Negative (8000 - 1)", $time);
    arith_case(16'h8000, 16'h0001, Sub, 1'b0, "SUB Negative (8000 - 1)");

    $display("[%0t] Computing expected for SUB Overflow + (7FFF - FFFF)", $time);
    arith_case(16'h7FFF, 16'hFFFF, Sub, 1'b0, "SUB Overflow + (7FFF - FFFF)");

    $display("[%0t] Computing expected for SUB Overflow - (8000 - 7FFF)", $time);
    arith_case(16'h8000, 16'h7FFF, Sub, 1'b0, "SUB Overflow - (8000 - 7FFF)");

    $display("[%0t] Computing expected for SUB Alternating (AAAA - 5555)", $time);
    arith_case(16'hAAAA, 16'h5555, Sub, 1'b0, "SUB Alternating (AAAA - 5555)");


    // --- INC / DEC / Transfer cases ---
    $display("[%0t] Computing expected for INC A Case 0", $time);
    arith_case(16'h0000, 16'h0000, IncA, 1'b0, "INC A Case 0");

    $display("[%0t] Computing expected for INC A Wraparound (FFFF -> 0000)", $time);
    arith_case(16'hFFFF, 16'h0000, IncA, 1'b0, "INC A Wraparound (FFFF -> 0000)");

    $display("[%0t] Computing expected for DEC A Case 0", $time);
    arith_case(16'h0000, 16'h0000, DecA, 1'b0, "DEC A Case 0");

    $display("[%0t] Computing expected for DEC A to Zero (0001 -> 0000)", $time);
    arith_case(16'h0001, 16'h0000, DecA, 1'b0, "DEC A to Zero (0001 -> 0000)");

    $display("[%0t] Computing expected for TRAN_A1 Case 0", $time);
    arith_case(16'h0000, 16'h0000, TranA1, 1'b0, "TRAN_A1 Case 0");

    $display("[%0t] Computing expected for TRAN_A2 Case 0", $time);
    arith_case(16'h7FFF, 16'h0000, TranA2, 1'b0, "TRAN_A2 Case 0");

    $display("[%0t] Computing expected for TRAN_A1 Negative (8000 -> 8000)", $time);
    arith_case(16'h8000, 16'h0000, TranA1, 1'b0, "TRAN_A1 Negative (8000 -> 8000)");

    // PHASE 3: ALU LOGIC TESTS
    banner("PHASE 3: ALU LOGIC TESTS");

    // --- Zeros / Ones sanity checks ---
    $display("[%0t] Computing expected for AND 0000 & 0000", $time);
    logic_case(16'h0000, 16'h0000, AndOp, "AND 0000&0000 -> 0000 (Z=1)");

    $display("[%0t] Computing expected for AND FFFF & FFFF", $time);
    logic_case(16'hFFFF, 16'hFFFF, AndOp, "AND FFFF&FFFF -> FFFF (N=1)");

    $display("[%0t] Computing expected for OR  0000 | FFFF", $time);
    logic_case(16'h0000, 16'hFFFF, OrOp,  "OR  0000|FFFF -> FFFF (N=1)");

    $display("[%0t] Computing expected for XOR FFFF ^ FFFF", $time);
    logic_case(16'hFFFF, 16'hFFFF, XorOp, "XOR FFFF^FFFF -> 0000 (Z=1)");

    // --- Alternating patterns / bit emphasis ---
    $display("[%0t] Computing expected for AND AAAA & AAAA", $time);
    logic_case(16'hAAAA, 16'hAAAA, AndOp, "AND AAAA&AAAA -> AAAA");

    $display("[%0t] Computing expected for OR  5555 | 5555", $time);
    logic_case(16'h5555, 16'h5555, OrOp,  "OR  5555|5555 -> 5555");

    $display("[%0t] Computing expected for XOR AAAA ^ 5555", $time);
    logic_case(16'hAAAA, 16'h5555, XorOp, "XOR AAAA^5555 -> FFFF");

    // --- MSB / LSB specific ---
    $display("[%0t] Computing expected for OR  MSB|LSB (8000 | 0001)", $time);
    logic_case(16'h8000, 16'h0001, OrOp,  "OR  8000|0001 -> 8001 (N=1)");

    $display("[%0t] Computing expected for AND keep MSB (8000 & 8000)", $time);
    logic_case(16'h8000, 16'h8000, AndOp, "AND 8000&8000 -> 8000 (N=1)");

    $display("[%0t] Computing expected for AND keep LSB (0001 & 0001)", $time);
    logic_case(16'h0001, 16'h0001, AndOp, "AND 0001&0001 -> 0001");

    // --- Equal / disjoint sets ---
    $display("[%0t] Computing expected for XOR A==B (1234 ^ 1234)", $time);
    logic_case(16'h1234, 16'h1234, XorOp, "XOR 1234^1234 -> 0000 (Z=1)");

    $display("[%0t] Computing expected for AND disjoint (0F0F & F0F0)", $time);
    logic_case(16'h0F0F, 16'hF0F0, AndOp, "AND 0F0F&F0F0 -> 0000 (Z=1)");

    $display("[%0t] Computing expected for OR  disjoint (0F0F | F0F0)", $time);
    logic_case(16'h0F0F, 16'hF0F0, OrOp,  "OR  0F0F|F0F0 -> FFFF (N=1)");

    // --- NOT (B is ignored by model/RTL) ---
    $display("[%0t] Computing expected for NOT 0000", $time);
    logic_case(16'h0000, 16'h0000, NotOp, "NOT 0000 -> FFFF (N=1)");

    $display("[%0t] Computing expected for NOT FFFF", $time);
    logic_case(16'hFFFF, 16'h0000, NotOp, "NOT FFFF -> 0000 (Z=1)");

    $display("[%0t] Computing expected for NOT 8000", $time);
    logic_case(16'h8000, 16'h0000, NotOp, "NOT 8000 -> 7FFF");

    $display("[%0t] Computing expected for NOT 7FFF", $time);
    logic_case(16'h7FFF, 16'h0000, NotOp, "NOT 7FFF -> 8000 (N=1)");
    // NOT uses only A; B is don't-care
    MF_SEL = 1'b0; S_ALU = NotOp; CIN = 1'b0; A = 16'h0000; B = '0;
    @(posedge clk); compute_expected("NOT Case 0"); @(posedge clk);

    // …add remaining cases you already had…

    $display("\n[ %0t ] --- TESTBENCH COMPLETE ---", $time);
    #10 $finish;
  end

endmodule
`default_nettype none
