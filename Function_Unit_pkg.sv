// Function_Unit_pkg.sv
// Shared types, opcodes, and golden reference models for Function_Unit verification.
// This package is *pure*: it never touches DUT/TB signals. All inputs are explicit.
// Import with:  import Function_Unit_pkg::*;

`timescale 1ns/1ps

package Function_Unit_pkg;

  // ----------------------------
  // Types
  // ----------------------------
  typedef struct packed {
    logic [15:0] f;
    logic        v;
    logic        c;
    logic        n;
    logic        z;
  } exp_t;

  /* verilator lint_off UNUSEDPARAM */
  // ----------------------------
  // Shared opcode encodings
  // These mirror ALU.sv (ALU_SEL = {S_ALU, CIN})
  // ----------------------------
  localparam logic [3:0] TranA1    = 4'b0000;
  localparam logic [3:0] IncA      = 4'b0001;
  localparam logic [3:0] Add       = 4'b0010;
  localparam logic [3:0] AddCarry  = 4'b0011;
  localparam logic [3:0] Add1S     = 4'b0100; // A + ~B + 0
  localparam logic [3:0] Sub       = 4'b0101; // A + ~B + 1
  localparam logic [3:0] DecA      = 4'b0110;
  localparam logic [3:0] TranA2    = 4'b0111;

  // Logic opcodes (S_ALU only; CIN ignored)
  localparam logic [2:0] AndOp     = 3'b100;
  localparam logic [2:0] OrOp      = 3'b101;
  localparam logic [2:0] XorOp     = 3'b110;
  localparam logic [2:0] NotOp     = 3'b111;

  // Shifter opcodes (S_SHF)
  localparam logic [1:0] ShfPass   = 2'b00;
  localparam logic [1:0] ShiftL    = 2'b01;
  localparam logic [1:0] ShiftR    = 2'b10;
  localparam logic [1:0] ShiftRA   = 2'b11;
  /* verilator lint_on UNUSEDPARAM */

  // ------------------------------------------------------------
  // Golden model: ALU
  // This matches the encodings used in ALU.sv (per your uploaded file)
  // Inputs: a, b, s_alu (3b), cin
  // Output: exp_t (f,v,c,n,z)
  // ------------------------------------------------------------
  function automatic exp_t model_alu(
    input logic [15:0] a,
    input logic [15:0] b,
    input logic [2:0]  s_alu,
    input logic        cin
  );
    exp_t e;
    logic [16:0] tmp17;
    logic [15:0] f_out;
    logic v_out, c_out, n_out, z_out;

    unique case ({s_alu, cin})
      // Transfers / INC / DEC
      {3'b000,1'b0}: begin // TranA1
        f_out = a;      c_out = 1'b0; v_out = 1'b0;
      end
      {3'b000,1'b1}: begin // IncA (A + 1)
        tmp17 = {1'b0,a} + 17'd1;
        f_out = tmp17[15:0]; c_out = tmp17[16];
        v_out = (a == 16'h7FFF);
      end
      {3'b001,1'b0}: begin // Add (A + B)
        tmp17 = {1'b0,a} + {1'b0,b};
        f_out = tmp17[15:0]; c_out = tmp17[16];
        v_out = (~a[15] & ~b[15] &  f_out[15]) |
                ( a[15] &  b[15] & ~f_out[15]);
      end
      {3'b001,1'b1}: begin // AddCarry (A + B + 1)
        tmp17 = {1'b0,a} + {1'b0,b} + 17'd1;
        f_out = tmp17[15:0]; c_out = tmp17[16];
        v_out = (~a[15] & ~b[15] &  f_out[15]) |
                ( a[15] &  b[15] & ~f_out[15]);
      end
      {3'b010,1'b0}: begin // Add1S (A + ~B + 0)
        logic [15:0] b_eff = ~b;
        tmp17 = {1'b0,a} + {1'b0,b_eff};
        f_out = tmp17[15:0]; c_out = tmp17[16];
        v_out = (~a[15] & ~b_eff[15] &  f_out[15]) |
                ( a[15] &  b_eff[15] & ~f_out[15]);
      end
      {3'b010,1'b1}: begin // Sub (A + ~B + 1)
        logic [15:0] b_eff = ~b;
        tmp17 = {1'b0,a} + {1'b0,b_eff} + 17'd1;
        f_out = tmp17[15:0]; c_out = tmp17[16]; // carry=1 => no borrow
        v_out = (~a[15] & ~b_eff[15] &  f_out[15]) |
                ( a[15] &  b_eff[15] & ~f_out[15]);
      end
      {3'b011,1'b0}: begin // DecA (A - 1)
        tmp17 = {1'b0,a} + 17'h1_FFFF;
        f_out = tmp17[15:0]; c_out = tmp17[16];
        v_out = (a == 16'h8000);
      end
      {3'b011,1'b1}: begin // TranA2
        f_out = a;      c_out = 1'b0; v_out = 1'b0;
      end
      default: begin
        // Logic ops (CIN is don't-care)
        unique case (s_alu)
          AndOp: begin f_out = a & b; c_out = 1'b0; v_out = 1'b0; end
          OrOp : begin f_out = a | b; c_out = 1'b0; v_out = 1'b0; end
          XorOp: begin f_out = a ^ b; c_out = 1'b0; v_out = 1'b0; end
          NotOp: begin f_out = ~a;    c_out = 1'b0; v_out = 1'b0; end
          default: begin f_out = 'x; c_out = 1'b0; v_out = 1'b0; end
        endcase
      end
    endcase

    n_out = f_out[15];
    z_out = (f_out == 16'h0000);

    e.f = f_out; e.v = v_out; e.c = c_out; e.n = n_out; e.z = z_out;
    return e;
  endfunction

  // ------------------------------------------------------------
  // Golden model: Shifter
  // Matches Shifter.sv behavior (with flags matching TB expectations)
  // Inputs: a (operand), s_shf
  // ------------------------------------------------------------
  function automatic exp_t model_shf(
    input logic [15:0] a,
    input logic [1:0]  s_shf
  );
    exp_t e;
    logic [15:0] h;

    unique case (s_shf)
      ShfPass: h = a;
      ShiftL : h = a << 1;
      ShiftR : h = a >> 1;
      ShiftRA: h = $signed(a) >>> 1;
      default: h = 'x;
    endcase

    // TB expects: V=0; C=0; N=MSB; Z=(h==0)
    e.f = h;
    e.v = 1'b0;
    e.c = 1'b0;
    e.n = h[15];
    e.z = (h == 16'h0000);
    return e;
  endfunction

endpackage : Function_Unit_pkg
