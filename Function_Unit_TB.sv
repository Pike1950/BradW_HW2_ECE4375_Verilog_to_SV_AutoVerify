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

`timescale 1ns/1ps

module Function_Unit_TB;
  // ---- VCD dump ----
  initial begin
    $dumpfile("Function_Unit_TB.vcd");
    $dumpvars(0, Function_Unit_TB);
    $display("[%0t] VCD dumping to Function_Unit_TB.vcd", $time);
  end



  // ----------------------------
  // DUT I/O
  // ----------------------------
  logic        clk;
  logic        rst_n;

  logic [15:0] A;
  logic [15:0] B;

  // control
  logic [2:0]  S_ALU;
  logic [1:0]  S_SHF;
  logic        MF_SEL;  // 0 = ALU path, 1 = Shifter path
  logic        CIN;

  // DUT outputs
  logic [15:0] F;
  logic        V, C, N, Z;

  // ------------- Expected (golden) -------------
  /* verilator lint_off UNUSEDSIGNAL */
  logic [31:0] expF;          // extended for convenient viewing
  logic        expV, expC, expN, expZ;
  /* verilator lint_on UNUSEDSIGNAL */

  // One-cycle sampling alignment
  logic        sampleTick;   // toggles to define compare instants

  // ----------------------------
  // Instantiate DUT
  // ----------------------------
  Function_Unit dut (
    .A        (A),
    .B        (B),
    .S_ALU    (S_ALU),
    .S_SHF    (S_SHF),
    .MF_SEL   (MF_SEL),
    .CIN      (CIN),
    .F        (F),
    .V        (V),
    .C        (C),
    .N        (N),
    .Z        (Z)
  );

  // ----------------------------
  // Clock / reset
  // ----------------------------
  initial begin
    clk = 0;
    forever #5 clk = ~clk; // 100 MHz
  end

  initial begin
    rst_n = 0;
    sampleTick = 0;
    #12;
    rst_n = 1;
  end

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      sampleTick <= 0;
    else
      sampleTick <= ~sampleTick;
  end

  // ----------------------------
  // Opcodes (PascalCase for localparams)
  // ----------------------------
  // Arithmetic family packed as 4 bits: [3:1]=S_ALU, [0]=CIN
  localparam logic [3:0] TranA1    = 4'b0000;
  localparam logic [3:0] IncA      = 4'b0001;
  localparam logic [3:0] Add       = 4'b0010;
  localparam logic [3:0] AddCarry  = 4'b0011;
  localparam logic [3:0] Add1S     = 4'b0100; // A + ~B + 0
  localparam logic [3:0] Sub       = 4'b0101; // A + ~B + 1
  localparam logic [3:0] DecA      = 4'b0110;
  localparam logic [3:0] TranA2    = 4'b0111;

  // Logic opcodes (S_ALU only)
  localparam logic [2:0] AndOp     = 3'b100;
  localparam logic [2:0] OrOp      = 3'b101;
  localparam logic [2:0] XorOp     = 3'b110;
  localparam logic [2:0] NotOp     = 3'b111;

  // ----------------------------
  // Helpers
  // ----------------------------
  task automatic drive_opcode(input logic [3:0] opc);
    begin
      S_ALU = opc[3:1];
      CIN   = opc[0];
    end
  endtask

  function automatic void check_result(
    input string testname,
    input int    expected_F,
    input bit    expected_V,
    input bit    expected_C,
    input bit    expected_N,
    input bit    expected_Z
  );
    if ((F !== expected_F[15:0]) ||
        (V !== expected_V) ||
        (C !== expected_C) ||
        (N !== expected_N) ||
        (Z !== expected_Z)) begin
      $display("[%0t] ❌ FAIL: %s", $time, testname);
      $display("    Expected F=%0d V=%0b C=%0b N=%0b Z=%0b",
               expected_F, expected_V, expected_C, expected_N, expected_Z);
      $display("    Got      F=%0d V=%0b C=%0b N=%0b Z=%0b",
               F, V, C, N, Z);
    end
    else begin
      $display("[%0t] ✅ PASS: %s | F=%0d V=%0b C=%0b N=%0b Z=%0b",
               $time, testname, F, V, C, N, Z);
    end
  endfunction

  // ----------------------------
  // Golden ALU model (combinational)
  // ----------------------------
  function automatic void golden_alu(
    input  logic [15:0] a,
    input  logic [15:0] b,
    input  logic [2:0]  s_alu,
    input  logic        cin,
    output logic [15:0] f_out,
    output logic        v_out,
    output logic        c_out,
    output logic        n_out,
    output logic        z_out
  );
    logic [16:0] tmp17;
    logic [15:0] b_eff;

    f_out = 16'h0000; v_out = 1'b0; c_out = 1'b0;

    unique casez (s_alu)
      3'b000,3'b001,3'b010,3'b011: begin
        unique case ({s_alu, cin})
          {3'b000,1'b0}: begin // TranA1
            f_out = a; c_out = 1'b0; v_out = 1'b0;
          end
          {3'b000,1'b1}: begin // IncA
            tmp17 = {1'b0,a} + 17'd1;
            f_out = tmp17[15:0]; c_out = tmp17[16];
            v_out = (~a[15] &  f_out[15]); // add 1 overflow
          end
          {3'b001,1'b0}: begin // Add
            tmp17 = {1'b0,a} + {1'b0,b};
            f_out = tmp17[15:0]; c_out = tmp17[16];
            v_out = (~a[15] & ~b[15] &  f_out[15]) |
                    ( a[15] &  b[15] & ~f_out[15]);
          end
          {3'b001,1'b1}: begin // AddCarry
            tmp17 = {1'b0,a} + {1'b0,b} + 17'd1;
            f_out = tmp17[15:0]; c_out = tmp17[16];
            v_out = (~a[15] & ~b[15] &  f_out[15]) |
                    ( a[15] &  b[15] & ~f_out[15]);
          end
          {3'b010,1'b0}: begin // Add1S (A - B - 1)
            b_eff = ~b;
            tmp17 = {1'b0,a} + {1'b0,b_eff};
            f_out = tmp17[15:0]; c_out = tmp17[16];
            v_out = (~a[15] & ~b_eff[15] &  f_out[15]) |
                    ( a[15] &  b_eff[15] & ~f_out[15]);
          end
          {3'b010,1'b1}: begin // Sub (A - B)
            b_eff = ~b;
            tmp17 = {1'b0,a} + {1'b0,b_eff} + 17'd1;
            f_out = tmp17[15:0]; c_out = tmp17[16]; // carry=1 means no borrow
            v_out = (~a[15] & ~b_eff[15] &  f_out[15]) |
                    ( a[15] &  b_eff[15] & ~f_out[15]);
          end
          {3'b011,1'b0}: begin // DecA (A - 1)
            tmp17 = {1'b0,a} + 17'h1_FFFF;
            f_out = tmp17[15:0]; c_out = tmp17[16];
            v_out = (a == 16'h8000);
          end
          {3'b011,1'b1}: begin // TranA2
            f_out = a; c_out = 1'b0; v_out = 1'b0;
          end
          default: begin f_out=16'h0000; c_out=1'b0; v_out=1'b0; end
        endcase
      end

      3'b100: begin f_out = a & b; c_out=1'b0; v_out=1'b0; end
      3'b101: begin f_out = a | b; c_out=1'b0; v_out=1'b0; end
      3'b110: begin f_out = a ^ b; c_out=1'b0; v_out=1'b0; end
      3'b111: begin f_out = ~a;    c_out=1'b0; v_out=1'b0; end
      default: begin f_out=16'h0000; c_out=1'b0; v_out=1'b0; end
    endcase

    n_out = f_out[15];
    z_out = (f_out == 16'h0000);
  endfunction

  // golden shifter model (operates on A, flags: V=0 C=0 N=MSB(F) Z=(F==0))
  function automatic void golden_shifter(
    input  logic [15:0] a,
    input  logic [1:0]  s_shf,
    output logic [15:0] f_out,
    output logic        v_out,
    output logic        c_out,
    output logic        n_out,
    output logic        z_out
  );
    logic signed [15:0] as;
    as = a;
    unique case (s_shf)
      2'b00: f_out = a;
      2'b01: f_out = a << 1;
      2'b10: f_out = a >> 1;
      2'b11: f_out = as >>> 1;
      default: f_out = '0;
    endcase
    v_out = 1'b0;
    c_out = 1'b0;
    n_out = f_out[15];
    z_out = (f_out == 16'h0000);
  endfunction


  // compare pack
  typedef struct packed { logic [15:0] f; logic v,c,n,z; } exp_t;
  exp_t exp_q;

  task automatic compute_expected(input string tag);
    logic [15:0] gf; logic gv,gc,gn,gz;
    if (MF_SEL) begin
      // Shifter path expected values (A operand)
      golden_shifter(A,S_SHF,gf,gv,gc,gn,gz);
    end else begin
      // ALU path expected values
      golden_alu(A,B,S_ALU,CIN,gf,gv,gc,gn,gz);
    end
    exp_q.f = gf; exp_q.v = gv; exp_q.c = gc; exp_q.n = gn; exp_q.z = gz;
    expF = {16'h0000,gf}; expV=gv; expC=gc; expN=gn; expZ=gz;
    $display("[%0t] Computing expected for %s", $time, tag);
  endtask


  // Compare every other cycle (when sampleTick==1)
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin end
    else if (sampleTick) begin
      check_result("sample", int'(exp_q.f), exp_q.v, exp_q.c, exp_q.n, exp_q.z);
    end
  end

  // ------------------------------------------------------------
  // Small helper task for logic ops (declared at TB scope — no nesting)
  // ------------------------------------------------------------
  task automatic logic_case(
    input logic [15:0] a_i,
    input logic [15:0] b_i,
    input logic [2:0]  op,
    input string       tag
  );
    A = a_i; B = b_i; S_ALU = op; CIN = 1'b0;
    @(posedge clk); compute_expected(tag); @(posedge clk);
  endtask

  // ----------------------------
  // Test sequencing
  // ----------------------------
  task automatic do_shift_tests;
    $display("\n[ %0t ] --- PHASE 1: SHIFT TESTS ---", $time);

    MF_SEL = 1'b1; S_SHF = 2'b00; // pass-through via shifter

    A = 16'hF00F; B = 16'h0000; S_ALU = 3'b000; CIN = 1'b0;
    @(posedge clk); compute_expected("SHF PASS"); @(posedge clk);

    B = 16'hEFFF; S_SHF = 2'b01; // SHL
    @(posedge clk); compute_expected("SHL"); @(posedge clk);

    B = 16'h00F0; S_SHF = 2'b10; // SHR
    @(posedge clk); compute_expected("SHR"); @(posedge clk);

    B = 16'h8001; S_SHF = 2'b11; // SRA
    @(posedge clk); compute_expected("SRA"); @(posedge clk);
  endtask

  task automatic do_alu_arith_tests;
    $display("\n[ %0t ] --- PHASE 2: ALU ARITHMETIC TESTS ---", $time);

    MF_SEL = 1'b0; S_SHF = 2'b00;

    // Case 0: A=0, B=0
    A = 16'h0000; B = 16'h0000;
    drive_opcode(Add);      @(posedge clk); compute_expected("ADD Case 0"); @(posedge clk);
    drive_opcode(AddCarry); @(posedge clk); compute_expected("ADD+Carry Case 0"); @(posedge clk);
    drive_opcode(Sub);      @(posedge clk); compute_expected("SUB Case 0"); @(posedge clk);
    drive_opcode(Add1S);    @(posedge clk); compute_expected("SUB+Borrow Case 0"); @(posedge clk);
    drive_opcode(IncA);     @(posedge clk); compute_expected("INC A Case 0"); @(posedge clk);
    drive_opcode(DecA);     @(posedge clk); compute_expected("DEC A Case 0"); @(posedge clk);
    drive_opcode(TranA1);   @(posedge clk); compute_expected("TRAN_A1 Case 0"); @(posedge clk);
    drive_opcode(TranA2);   @(posedge clk); compute_expected("TRAN_A2 Case 0"); @(posedge clk);

    // Case 1: A=0x7FFF, B=0x0001
    A = 16'h7FFF; B = 16'h0001;
    drive_opcode(Add);      @(posedge clk); compute_expected("ADD Case 1"); @(posedge clk);
    drive_opcode(AddCarry); @(posedge clk); compute_expected("ADD+Carry Case 1"); @(posedge clk);
    drive_opcode(Sub);      @(posedge clk); compute_expected("SUB Case 1"); @(posedge clk);
    drive_opcode(Add1S);    @(posedge clk); compute_expected("SUB+Borrow Case 1"); @(posedge clk);
    drive_opcode(IncA);     @(posedge clk); compute_expected("INC A Case 1"); @(posedge clk);
    drive_opcode(DecA);     @(posedge clk); compute_expected("DEC A Case 1"); @(posedge clk);
    drive_opcode(TranA1);   @(posedge clk); compute_expected("TRAN_A1 Case 1"); @(posedge clk);
    drive_opcode(TranA2);   @(posedge clk); compute_expected("TRAN_A2 Case 1"); @(posedge clk);

    // Case 2: A=0xFFFF, B=0x0001
    A = 16'hFFFF; B = 16'h0001;
    drive_opcode(Add);      @(posedge clk); compute_expected("ADD Case 2"); @(posedge clk);
    drive_opcode(AddCarry); @(posedge clk); compute_expected("ADD+Carry Case 2"); @(posedge clk);
    drive_opcode(Sub);      @(posedge clk); compute_expected("SUB Case 2"); @(posedge clk);
    drive_opcode(Add1S);    @(posedge clk); compute_expected("SUB+Borrow Case 2"); @(posedge clk);
  endtask

  task automatic do_alu_logic_tests;
    $display("\n[ %0t ] --- PHASE 3: ALU LOGIC TESTS ---", $time);
    MF_SEL = 1'b0;

    // Case 2: A=0xFFFF, B=0x0001
    logic_case(16'hFFFF, 16'h0001, AndOp, "AND Case 0");
    logic_case(16'hFFFF, 16'h0001, OrOp , "OR  Case 0");
    logic_case(16'hFFFF, 16'h0001, XorOp, "XOR Case 0");
    logic_case(16'hFFFF, 16'h0001, NotOp, "NOT Case 0");
    drive_opcode(IncA);  @(posedge clk); compute_expected("INC A Case 2"); @(posedge clk);
    drive_opcode(DecA);  @(posedge clk); compute_expected("DEC A Case 2"); @(posedge clk);
    drive_opcode(TranA1);@(posedge clk); compute_expected("TRAN_A1 Case 2"); @(posedge clk);
    drive_opcode(TranA2);@(posedge clk); compute_expected("TRAN_A2 Case 2"); @(posedge clk);

    // Case 3: A=0x7FFF, B=0x8001
    logic_case(16'h7FFF, 16'h8001, AndOp, "AND Case 1");
    logic_case(16'h7FFF, 16'h8001, OrOp , "OR  Case 1");
    logic_case(16'h7FFF, 16'h8001, XorOp, "XOR Case 1");
    logic_case(16'h7FFF, 16'h8001, NotOp, "NOT Case 1");
    drive_opcode(Add);      @(posedge clk); compute_expected("ADD Case 3"); @(posedge clk);
    drive_opcode(AddCarry); @(posedge clk); compute_expected("ADD+Carry Case 3"); @(posedge clk);
    drive_opcode(Sub);      @(posedge clk); compute_expected("SUB Case 3"); @(posedge clk);
    drive_opcode(Add1S);    @(posedge clk); compute_expected("SUB+Borrow Case 3"); @(posedge clk);
    drive_opcode(IncA);     @(posedge clk); compute_expected("INC A Case 3"); @(posedge clk);
    drive_opcode(DecA);     @(posedge clk); compute_expected("DEC A Case 3"); @(posedge clk);
    drive_opcode(TranA1);   @(posedge clk); compute_expected("TRAN_A1 Case 3"); @(posedge clk);
    drive_opcode(TranA2);   @(posedge clk); compute_expected("TRAN_A2 Case 3"); @(posedge clk);

    // Case 4: A=12345, B=0xE3A6
    logic_case(16'd12345, 16'hE3A6, AndOp, "AND Case 4");
    logic_case(16'd12345, 16'hE3A6, OrOp , "OR  Case 4");
    logic_case(16'd12345, 16'hE3A6, XorOp, "XOR Case 4");
    logic_case(16'd12345, 16'hE3A6, NotOp, "NOT Case 4");
    drive_opcode(Add);      @(posedge clk); compute_expected("ADD Case 4"); @(posedge clk);
    drive_opcode(AddCarry); @(posedge clk); compute_expected("ADD+Carry Case 4"); @(posedge clk);
    drive_opcode(Sub);      @(posedge clk); compute_expected("SUB Case 4"); @(posedge clk);
    drive_opcode(Add1S);    @(posedge clk); compute_expected("SUB+Borrow Case 4"); @(posedge clk);
    drive_opcode(IncA);     @(posedge clk); compute_expected("INC A Case 4"); @(posedge clk);
    drive_opcode(DecA);     @(posedge clk); compute_expected("DEC A Case 4"); @(posedge clk);
    drive_opcode(TranA1);   @(posedge clk); compute_expected("TRAN_A1 Case 4"); @(posedge clk);
    drive_opcode(TranA2);   @(posedge clk); compute_expected("TRAN_A2 Case 4"); @(posedge clk);
  endtask

  // ----------------------------
  // Top-level stimulus
  // ----------------------------
  initial begin
    $display("\n----------------------------------------------------------");
    $display("     FUNCTION UNIT TESTBENCH - BORROW & OVERFLOW CHECK    ");
    $display("----------------------------------------------------------\n");

    // init
    A=16'h0000; B=16'h0000;
    S_ALU=3'b000; S_SHF=2'b00; CIN=1'b0; MF_SEL=1'b0;
    expF=32'h0; expV=0; expC=0; expN=0; expZ=0;

    @(posedge rst_n);
    @(posedge clk);

    do_shift_tests();
    do_alu_arith_tests();
    do_alu_logic_tests();

    $display("\n[ %0t ] --- TESTBENCH COMPLETE ---", $time);
    $finish;
  end

endmodule
`default_nettype none
