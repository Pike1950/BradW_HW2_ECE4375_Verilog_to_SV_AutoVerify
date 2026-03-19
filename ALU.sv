//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer: Bradley Ward
//
// Create Date:     01/31/2020 01:28 PM
// Updated Date:    10/26/2025 1:31 PM
// Design Name:
// Module Name:     ALU
// Project Name:    HW2 Tiny Processor
// Target Devices:
// Tool Versions:
// Description:     Simulation of a Function Unit of a simple processor which includes the logic for an Arithmetic/Logic
//                  Unit and a Shifter module with a multiplexer to togger either ALU or shifter outputs. ALU contains 8
//                  arithmetic operations and 4 logical operations controlled by select bits G, CIN, and MODE_SEL. Arithmetic
//                  operations include add, subtract, increment, decrement, transfer, and all of these with a borrow. Logic
//                  operations include AND, OR, XOR, and NOT. Shifter has three operations with passthrough, shift left, and
//                  shift right.
//
// Dependencies:
//
// Revision:
//                  Revision 0.01 - File Created
//                  Revision 0.02 - Fixed wiring for the carry and overflow bits to match correct math operations when select bits
//                                  are applied
//                  Revision 0.03 - Combined Original ALU and Shifter modules into a single ALU module. Must use a multiplexer to toggle
//                                  between shifter values and ALU values based on MODE_SEL input.
//                  Revision 0.04 - Updated ADD_V and SUB_V assignments to fix V flag computation
//////////////////////////////////////////////////////////////////////////////////
`timescale 1ns/1ps
`default_nettype none

module ALU #(
            // ────────────────────────────────────────────
            // PARAMETERIZATION BLOCK
            // ────────────────────────────────────────────
            // Declares compile-time constants configurable at instantiation.
            // 'int unsigned' means this is a non-negative integer constant.
            parameter int unsigned  WIDTH = 16
            // Now anywhere we say [WIDTH-1:0], this will mean [15:0] by default.
            )
        (
            // ────────────────────────────────────────────
            // PORT LIST
            // ────────────────────────────────────────────
            input   logic   [2:0]   S_ALU,   // {S2,S1,S0}
            input   logic   [WIDTH-1:0]  A,
            input   logic   [WIDTH-1:0]  B,
            input   logic           CIN,
            output  logic           V, C, N, Z,
            output  logic   [WIDTH-1:0]  G
        );

    // ────────────────────────────────────────────
    // OPCODES (Camel/PascalCase for consistency)
    // ────────────────────────────────────────────
    // 4-bit opcodes where LSB is CIN in arithmetic family
    localparam logic[3:0] TranA1   = 4'b0000;
    localparam logic[3:0] TranA2   = 4'b0111;
    localparam logic[3:0] IncA     = 4'b0001;
    localparam logic[3:0] DecA     = 4'b0110;
    localparam logic[3:0] Add      = 4'b0010;
    localparam logic[3:0] AddCarry = 4'b0011;
    localparam logic[3:0] Add1S    = 4'b0100; // A + ~B + 0  (A - B - 1)
    localparam logic[3:0] Sub      = 4'b0101; // A + ~B + 1  (A - B)

    // Boolean family decoded by [3:1]
    localparam logic[2:0] AndOp = 3'b100;
    localparam logic[2:0] OrOp  = 3'b101;
    localparam logic[2:0] XorOp = 3'b110;
    localparam logic[2:0] NotOp = 3'b111;

    // ────────────────────────────────────────────
    // INTERNAL SIGNALS
    // ────────────────────────────────────────────
    // Arithmetic extension by +1 bit for carry/borrow capture
    // WIDTH+1 to hold possible carry out
    logic [WIDTH:0] A1, B1, CIN1, result_ext;
    // Assign extended versions of A, B, and CIN
    assign A1   = {1'b0, A};
    assign B1   = {1'b0, B};
    assign CIN1 = {{WIDTH{1'b0}}, CIN};

    // Combine select lines + CIN into single ALU selector (per Mano Fig. 7-10)
    logic [3:0] ALU_SEL;
    assign ALU_SEL = {S_ALU, CIN};

    // Helper for ones-complement of B in extended width
    logic [WIDTH:0] B1_INV;           // 17-bit, MSB=0, lower bits =~B
    assign B1_INV = {1'b0, ~B};

    // ────────────────────────────────────────────
    // CORE ALU COMBINATIONAL LOGIC
    // ────────────────────────────────────────────
    always_comb begin
        // Initialize result_ext to unknown (prevents latch inference)
        result_ext = 'x;

        unique case (1'b1)
            // Transfers
            (ALU_SEL == TranA1)            : result_ext = A1;
            (ALU_SEL == TranA2)            : result_ext = A1;

            // Increment / Decrement
            // INC must be +1 (independent of external CIN), DEC is -1.
            (ALU_SEL == IncA)              : result_ext = A1 + {{WIDTH{1'b0}}, 1'b1};
            (ALU_SEL == DecA)              : result_ext = A1 - {{WIDTH{1'b0}}, 1'b1};

            // Add family
            (ALU_SEL == Add)               : result_ext = A1 + B1;
            (ALU_SEL == AddCarry)          : result_ext = A1 + B1 + CIN1;
            (ALU_SEL == Add1S)             : result_ext = A1 + B1_INV + {{WIDTH{1'b0}}, 1'b0};

            // Subtract (true A - B path)
            (ALU_SEL == Sub)               : result_ext = A1 + B1_INV + {{WIDTH{1'b0}}, 1'b1};

            // Boolean ops (pad MSB with 0 → carry slot defined; widths match)
            (ALU_SEL[3:1] == AndOp)        : result_ext = {1'b0, (A & B)};
            (ALU_SEL[3:1] == OrOp)         : result_ext = {1'b0, (A | B)};
            (ALU_SEL[3:1] == XorOp)        : result_ext = {1'b0, (A ^ B)};
            (ALU_SEL[3:1] == NotOp)        : result_ext = {1'b0, ~A};

            default                        : result_ext = 'x; // Should never happen
        endcase
    end

    // ────────────────────────────────────────────
    // OUTPUT & FLAG LOGIC
    // ────────────────────────────────────────────
    logic C_temp;
    assign {C_temp, G} = result_ext;

    // Borrow convention preserved (SUB flips to borrow)
    assign C = C_temp;

    // Overflow computation per opcode (V flag)
    logic sa = A[WIDTH-1], sb = B[WIDTH-1], sg = G[WIDTH-1];
    logic V_next;

    always_comb begin
        unique case (1'b1)
            (ALU_SEL == IncA)              : V_next = (~sa) &  sg;               // 0→1 at MSB
            (ALU_SEL == DecA)              : V_next =  sa   & ~sg;               // 1→0 at MSB
            (ALU_SEL == Add),
            (ALU_SEL == AddCarry)          : V_next = (sa == sb)  && (sg != sa); // add overflow
            (ALU_SEL == Add1S)             : V_next = (sa == ~sb) && (sg != sa); // A + ~B (1's)
            (ALU_SEL == Sub)               : V_next = (sa != sb)  && (sg != sa); // sub overflow
            default                        : V_next = 1'b0;                      // logic ops
                                                                                 // & tranfers
        endcase
    end

    assign N = G[WIDTH-1];
    assign V = V_next;
    assign Z = ~|G;

endmodule
`default_nettype wire
