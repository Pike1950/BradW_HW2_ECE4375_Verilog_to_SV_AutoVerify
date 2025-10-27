`timescale 1ns / 1ps
`default_nettype none
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
//////////////////////////////////////////////////////////////////////////////////

module Function_Unit #(
    parameter int unsigned WIDTH = 16
)
    (
    // Control per Figs. 7-9/7-10/7-16
    input  logic [2:0]       S_ALU,   // {S2,S1,S0} to ALU
    input  logic [1:0]       S_SHF,   // {H1,H0}   to Shifter
    input  logic             MF_SEL,  // separate mux select for F (NOT tied to S2)
    // Datapath
    input  logic [WIDTH-1:0] A,
    input  logic [WIDTH-1:0] B,
    input  logic             CIN,
    // Flags + Output
    output logic             V, C, N, Z,
    output logic [WIDTH-1:0] F
    );

  // ALU
  logic [WIDTH-1:0] G;
  logic V_a, C_a, N_a, Z_a;
  ALU #(.WIDTH(WIDTH)) u_alu (
    .S_ALU (S_ALU), .A(A), .B(B), .CIN(CIN),
    .V(V_a), .C(C_a), .N(N_a), .Z(Z_a), .G(G)
  );

  // Shifter
  logic [WIDTH-1:0] H;
  logic V_s, C_s, N_s, Z_s;
  Shifter #(.WIDTH(WIDTH)) u_shf (
    .S_SHF(S_SHF), .B(B), .H(H),
    .V_s(V_s), .C_s(C_s), .N_s(N_s), .Z_s(Z_s)
  );

  // Output mux
  assign F           = (MF_SEL) ? H : G;
  assign {V,C,N,Z}   = (MF_SEL) ? {V_s, C_s, N_s, Z_s}
                                : {V_a, C_a, N_a, Z_a};

endmodule

`default_nettype wire
