`timescale 1ns/1ps
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
//                  shift right and the shifts are 1 bit an operation.The test bench runs a 16 shift operations - 4 ops pass,
//                  4 ops shift left, and 8 ops shift right, 8 arithmetic operations, and 4 logic operations. The 12 ALU operations test
//                  cases to set overflow, negative, zero, and carry flags based on large 16 bit register values to trigger these
//                  flags properly.
//
// Dependencies:
//
// Revision:        Revision 0.01 - File Created, bulk of Verilog code written in 2020
//                  Revision 0.02 - Ported to SystemVerilog, updated based on professor feedback on ALU design, and added comments
//                  Revision 0.03 - Updated flag logic to fix V flag computation
//                  Revision 0.04 - Updated ADD_V and SUB_V assignments to fix V flag computation
//                  Revision 0.05 - Updated Function_Unit to use direct assign for MUX F output
//                  Revision 0.06 - Fixed carry flag logic to properly reflect borrow convention
// Additional Comments: Revision 0.01 made with help from Gerald Barnett, Cody Cartier-Solomon, and Rice Rodriguez.
//////////////////////////////////////////////////////////////////////////////////


module Shifter #(
  parameter int unsigned WIDTH = 16
)(
  input  logic [1:0]       S_SHF,
  input  logic [WIDTH-1:0] B,
  output logic [WIDTH-1:0] H,
  // NEW:
  output logic             V_s, C_s, N_s, Z_s
);

  localparam logic [1:0] ShiftL = 2'b01;
  localparam logic [1:0] ShiftR = 2'b10;

  always_comb begin
    unique case (S_SHF)
      2'b00  : H = B;         // pass-through
      ShiftL : H = B << 1;    // logical left shift
      ShiftR : H = B >> 1;    // logical right shift
      default: H = 'x;
    endcase
  end

  // Flags for shifter (match TB expectations):
  // V is 0 for shifts; N = MSB(H); Z = (H==0)
  // TB expects C to be a POST-SHIFT edge bit:
  //   - for ShiftL: C = H[WIDTH-1]
  //   - for ShiftR: C = H[0]
  //   - for pass-through: C = 0
  assign V_s = 1'b0;
  assign C_s = (S_SHF == ShiftL) ? H[WIDTH-1] :
               (S_SHF == ShiftR) ? H[0]       : 1'b0;
  assign N_s = H[WIDTH-1];
  assign Z_s = ~|H;

endmodule
