# Function Unit - 16-Bit ALU and Shifter in SystemVerilog

A parameterized 16-bit Function Unit based on the datapath described in Mano and Kime's *Computer System Architecture* (Chapter 7, Figures 7-9, 7-10, 7-16). Originally written in Verilog for ECE 4375 at Texas Tech University, then refactored to SystemVerilog with a self-checking testbench, golden reference model, and automated verification via Verilator and GTKWave.

## Architecture

The Function Unit consists of three components:

```
        A [15:0]    B [15:0]    CIN
           |           |         |
     +-----+-----------+---------+-------+
     |     |           |         |       |
     |  +--v-----------v---------v--+    |
     |  |           ALU             |    |
     |  |   8 arithmetic + 4 logic  |    |
     |  +--------+------------------+    |
     |           |G                      |
     |  +--------v--+    +-----------+   |
     |  |           |    |  Shifter  |   |
     |  |           |    |  B >> B<< |   |
     |  |           |    +-----+-----+   |
     |  |           |          |H        |
     |  |    +------v----------v---+     |
     |  |    |     MUX (MF_SEL)    |     |
     |  |    +----------+----------+     |
     |  |               |F               |
     +--+---+-----------+--+----+--------+
            |              |    |    |
          V,C,N,Z        F[15:0]
```

The ALU takes two 16-bit operands (A and B), a carry-in (CIN), and a 3-bit select (S_ALU). It produces a 16-bit result (G) and four status flags. The Shifter takes the B bus and a 2-bit select (S_SHF), producing a shifted result (H). A multiplexer controlled by MF_SEL selects between G and H for the final output F, and also selects the corresponding flag set.

## Opcode Encoding

The ALU uses a 4-bit internal selector formed by concatenating the control inputs:

```
ALU_SEL = {S_ALU[2:0], CIN}
```

This encoding follows Table 7-10 in Mano/Kime. The upper 3 bits select the operation family, and CIN serves as the least significant bit for arithmetic operations.

### Arithmetic Operations (S_ALU[2] = 0)

| ALU_SEL | S_ALU | CIN | Operation | Description |
|---------|-------|-----|-----------|-------------|
| 0000 | 000 | 0 | Transfer A | G = A |
| 0001 | 000 | 1 | Increment A | G = A + 1 |
| 0010 | 001 | 0 | Add | G = A + B |
| 0011 | 001 | 1 | Add with Carry | G = A + B + 1 |
| 0100 | 010 | 0 | Add Ones Comp | G = A + ~B |
| 0101 | 010 | 1 | Subtract | G = A + ~B + 1 (= A - B) |
| 0110 | 011 | 0 | Decrement A | G = A - 1 |
| 0111 | 011 | 1 | Transfer A | G = A |

Subtraction uses the two's complement method: `A + ~B + 1`. The Add Ones Complement operation (`A + ~B + 0`) computes `A - B - 1`, which is useful in multi-precision arithmetic.

### Logic Operations (S_ALU[2] = 1)

| S_ALU[2:0] | Operation | Description |
|------------|-----------|-------------|
| 100 | AND | G = A & B |
| 101 | OR | G = A \| B |
| 110 | XOR | G = A ^ B |
| 111 | NOT | G = ~A |

For logic operations, CIN is a don't-care. The ALU decodes these by comparing `ALU_SEL[3:1]` against 3-bit constants.

### Shifter Operations

| S_SHF | Operation | Description |
|-------|-----------|-------------|
| 00 | Pass-through | H = B |
| 01 | Shift Left | H = B << 1 (logical, zero fill) |
| 10 | Shift Right | H = B >> 1 (logical, zero fill) |
| 11 | Arithmetic Right | H = B >>> 1 (sign-extending) |

The Shifter operates on Bus B. MF_SEL selects whether F comes from the ALU (MF_SEL = 0) or the Shifter (MF_SEL = 1). The corresponding flag set is muxed alongside the data.

## Status Flags

Four condition flags are produced:

| Flag | Name | Definition |
|------|------|------------|
| V | Overflow | Signed overflow detected (result sign incorrect) |
| C | Carry | Carry-out from bit 16 of the extended arithmetic result |
| N | Negative | MSB of the result (G[15]) |
| Z | Zero | Result is all zeros (~\|G) |

### Overflow (V) Rules

Overflow detection is per-opcode because different operations have different overflow conditions:

- **Add, Add with Carry**: V = 1 when both operands have the same sign but the result has a different sign. Formally: `(A[15] == B[15]) && (G[15] != A[15])`.
- **Subtract**: V = 1 when operands have different signs and the result sign differs from A's sign. Formally: `(A[15] != B[15]) && (G[15] != A[15])`.
- **Add Ones Comp (A + ~B)**: Same as Add, but the effective second operand's sign is `~B[15]`.
- **Increment**: V = 1 only when A = 0x7FFF (max positive wraps to negative). Formally: `(~A[15]) & G[15]`.
- **Decrement**: V = 1 only when A = 0x8000 (min negative wraps to positive). Formally: `A[15] & (~G[15])`.
- **Transfers, Logic ops**: V = 0 always. These operations cannot produce signed overflow.

### Carry (C) Convention

The carry flag is the raw carry-out from the 17-bit extended computation. For the subtraction path (`A + ~B + 1`), this means:

- C = 1 when A >= B (no borrow)
- C = 0 when A < B (borrow occurred)

This is the natural two's complement carry convention, where carry-out = 1 indicates no borrow.

## Width Discipline

All arithmetic is performed at WIDTH+1 bits (17 bits for the default 16-bit configuration) to cleanly capture the carry/borrow bit without truncation or implicit widening. The operands are zero-extended before any operation:

```systemverilog
logic [WIDTH:0] A1   = {1'b0, A};       // 17-bit extended A
logic [WIDTH:0] B1   = {1'b0, B};       // 17-bit extended B
logic [WIDTH:0] B1_INV = {1'b0, ~B};    // 17-bit ones complement of B
```

The result is then split:

```systemverilog
assign {C_temp, G} = result_ext;         // bit 16 = carry, bits 15:0 = result
```

This approach eliminates all Verilator width truncation/expansion warnings and ensures every branch of the ALU case statement produces a consistently sized result.

## Verification

### Approach

The testbench (`Function_Unit_TB.sv`) is fully self-checking. It uses a golden reference model defined in `Function_Unit_pkg.sv` that independently computes the expected result and flags for every operation. Each test case:

1. Drives the DUT inputs
2. Waits for a clock edge (combinational settling)
3. Calls the golden model with the same inputs
4. Compares DUT outputs against the golden model
5. Reports PASS or FAIL with full signal values

The golden model (`model_alu` and `model_shf` functions) mirrors the RTL's encoding and arithmetic but is written in a structurally different style (explicit temporary variables, no concatenation tricks) to avoid common-mode errors between the model and the DUT.

### Test Phases

The testbench runs 39 test cases across three phases:

**Phase 1: Shift Tests (4 tests)**

Tests all four shifter modes with patterns chosen to exercise the MSB and LSB boundaries:

| Test | Input | Operation | Verifies |
|------|-------|-----------|----------|
| SHF PASS | 0xF00F | Pass-through | Data integrity |
| SHL | 0xEFFF | Shift left | MSB shifted out, LSB = 0 |
| SHR | 0xF06F | Shift right | LSB shifted out, MSB = 0 |
| SRA | 0xF87F | Arithmetic right | MSB preserved (sign extension) |

**Phase 2: Arithmetic Tests (23 tests)**

Covers all eight arithmetic opcodes with emphasis on flag-setting corner cases:

Add family (5 tests):
- Zero + Zero (baseline, Z flag)
- Add with carry (0 + 0 + 1)
- Carry-out: 0xFFFF + 1 (C = 1, result wraps to 0)
- Positive overflow: 0x7FFF + 1 (V = 1, positive wraps to negative)
- Negative overflow: 0x8000 + 0x8000 (V = 1, negative wraps to positive)

Subtract family (6 tests):
- Equal operands: 0 - 0 (Z = 1, C = 1 indicating no borrow)
- Borrow case: 0 - 1 (C = 0 indicating borrow)
- Signed boundary: 0x8000 - 1 (V = 1, negative overflows to positive)
- Cross-sign overflow: 0x7FFF - 0xFFFF (V = 1)
- Cross-sign overflow: 0x8000 - 0x7FFF (V = 1)
- Alternating bits: 0xAAAA - 0x5555

Add Ones Complement (3 tests):
- A + ~B with various operands to verify the ones-complement-without-carry path

Increment and Decrement (6 tests):
- INC from zero (basic operation)
- INC wraparound: 0xFFFF + 1 = 0x0000 (C = 1)
- INC overflow: 0x7FFF + 1 = 0x8000 (V = 1, the grader's specific concern)
- DEC from zero (wraps to 0xFFFF)
- DEC to zero: 0x0001 - 1 = 0x0000 (Z = 1)
- DEC overflow: 0x8000 - 1 = 0x7FFF (V = 1, the grader's specific concern)

Transfer (3 tests):
- Transfer zero (Z = 1)
- Transfer positive value
- Transfer negative value (N = 1)

**Phase 3: Logic Tests (16 tests)**

Covers all four logic operations with patterns designed to verify bitwise correctness and flag behavior:

- Zeros and ones: AND/OR/XOR with 0x0000 and 0xFFFF to verify identity and annihilation properties
- Alternating bits: 0xAAAA and 0x5555 to catch bit-lane errors
- MSB/LSB isolation: single-bit patterns to verify no bit-shifting or masking errors
- Disjoint sets: 0x0F0F and 0xF0F0 to verify AND produces zero and OR produces all-ones
- NOT: four cases covering 0x0000, 0xFFFF, 0x8000, 0x7FFF to verify inversion at both boundaries
- All logic tests verify V = 0 and C = 0 (logic operations never set overflow or carry)

### Corner Cases Specifically Targeted

These tests address feedback from the original code review:

- **INC/DEC overflow is independent of B**: The overflow flag for increment and decrement depends only on A's sign bit and G's sign bit. B is not involved. Tests verify V = 1 at the exact boundary values (0x7FFF for INC, 0x8000 for DEC) regardless of what B contains.
- **Subtraction borrow convention**: Equal operands (A - A) must produce C = 1 (no borrow), not C = 0. The test `SUB Case 0` (0 - 0) verifies C = 1, Z = 1.
- **Logic operations never set V**: All 16 logic tests confirm V = 0 for every combination.

## File Structure

```
.
├── ALU.sv                    # 16-bit ALU (8 arithmetic + 4 logic operations)
├── Shifter.sv                # 1-bit shift unit (pass, SHL, SHR, SRA)
├── Function_Unit.sv          # Top-level: ALU + Shifter + output mux
├── Function_Unit_pkg.sv      # Shared types, opcodes, golden reference models
├── Function_Unit_TB.sv       # Self-checking testbench (39 test cases)
├── sim_main.cpp              # Verilator C++ harness
├── Makefile                  # Build, simulate, lint, view waveforms
├── overview.gtkw             # GTKWave save: full signal overview
├── debug_alu.gtkw            # GTKWave save: ALU-focused debug view
├── debug_shifter.gtkw        # GTKWave save: Shifter-focused debug view
├── debug_flags.gtkw          # GTKWave save: flags verification view
└── .vscode/
    ├── tasks.json            # VSCode build/run/lint tasks via Make
    └── c_cpp_properties.json # C++ IntelliSense for Verilator output
```

## Building and Running

Requires Verilator, g++, and Make. GTKWave is optional (for waveform viewing).

```bash
# Install dependencies (Ubuntu/Debian/WSL2/Raspberry Pi)
sudo apt install -y verilator gtkwave build-essential

# Lint check (fast, no compile)
make lint

# Build and run simulation
make run

# View waveforms (multiple pre-configured views)
make waves-overview     # All signals, compact
make waves-alu          # ALU internals, analog traces
make waves-shifter      # Shifter-focused
make waves-flags        # Flags verification, tall traces

# Clean build artifacts
make clean

# Show all available targets
make help
```

Expected output from `make run`:

```
     FUNCTION UNIT TESTBENCH - BORROW & OVERFLOW CHECK
----------------------------------------------------------

[ 0 ] --- PHASE 1: SHIFT TESTS ---
[10000] PASS: SHF PASS | F=61455 V=0 C=0 N=1 Z=0
[50000] PASS: SHL | F=57342 V=0 C=0 N=1 Z=0
[90000] PASS: SHR | F=30775 V=0 C=0 N=0 Z=0
[130000] PASS: SRA | F=64575 V=0 C=0 N=1 Z=0

... (39 total, all PASS) ...

[ 1790000 ] --- TESTBENCH COMPLETE ---
```

## Design Decisions

**SystemVerilog conversion**: The original Verilog design used `reg`/`wire` types, `always @(*)`, and positional port connections. The SystemVerilog version uses `logic` throughout, `always_comb` for combinational blocks, `unique case` for latch-free decoding, named port connections, `default_nettype none` for implicit wire prevention, and parameterized widths.

**Complement-add for subtraction**: Subtraction is implemented as `A + ~B + 1` rather than `A - B`. This matches the textbook datapath (which has a B-input inverter and a carry-in adder) and produces the correct carry-out polarity for the borrow convention without any post-processing on the carry flag.

**Per-opcode overflow**: Rather than a single overflow formula applied to all operations, V is computed with a dedicated case statement that applies the correct rule for each opcode. This prevents false positives on logic operations and handles the INC/DEC boundary cases without referencing the B operand.

**Golden model in a package**: The reference model lives in `Function_Unit_pkg.sv` as pure functions that take explicit inputs and return a struct of expected outputs. This keeps the model completely decoupled from DUT signals, making it impossible for the model to accidentally read from the DUT and trivially pass.

## References

- Mano, M.M. and Kime, C.R., *Logic and Computer Design Fundamentals*, Chapter 7
- Figures 7-9 (Function Unit block diagram), 7-10 (ALU operation table), 7-16 (Shifter)
- Table 7-10 (ALU function select encoding)

## Author

Bradley Ward
- Original Verilog (2020) with Gerald Barnett, Cody Cartier-Solomon, and Rice Rodriguez
- SystemVerilog refactor, automated verification, and toolchain migration (2025-2026)
