# ============================================================
# Makefile for Function_Unit verification
# Works natively on Linux, in Docker, on Raspberry Pi, etc.
# No Cygwin dependency.
# ============================================================

VERILATOR    ?= verilator
GTKWAVE      ?= gtkwave
TOP_MODULE   := Function_Unit_TB
VCD_FILE     := Function_Unit_TB.vcd
OUT_BIN      := obj_dir/func_tb.out

# All SV sources in dependency order (package first)
SV_SRCS      := Function_Unit_pkg.sv ALU.sv Shifter.sv Function_Unit.sv Function_Unit_TB.sv
CPP_SRCS     := sim_main.cpp

VFLAGS       := -Wall --timing --cc --exe --trace \
                -o func_tb.out \
                --top-module $(TOP_MODULE) \
                -CFLAGS "-std=gnu++17"

# ── Targets ─────────────────────────────────────────────────

.PHONY: all build run waves lint clean help

all: run                  ## Build + run simulation (default)

build: $(OUT_BIN)         ## Verilate + compile C++ (incremental)

$(OUT_BIN): $(SV_SRCS) $(CPP_SRCS)
	$(VERILATOR) $(VFLAGS) $(SV_SRCS) $(CPP_SRCS)
	$(MAKE) -C obj_dir -f V$(TOP_MODULE).mk -j$$(nproc)

run: build                ## Run simulation, produce VCD
	./$(OUT_BIN) +dump
	@echo ""
	@echo "── VCD written to $(VCD_FILE) ──"

waves: run                ## Run sim then open GTKWave
	$(GTKWAVE) $(VCD_FILE) Function_Unit_TB.gtkw &

lint:                     ## Lint-only (no compile)
	$(VERILATOR) -Wall --timing --lint-only $(SV_SRCS)

clean:                    ## Remove all build artifacts
	rm -rf obj_dir $(VCD_FILE)

help:                     ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | \
	  awk 'BEGIN {FS = ":.*## "}; {printf "  %-12s %s\n", $$1, $$2}'

waves-overview: run
	$(GTKWAVE) $(VCD_FILE) overview.gtkw &

waves-alu: run
	$(GTKWAVE) $(VCD_FILE) debug_alu.gtkw &

waves-shifter: run
	$(GTKWAVE) $(VCD_FILE) debug_shifter.gtkw &

waves-flags: run
	$(GTKWAVE) $(VCD_FILE) debug_flags.gtkw &
