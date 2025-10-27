#include "VFunction_Unit_TB.h"
#include "verilated.h"

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    // IMPORTANT: allow $dumpfile/$dumpvars to work
    Verilated::traceEverOn(true);    // <-- must be called before time advances

    vluint64_t main_time = 0;
    Verilated::timeInc(0);           // start at time 0

    auto* top = new VFunction_Unit_TB;

    while (!Verilated::gotFinish()) {
        top->eval();
        ++main_time;
        Verilated::timeInc(1);       // advance 1 time unit per loop
    }

    delete top;
    return 0;
}
