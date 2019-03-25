[![Build Status](https://travis-ci.org/Oric4ever/lcc65.svg?branch=master)](https://travis-ci.org/Oric4ever/lcc65)

# lcc65

C Cross-Compiler

Here is the primary source for the C Cross-Compiler targetting the 6502 processor that is included in the OSDK (Oric Software Development Kit). I am (Fabrice) putting it here so that it eventually becomes more familiar to people willing to maintain it in the long term...

Also, in order to increase its maintenability, I will try to progressively document what my 6502 code generator does. If you are interested in it, the first thing to read is Hanson & Fraser's interfacing document (docs/INTERFAC.pdf): the lcc65 compiler is built from Hanson & Fraser's frontend, and my small 6502 backend (6502/src/gen.c).

The frontend supplies the backend with forest of dags (directed acyclic graphs), the backend is responsible for code generation. The following options are accepted by the backend and affect the generated output; comparison of these outputs give a better understanding of what the backend does...

* -G generates a graph output (in Graphviz dot format) instead of code output. The output is a representation of the forests of dags produced by the frontend, which means that the backend doesn't do any work (except transformation of Hanson & Fraser's internal structures to Graphviz dot format). For example, the visual representation of the compilation of tests/torture_test.c file gives the graph (tests/torture_test.dot), which once converted to pdf ("dot -Tpdf torture_test.dot") can be graphically seen in tests/torture_test.pdf.

* -O0 transforms the graph into code. More precisely, for each function, the backend first linearizes the forest of dags given by the frontend, allocates temporary variables to hold the result of operators, defines a number of "operand handling modes" (Constants, Direct, ZeroPage, Indirect, Y-Indexed) that will be used for optimizing the code in the advanced -O2 and -O3 optimizations, and generates 16-bit macro-instructions instead of 6502 opcodes in order to keep the generator small.

* -O1 removes the leaf nodes of the graph (ADDRG, ADDRF, ADDRL, CNST) so that their value become direct operands of the nodes above them. At the same time, this removes the need for the associated temporary variables.

* -O2 allocates some parameters and some local variables into "virtual-registers" in zero-page: these zero-page variables are of course faster than the normal parameters or local variables which are accessed on a software stack (i.e using indirect Y-indexed access). -O2 also does some simple optimizations like converting additions/substractions with 1 to INC/DEC operations, special treatment of 1-bit shifts or equality with 0, removal of some un-necessary conversions between chars/ints/unsigned ints, removal of function prologue/epilogue when possible.

* -O3 is an attempt for more agressive optimizations, trying to remove some indirection nodes (ASGN, INDIR) of usual code patterns so that addressing mode of the 16-bit macros integrates the additional indirection.
