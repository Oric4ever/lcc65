# lcc65

C Cross-Compiler

Here is the primary source for the C Cross-Compiler targetting the 6502 processor that is included in the OSDK (Oric Software Development Kit). I am (Fabrice) putting it here so that it eventually becomes more familiar to people willing to maintain it in the long term...

Also, in order to increase its maintenability, I will try to progressively document what my 6502 code generator does. If you are interested in it, the first thing to read is Hanson & Fraser's interfacing document (docs/INTERFAC.pdf): the lcc65 compiler is built from Hanson & Fraser's frontend, and my small 6502 backend (6502/src/gen.c).

The frontend supplies the backend with forest of dags (directed acyclic graphs), the backend is responsible for code generation. I've just added a -G option to the compiler: this option generates a graph output (in Graphviz dot format) of the forests of dags given by the frontend, so that it becomes easier to explain what the backend does...

* -G does not output code, it produces a text representation of the output of Hanson & Fraser lcc frontend, that you can feed to Graphviz dot tool. For example, the visual representation of the compilation of tests/torture_test.c file gives the graph (tests/torture_test.dot), which once converted to pdf ("dot -Tpdf torture_test.dot") can be graphically seen in tests/torture_test.pdf.

* -O0 transforms the graph into code. More precisely, for each function, the backend first linearizes the forest of dags given by the frontend, allocates temporary variables to hold the result of operators, defines a number of "adressing modes" that will be used for optimizing the code in the advanced -On options, and generates 16-bit macro-instructions instead of 6502 opcodes in order to keep the generator small.

* -O1 removes the leaf nodes of the graph (ADDRG, ADDRF, ADDRL, CNST) so that their value become direct operands of the nodes above them. At the same time, this removes the need for the associated temporary variables.

* -O2 allocates some parameters and some local variables into "virtual-registers" in zero-page: these zero-page variables are of course faster than the normal parameters or local variables which are accessed on a software stack (i.e using indirect Y-indexed access). -O2 also does some simple optimizations like converting additions/substractions with 1 to INC/DEC operations, special treatment of 1-bit shifts or equality with 0, removal of some un-necessary conversions between chars/ints/unsigned ints, removal of function prologue/epilogue when possible.

* -O3 is an attempt for more agressive optimizations, trying to remove some indirection nodes (ASGN, INDIR) of usual code patterns so that addressing mode of the 16-bit macros integrates the additional indirection.
